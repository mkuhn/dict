// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <unordered_map>
#include <boost/functional/hash.hpp>

template <typename Container>
struct container_hash {
  std::size_t operator()(Container const& c) const {
    return boost::hash_range(c.begin(), c.end());
  }
};

typedef std::vector<double> Double_vector;
typedef std::vector<std::string> String_vector;

template<class T>
using Double_map = std::unordered_map<double, T>;

template<class T>
using String_map = std::unordered_map<std::string, T>;

template<class T>
using Double_vector_map = std::unordered_map<Double_vector, T, container_hash<Double_vector> >;

template<class T>
using String_vector_map = std::unordered_map<String_vector, T, container_hash<String_vector> >;


template<class T>
class Dict {
  protected:

    Double_vector_map<T> double_vector_map;
    Double_map<T> double_map;

    String_vector_map<T> string_vector_map;
    String_map<T> string_map;

    const SEXP NA = Rcpp::wrap(NA_LOGICAL);

    T get_item(SEXP& key, const SEXP& default_value, bool stop_if_missing) {

      switch( TYPEOF(key) ) {
        case INTSXP:
          // fall-through, as R can't decide (1:3 is an integer, c(1,2,3) is numeric)
        case REALSXP: {
          Rcpp::NumericVector nv(key);
          if (nv.size() == 1) {
            typename Double_map<T>::const_iterator it = double_map.find(nv.at(0));
            if (it != double_map.end())
              return it->second;
          } else {
            Double_vector v(nv.begin(), nv.end());
            typename Double_vector_map<T>::const_iterator it = double_vector_map.find(v);
            if (it != double_vector_map.end())
              return it->second;
          }
          break;
        }

        case STRSXP: {
          Rcpp::StringVector sv(key);
          if (sv.size() == 1) {
            typename String_map<T>::const_iterator it = string_map.find(Rcpp::as<std::string>(sv.at(0)));
            if (it != string_map.end())
              return it->second;
          } else {
            String_vector v(sv.begin(), sv.end());
            typename String_vector_map<T> ::const_iterator it = string_vector_map.find(v);
            if (it != string_vector_map.end())
              return it->second;
          }
          break;
        }

        default:
          Rcpp::stop("incompatible SEXP encountered");
      }

      if (stop_if_missing) {
        Rcpp::Rcout << "Key not found: ";
        Rcpp::print(key);
        Rcpp::stop("Key error!");
      }

      return default_value;
    }

    inline void set_double(const double& key, T& value) {
      double_map[key] = value;
    }

    inline void set_double_vector(const Double_vector& key, T& value) {
      double_vector_map[key] = value;
    }

    inline void set_string(const std::string& key, T& value) {
      string_map[key] = value;
    }

    inline void set_string_vector(const String_vector& key, T& value) {
      string_vector_map[key] = value;
    }


  public:

    T get_with_default(SEXP& key, SEXP& default_value) {
      return get_item(key, default_value, false);
    }

    T get_or_stop(SEXP& key) {
      return get_item(key, NA, true);
    }

    void set(const SEXP& key, T& value) {

      switch( TYPEOF(key) ) {

        case INTSXP: // fall-through
        case REALSXP: {
          Rcpp::NumericVector v(key);
          if (v.size() == 1) {
            set_double(v.at(0), value);
          } else {
            Double_vector dv(v.begin(), v.end());
            set_double_vector(dv, value);
          }

          break;
        }

        case STRSXP: {
          Rcpp::StringVector v(key);
          if (v.size() == 1) {
            std::string s = Rcpp::as<std::string>(v.at(0));
            set_string(s, value);
          } else {
            String_vector sv(v.begin(), v.end());
            set_string_vector(sv, value);
          }

          break;
        }

        default:
          Rcpp::stop("incompatible SEXP encountered");
      }
    }

    size_t length() {
      return double_vector_map.size() + double_map.size() +
             string_vector_map.size() + string_map.size();
    }

    Rcpp::List keys() {

      std::vector<SEXP> keys;
      keys.reserve(length());

      for (auto kv : double_map)        keys.push_back(Rcpp::wrap(kv.first));
      for (auto kv : double_vector_map) keys.push_back(Rcpp::wrap(kv.first));
      for (auto kv : string_map)        keys.push_back(Rcpp::wrap(kv.first));
      for (auto kv : string_vector_map) keys.push_back(Rcpp::wrap(kv.first));

      return Rcpp::wrap(keys);
    }

    Rcpp::List values() {

      std::vector<SEXP> values;
      values.reserve(length());

      for (auto kv : double_map)        values.push_back(Rcpp::wrap(kv.second));
      for (auto kv : double_vector_map) values.push_back(Rcpp::wrap(kv.second));
      for (auto kv : string_map)        values.push_back(Rcpp::wrap(kv.second));
      for (auto kv : string_vector_map) values.push_back(Rcpp::wrap(kv.second));

      return Rcpp::wrap(values);
    }

    Rcpp::List items() {

      std::vector<Rcpp::List> items;
      items.reserve(length());

      for (auto kv : double_map)
        items.push_back( Rcpp::List::create(
            Rcpp::Named("key") = Rcpp::wrap(kv.first),
            Rcpp::Named("value") = Rcpp::wrap(kv.second)
        ));

      for (auto kv : double_vector_map)
        items.push_back( Rcpp::List::create(
            Rcpp::Named("key") = Rcpp::wrap(kv.first),
            Rcpp::Named("value") = Rcpp::wrap(kv.second)
        ));

      for (auto kv : string_map)
        items.push_back( Rcpp::List::create(
            Rcpp::Named("key") = Rcpp::wrap(kv.first),
            Rcpp::Named("value") = Rcpp::wrap(kv.second)
        ));

      for (auto kv : string_vector_map)
        items.push_back( Rcpp::List::create(
            Rcpp::Named("key") = Rcpp::wrap(kv.first),
            Rcpp::Named("value") = Rcpp::wrap(kv.second)
        ));

      return Rcpp::wrap(items);
    }

};

class NumVecDict : private Dict<Rcpp::NumericVector> {

public:

  Rcpp::NumericVector get_with_default(SEXP& key, SEXP& default_value) {
    return Dict<Rcpp::NumericVector>::get_with_default(key, default_value);
  }

  Rcpp::NumericVector get_or_stop(SEXP& key) {
    return Dict<Rcpp::NumericVector>::get_or_stop(key);
  }

  void set(SEXP& key, Rcpp::NumericVector& value) {
    Dict<Rcpp::NumericVector>::set(key, value);
  }

  void append_number(SEXP& key, double value) {

    switch( TYPEOF(key) ) {
      case INTSXP: // fall-through
      case REALSXP: {
        Rcpp::NumericVector nv(key);
        if (nv.size() == 1) {
          Double_map<Rcpp::NumericVector>::iterator it = double_map.find(nv.at(0));
          if (it != double_map.end()) {
            it->second.push_back(value);
          } else {
            Rcpp::NumericVector vv;
            vv.push_back(value);
            double_map[nv.at(0)] = vv;
          }

        } else {
          Double_vector v(nv.begin(), nv.end());
          Double_vector_map<Rcpp::NumericVector>::iterator it = double_vector_map.find(v);
          if (it != double_vector_map.end()) {
            it->second.push_back(value);
          } else {
            Rcpp::NumericVector vv;
            vv.push_back(value);
            double_vector_map[v] = vv;
          }
        }
        break;
      }
      case STRSXP: {
        Rcpp::StringVector sv(key);
        if (sv.size() == 1) {
          String_map<Rcpp::NumericVector>::iterator it = string_map.find(Rcpp::as<std::string>(sv.at(0)));
          if (it != string_map.end()) {
            it->second.push_back(value);
          } else {
            Rcpp::NumericVector vv;
            vv.push_back(value);
            string_map[Rcpp::as<std::string>(sv.at(0))] = vv;
          }

        } else {
          String_vector v(sv.begin(), sv.end());
          String_vector_map<Rcpp::NumericVector>::iterator it = string_vector_map.find(v);
          if (it != string_vector_map.end()) {
            it->second.push_back(value);
          } else {
            Rcpp::NumericVector vv;
            vv.push_back(value);
            string_vector_map[v] = vv;
          }
        }
        break;
      }

      default:
        Rcpp::stop("incompatible SEXP encountered");
    }
  }

  Rcpp::List keys() {
    return Dict<Rcpp::NumericVector>::keys();
  }

  Rcpp::List values() {
    return Dict<Rcpp::NumericVector>::values();
  }

  Rcpp::List items() {
    return Dict<Rcpp::NumericVector>::items();
  }

  NumVecDict means() {
    NumVecDict result;

    for (auto kv : double_map) {
      Rcpp::NumericVector mv;
      mv.push_back(Rcpp::mean(kv.second));
      result.set_double(kv.first, mv);
    }

    for (auto kv : double_vector_map) {
      Rcpp::NumericVector mv;
      mv.push_back(Rcpp::mean(kv.second));
      result.set_double_vector(kv.first, mv);
    }

    for (auto kv : string_map)        {
      Rcpp::NumericVector mv;
      mv.push_back(Rcpp::mean(kv.second));
      result.set_string(kv.first, mv);
    }

    for (auto kv : string_vector_map) {
      Rcpp::NumericVector mv;
      mv.push_back(Rcpp::mean(kv.second));
      result.set_string_vector(kv.first, mv);
    }

    return result;
  }

};

RCPP_EXPOSED_CLASS(NumVecDict)


RCPP_MODULE(dict_module){
    using namespace Rcpp ;

    class_< Dict<SEXP> >("Dict")

    .constructor()

    .method( "[[", &Dict<SEXP>::get_or_stop )
    .method( "get_with_default", &Dict<SEXP>::get_with_default )
    .method( "[[<-", &Dict<SEXP>::set )
    .method( "set", &Dict<SEXP>::set )
    .method( "keys", &Dict<SEXP>::keys )
    .method( "values", &Dict<SEXP>::values )
    .method( "items", &Dict<SEXP>::items )

    ;

    class_< NumVecDict >("NumVecDict")

    .constructor()

    .method( "[[", &NumVecDict::get_or_stop )
    .method( "get_with_default", &NumVecDict::get_with_default )
    .method( "[[<-", &NumVecDict::set )
    .method( "set", &NumVecDict::set )
    .method( "append_number", &NumVecDict::append_number )
    .method( "keys", &NumVecDict::keys )
    .method( "values", &NumVecDict::values )
    .method( "items", &NumVecDict::items )
    .method( "means", &NumVecDict::means )

    ;

}
