// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <functional>
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

template<class Key, class Hash = std::hash<Key> >
using T_NV_map = std::unordered_map<Key, Rcpp::NumericVector, Hash>;

template<class Key, class Hash = std::hash<Key> >
void append_or_add(T_NV_map<Key, Hash>& nv_map, const Key& key, double value) {
  auto it = nv_map.find(key);
  if (it != nv_map.end()) {
    it->second.push_back(value);
  } else {
    Rcpp::NumericVector vv;
    vv.push_back(value);
    nv_map[key] = vv;
  }
}

template<class Key, class Hash = std::hash<Key> >
void append_or_add(T_NV_map<Key, Hash>& nv_map, const Key& key, Rcpp::NumericVector values) {
  auto it = nv_map.find(key);
  if (it != nv_map.end()) {
    for (double d : values)
      it->second.push_back(d);
  } else {
    nv_map[key] = values;
  }
}


template<class T>
class Dict {
  protected:

    Double_vector_map<T> double_vector_map;
    Double_map<T> double_map;

    String_vector_map<T> string_vector_map;
    String_map<T> string_map;

    const SEXP NA = Rcpp::wrap(NA_LOGICAL);

    T get_item(SEXP& key, const T& default_value, bool store_default) {

      switch( TYPEOF(key) ) {
        case INTSXP:
          // fall-through, as R can't decide (1:3 is an integer, c(1,2,3) is numeric)
        case REALSXP: {
          Rcpp::NumericVector nv(key);
          if (nv.size() == 1) {
            typename Double_map<T>::const_iterator it = double_map.find(nv.at(0));
            if (it != double_map.end())
              return it->second;

            if (store_default) {
              set_double(nv.at(0), default_value);
              return(default_value);
            }

          } else {
            Double_vector v(nv.begin(), nv.end());
            typename Double_vector_map<T>::const_iterator it = double_vector_map.find(v);
            if (it != double_vector_map.end())
              return it->second;

            if (store_default) {
              set_double_vector(v, default_value);
              return(default_value);
            }

          }
          break;
        }

        case STRSXP: {
          Rcpp::StringVector sv(key);
          if (sv.size() == 1) {
            std::string s = Rcpp::as<std::string>(sv.at(0));
            typename String_map<T>::const_iterator it = string_map.find(s);
            if (it != string_map.end())
              return it->second;

            if (store_default) {
              set_string(s, default_value);
              return(default_value);
            }

          } else {
            String_vector v(sv.begin(), sv.end());
            typename String_vector_map<T> ::const_iterator it = string_vector_map.find(v);
            if (it != string_vector_map.end())
              return it->second;

            if (store_default) {
              set_string_vector(v, default_value);
              return(default_value);
            }

          }
          break;
        }

        default:
          Rcpp::stop("incompatible SEXP encountered");
      }

      return default_value;
    }

    inline void set_double(const double& key, const T& value) {
      double_map[key] = value;
    }

    inline void set_double_vector(const Double_vector& key, const T& value) {
      double_vector_map[key] = value;
    }

    inline void set_string(const std::string& key, const T& value) {
      string_map[key] = value;
    }

    inline void set_string_vector(const String_vector& key, const T& value) {
      string_vector_map[key] = value;
    }


  public:

    T get_with_default(SEXP& key, const T& default_value) {
      return get_item(key, default_value, false);
    }

    T get_with_default_or_store(SEXP& key, const T& default_value) {
      return get_item(key, default_value, true);
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

    // get a list of all keys
    Rcpp::List keys() {

      std::vector<SEXP> keys;
      keys.reserve(length());

      for (auto kv : double_map)        keys.push_back(Rcpp::wrap(kv.first));
      for (auto kv : double_vector_map) keys.push_back(Rcpp::wrap(kv.first));
      for (auto kv : string_map)        keys.push_back(Rcpp::wrap(kv.first));
      for (auto kv : string_vector_map) keys.push_back(Rcpp::wrap(kv.first));

      return Rcpp::wrap(keys);
    }

    // get a list of all values
    Rcpp::List values() {

      std::vector<SEXP> values;
      values.reserve(length());

      for (auto kv : double_map)        values.push_back(Rcpp::wrap(kv.second));
      for (auto kv : double_vector_map) values.push_back(Rcpp::wrap(kv.second));
      for (auto kv : string_map)        values.push_back(Rcpp::wrap(kv.second));
      for (auto kv : string_vector_map) values.push_back(Rcpp::wrap(kv.second));

      return Rcpp::wrap(values);
    }

    // get a list of all items [ (key = ..., value = ...), (key = ..., value = ...), ... ]
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



class IdxDict : private Dict<int> {

public:

  int get_or_set_idx(SEXP& key) {
    return Dict<int>::get_with_default_or_store(key, length()+1);
  }

  int get_or_zero(SEXP& key) {
    return Dict<int>::get_with_default(key, 0);
  }

  Rcpp::List keys() {
    return Dict<int>::keys();
  }

  Rcpp::List items() {
    return Dict<int>::items();
  }

};




class NumVecDict : private Dict<Rcpp::NumericVector> {

  Rcpp::NumericVector empty_vector;

public:

  // calls to base class seem to be necessary for RCPP_MODULE
  Rcpp::NumericVector get_with_default(SEXP& key, SEXP& default_value) {
    return Dict<Rcpp::NumericVector>::get_with_default(key, default_value);
  }

  Rcpp::NumericVector get_or_empty_vector(SEXP& key) {
    return Dict<Rcpp::NumericVector>::get_with_default(key, empty_vector);
  }

  void set(SEXP& key, Rcpp::NumericVector& value) {
    Rcpp::NumericVector my_value = Rcpp::clone<Rcpp::NumericVector>(value);
    Dict<Rcpp::NumericVector>::set(key, my_value);
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

  size_t length() {
    return Dict<Rcpp::NumericVector>::length();
  }

  // append a single number to the specified item;
  // creates new entry if necessary
  void append_number(SEXP& key, double value) {

    switch( TYPEOF(key) ) {
      case INTSXP: // fall-through
      case REALSXP: {
        Rcpp::NumericVector nv(key);
        if (nv.size() == 1) {
          append_or_add(double_map, nv.at(0), value);
        } else {
          Double_vector v(nv.begin(), nv.end());
          append_or_add(double_vector_map, v, value);
        }
        break;
      }
      case STRSXP: {
        Rcpp::StringVector sv(key);
        if (sv.size() == 1) {
          append_or_add(string_map, Rcpp::as<std::string>(sv.at(0)), value);
        } else {
          String_vector v(sv.begin(), sv.end());
          append_or_add(string_vector_map, v, value);
        }
        break;
      }

      default:
        Rcpp::stop("incompatible SEXP encountered");
    }
  }

  // merge the given numvecdict into the current one
  void append_items(NumVecDict source) {

    for (auto kv : source.double_map)
      append_or_add(double_map, kv.first, kv.second);

    for (auto kv : source.double_vector_map)
      append_or_add(double_vector_map, kv.first, kv.second);

    for (auto kv : source.string_map)
      append_or_add(string_map, kv.first, kv.second);

    for (auto kv : source.string_vector_map)
      append_or_add(string_vector_map, kv.first, kv.second);
  }

  // return a new numvecdict: (key, mean(values)) for each key
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

    class_< IdxDict >("IdxDict")

    .constructor()

    .method( "get_or_set_idx", &IdxDict::get_or_set_idx )
    .method( "get_or_zero", &IdxDict::get_or_zero )
    .method( "keys", &IdxDict::keys )
    .method( "items", &IdxDict::items )

    ;

    class_< NumVecDict >("NumVecDict")

    .constructor()

    .method( "[[", &NumVecDict::get_or_empty_vector )
    .method( "get_with_default", &NumVecDict::get_with_default )
    .method( "[[<-", &NumVecDict::set )
    .method( "set", &NumVecDict::set )
    .method( "append_number", &NumVecDict::append_number )
    .method( "append_items", &NumVecDict::append_items )
    .method( "keys", &NumVecDict::keys )
    .method( "values", &NumVecDict::values )
    .method( "items", &NumVecDict::items )
    .method( "length", &NumVecDict::length )
    .method( "means", &NumVecDict::means )

    ;

}
