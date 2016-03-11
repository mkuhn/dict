// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

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

typedef std::unordered_map<double, SEXP> Double_map;
typedef std::unordered_map<std::string, SEXP> String_map;
typedef std::unordered_map<Double_vector, SEXP, container_hash<Double_vector> > Double_vector_map;
typedef std::unordered_map<String_vector, SEXP, container_hash<String_vector> > String_vector_map;

class Dict {
  private:

    Double_vector_map double_vector_map;
    Double_map double_map;

    String_vector_map string_vector_map;
    String_map string_map;

public:

    SEXP get(SEXP& key) {

      switch( TYPEOF(key) ) {
        case REALSXP: {
          Rcpp::NumericVector nv(key);
          if (nv.size() == 1) {
            Double_map::const_iterator it = double_map.find(nv.at(0));
            if (it != double_map.end())
              return it->second;
          } else {
            Double_vector v(nv.begin(), nv.end());
            Double_vector_map::const_iterator it = double_vector_map.find(v);
            if (it != double_vector_map.end())
              return it->second;
          }
          break;
        }

        case STRSXP: {
          Rcpp::StringVector sv(key);
          if (sv.size() == 1) {
            String_map::const_iterator it = string_map.find(Rcpp::as<std::string>(sv.at(0)));
            if (it != string_map.end())
              return it->second;
          } else {
            String_vector v(sv.begin(), sv.end());
            String_vector_map::const_iterator it = string_vector_map.find(v);
            if (it != string_vector_map.end())
              return it->second;
          }
          break;
        }

        default:
          Rcpp::stop("incompatible SEXP encountered");
      }

      return Rcpp::wrap(NA_LOGICAL);
    }

    void set(SEXP& key, SEXP& value) {

      switch( TYPEOF(key) ) {

        case REALSXP: {
          Rcpp::NumericVector v(key);
          if (v.size() == 1) {
            double_map[v.at(0)] = value;
          } else {
            Double_vector dv(v.begin(), v.end());
            double_vector_map[dv] = value;
          }

          break;
        }

        case STRSXP: {
          Rcpp::StringVector v(key);
          if (v.size() == 1) {
            string_map[Rcpp::as<std::string>(v.at(0))] = value;
          } else {
            String_vector sv(v.begin(), v.end());
            string_vector_map[sv] = value;
          }

          break;
        }

        default:
          Rcpp::stop("incompatible SEXP encountered");
      }
    }

};

RCPP_MODULE(dict_module){
    using namespace Rcpp ;

    class_<Dict>("Dict")

    .constructor()

    .method( "get", &Dict::get )
    .method( "set", &Dict::set )

    ;
}
