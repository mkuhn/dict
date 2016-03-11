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

typedef std::unordered_map<double, SEXP> Double_map;
typedef std::unordered_map<Double_vector, SEXP, container_hash<Double_vector> > Double_vector_map;


class Dict {
  private:

    Double_vector_map double_vector_map;
    Double_map double_map;

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
            Double_vector dv(nv.begin(), nv.end());
            Double_vector_map::const_iterator it = double_vector_map.find(dv);
            if (it != double_vector_map.end())
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
          Rcpp::NumericVector nv(key);
          if (nv.size() == 1) {
            double_map[nv.at(0)] = value;
          } else {
            Double_vector dv(nv.begin(), nv.end());
            double_vector_map[dv] = value;
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
