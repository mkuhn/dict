// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include <Rcpp.h>
#include <unordered_map>

// [[Rcpp::plugins(cpp11)]]

typedef std::unordered_map<int, SEXP> Int_map;

class dict {
  private:

    Int_map int_map;

  public:

    SEXP at(int key) {
      return int_map[key];
    }

    void set(int key, SEXP& value) {
      int_map[key] = value;
    }

};

RCPP_MODULE(dict_module){
    using namespace Rcpp ;

    class_<dict>("dict")

    .constructor()

    .method( "[[", &dict::at )
    .method( "[[<-", &dict::set )

    ;
}
