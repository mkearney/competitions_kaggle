#include <Rcpp.h>

namespace boost{
    std::size_t hash_value(Rcpp::String);
}

#include <boost/unordered_map.hpp>

namespace boost{
    std::size_t hash_value(Rcpp::String obj){
        return hash_value<SEXP>( obj.get_sexp() ) ;
    }
}
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector match3(const CharacterVector x, const CharacterVector table) {
  boost::unordered_map<String, int> lookup;

  int n = table.size();
  for (int i = 0; i < n; ++i) {
    lookup[table[i]] = i;
  }

  int m = x.size();
  IntegerVector out(m);
  for (int i = 0; i < m; ++i) {
    out[i] = lookup[x[i]] + 1;
  }
  return out;
}
