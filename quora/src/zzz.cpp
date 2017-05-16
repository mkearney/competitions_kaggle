// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>

#include <string>
#include <boost/regex.hpp>

std::string lowerCasea(std::string str) {

  int len = str.length();

  for( int i=0; i < len; i++ ) {
    str[i] = tolower (str[i]);
  }

  return str;
}

std::vector<std::string> lowerCase(std::vector<std::string> strings ) {

  int len = strings.size();

  for( int i=0; i < len; i++ ) {
    strings[i] = lowerCasea(strings[i]);
  }

  return strings;
}

