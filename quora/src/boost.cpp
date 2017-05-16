#include <Rcpp.h>
#include <iostream>
#include <string>
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>
#include <vector>

using namespace Rcpp;

std::string lowerCasea(std::string str) {

    int len = str.length();

    for( int i=0; i < len; i++ ) {
        str[i] = tolower (str[i]);
    }

    return str;
}

// [[Rcpp::export]]
std::vector<std::string> lowerCase(std::vector<std::string> strings ) {

    int len = strings.size();

    for( int i=0; i < len; i++ ) {
        strings[i] = lowerCasea(strings[i]);
    }

    return strings;
}


// [[Rcpp::export]]
std::string remove_punctuation(std::string input) {
  boost::regex punct("[[:punct:]]");
  std::string fmt = " ";
  std::string output = boost::regex_replace(input,punct,fmt);
  return output;
}


// [[Rcpp::export]]
SEXP tokenize( SEXP l ) {

    std::vector<std::string> strs;
    const char * line = CHAR(STRING_ELT(l,0));
    boost::split(strs, line, boost::is_space(), boost::token_compress_on);
    CharacterVector output(strs.begin(), strs.end());
    return output;
}





// [[Rcpp::export]]
std::vector<bool> strComp(std::vector<std::string> s1, std::vector<std::string> s2) {

    int len = s1.size();
    std::vector<bool> blout(len);

    for( int i=0; i < len; i++ ) {
        blout[i] = boost::iequals(s1[i], s2[i]);
    }

    return blout;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
v <- tokenize(lowerCase(c("This is a test A test")))
w <- tokenize(lowerCase(c("This is test A test")))
strComp(v, w)
x <- tokenize(lowerCase(c("This is a test A TEST")))
strComp(w, x)
y <- tokenize(lowerCase(c("This is a test testing")))
strComp(x, y)
z <- tokenize(lowerCase(c("This is a test a test")))
strComp(y, z)
*/
