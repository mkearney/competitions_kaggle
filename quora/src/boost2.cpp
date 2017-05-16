#include <Rcpp.h>
#include <string>
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>

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
SEXP tokenize( SEXP l ) {

    std::vector<std::string> strs;
    const char *line = CHAR(STRING_ELT(l,0));
    boost::split(strs, line, boost::is_any_of("\t "),boost::token_compress_on);

    CharacterVector output(strs.begin(),strs.end());
    return output;
}

// [[Rcpp::export]]
std::vector<std::string> splitr( std::string text ) {
    boost::trim_if(text, boost::is_punct());
    std::vector<std::string> results;
    boost::split(results, text, boost::is_any_of("\t "),boost::token_compress_on);
    return results;
}

// [[Rcpp::export]]
Rcpp::List splittr(std::vector<std::string> strings ) {

    int len = strings.size();
    Rcpp::List output(len);

    for( int i=0; i < len; i++ ) {
      strings[i] = lowerCasea(strings[i]);
      output[i] = splitr(strings[i]);
    }

    return output;
}

// [[Rcpp::export]]
LogicalVector testvecs( CharacterVector x, CharacterVector y){
    Rcpp::LogicalVector r(x.size());
    for( int i=0; i<x.size(); i++){
        r[i] = (x[i] == y[i]);
    }
    return(r);
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
