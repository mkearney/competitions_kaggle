#include <Rcpp.h>
#include <vector>
#include <string>
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace Rcpp;

// splitTokens - uses the prefixAtt and suffixAtt regexes to split
// the string. we can end up with the following cases
// str: abcdefg
// case 1: abc ----     : prefix match
// case 2: ---- efg     : sufix match
// case 3: abc - efg    : prefix and suffix with middle left over
// case 4: abcd --- , --- defg  : prefix and suffix but overlapping
// case 5: abc defg     : prefix and suffix touching
// For case 3, we will return prefix, middle, suffix.
std::vector<Token> splitToken( const Token &tok ) {
    std::vector<Token> outtokens;
    boost::smatch what;
    std::string str = tok.text();

    // if the token is mixed [0-9] and [A-Z]
    // then process it here
    if ( tok.isInClass( InClass::MIXED ) ) {
        // split mixed alpha digit tokens, eg:
        // 500W => 500 W or N123 => N 123 or I80 => I 80
        const char* replace( "$1 $2" );
        boost::u32regex re = boost::make_u32regex( std::string( "^(\\d+)([[:alpha:]\\p{L}])$" ) );
        std::string tmp = boost::u32regex_replace( str, re, replace );
        str = tmp;
        re = boost::make_u32regex( std::string( "\\<([[:alpha:]\\p{L}])(\\d+)\\>" ) );
        tmp = boost::u32regex_replace( str, re, replace );
        str = tmp;

        std::vector<std::string> words;
        boost::split(words, str, boost::is_any_of(" "), boost::token_compress_on);
        for ( const auto &e : words ) {
            Token ta( e );
            lex_.classify( ta, InClass::WORD );
            outtokens.push_back( ta );
        }
        return outtokens;
    }

    // a and b are prefix parts of str if matched
    // c and d are suffix parts of str if matched
    std::string a, b, c, d;

    boost::match_flag_type flags = boost::match_default;

    // could not get this to work with auto
    std::string::const_iterator start, end;

    std::string attached = "(" + lex_.regexPrefixAtt() + ")(.+)";
    if (attached.length() > 6) {
        boost::u32regex re = boost::make_u32regex( attached );
        // boost::u32regex_match can throw std::runtime_error if the
        // regex is too complex, we catch and and continue as if
        // we did not match
        try {
            start = str.begin();
            end   = str.end();
            if ( boost::u32regex_match( start, end, what, re, flags ) ) {
                a = std::string(what[1].first, what[1].second);
                b = std::string(what[2].first, what[2].second);
            }
        }
        catch (const std::runtime_error &e ) {
            // we might want to log e.what() while debugging
            return outtokens;
        };
    }

    attached = "(.+)(" + lex_.regexSuffixAtt() + ")";
    if (attached.length() > 6) {
        boost::u32regex re2 = boost::make_u32regex( attached );
        // boost::u32regex_match can throw std::runtime_error if the
        // regex is too complex, we catch and and continue as if
        // we did not match
        try {
            start = str.begin();
            end   = str.end();
            if ( boost::u32regex_match( start, end , what, re2, flags ) ) {
                c = std::string(what[1].first, what[1].second);
                d = std::string(what[2].first, what[2].second);
            }
        }
        catch (const std::runtime_error &e ) {
            // we might want to log e.what() while debugging
            return outtokens;
        };
    }

    // sort out the cases described above

    if ( a.size() > 0 ) {
        // case 1 and 5: prefix only or prefix touches suffix
        if ( d.size() == 0 or b.size() == d.size() ) {
            Token ta( a );
            lex_.classify( ta, InClass::WORD );
            outtokens.push_back( ta );
            Token tb( b );
            lex_.classify( tb, InClass::WORD );
            outtokens.push_back( tb );
        }
        // case 3: not overlapping
        else if ( d.size() > 0 and a.size() + d.size() < str.size() ) {
            Token ta( a );
            lex_.classify( ta, InClass::WORD );
            outtokens.push_back( ta );
            Token middle( str.substr( a.size(), str.size()-a.size()-d.size() ) );
            lex_.classify( middle, InClass::WORD );
            outtokens.push_back( middle );
            Token td( d );
            lex_.classify( td, InClass::WORD );
            outtokens.push_back( td );
        }
        // case 4: overlapping
        else if ( d.size() > 0 and a.size() + d.size() > str.size() ) {
            Token tc( c );
            lex_.classify( tc, InClass::WORD );
            outtokens.push_back( tc );
            Token td( d );
            lex_.classify( td, InClass::WORD );
            outtokens.push_back( td );
        }
    }
    // case 2: suffix only
    else if ( d.size() > 0 ) {
        Token tc( c );
        lex_.classify( tc, InClass::WORD );
        outtokens.push_back( tc );
        Token td( d );
        lex_.classify( td, InClass::WORD );
        outtokens.push_back( td );
    }

    return outtokens;
}

