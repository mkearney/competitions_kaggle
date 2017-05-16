##mwk
##quora training model
##kaggle competition data set
##04/25/2017

## install package if not already
##pkgs <- c("dplyr", "gbm", "Metrics", "Rcpp", "readr")
##if (any(!pkgs %in% installed.packages())) {
##    install.packages(pkgs)
##}

## load dplyr andRcpp
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(Rcpp))

## number of trees
ntrees <- 500
## load all training data
ntrain <- Inf

## read in functions
Rcpp::sourceCpp(code = '#include <Rcpp.h>
#include <string>
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>

using namespace Rcpp;

std::string lowerCasea(std::string str) {
    int len = str.length();
    for( int i=0; i < len; i++ ) {
        str[i] = tolower(str[i]);
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
}'
)

Rcpp::sourceCpp(code = '#include <Rcpp.h>
#include <string>
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>

using namespace Rcpp;

std::string lowerCasea(std::string str) {
    int len = str.length();
    for( int i=0; i < len; i++ ) {
        str[i] = tolower(str[i]);
    }
    return str;
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
}'
)

#' make_features
#' @param x Data frame
#' @param ds Name of data set
make_features <- function(x, ds = NULL) {
    .features <- function(x, ds = NULL) {
        x$q1tokens <- splittr(x$question1)
        x$q2tokens <- splittr(x$question2)
        x$q1token_count <- vapply(x$q1tokens, length, double(1))
        x$q2token_count <- vapply(x$q2tokens, length, double(1))
        x$token_count <- mapply(function(a, b) sum(c(a, b), na.rm = TRUE),
                                x$q1token_count, x$q2token_count)
        x$overlap <- mapply(intersect, x$q1tokens, x$q2tokens)
        x$overlap_count <- vapply(x$overlap, length, double(1))
        x$overlap_prop <- x$overlap_count / x$token_count
        corranks <- function(a, b, o) {
            if (length(o) == 0) return(0)
            if (length(o) == 1) return(.5)
            a <- unique(a[a %in% o])
            b <- unique(b[b %in% o])
            b <- factor(b, levels = a)
            a <- factor(a, levels = a)
            cor(as.numeric(a), as.numeric(b), use = "pairwise")
        }
        x$corrs <- mapply(corranks, x$q1tokens, x$q2tokens, x$overlap)
        x$corrs[is.na(x$corrs)] <- 0
        if (!is.null(ds)) {
            x$ds <- ds
        } else {
            x$ds <- NA
        }
        if (!"is_duplicate" %in% names(x)) {
            x$is_duplicate <- NA_integer_
        }
        x
    }
    if (nrow(x) > 1e4) {
        ctr <- 0L
        out <- vector("list", ceiling(nrow(x) / 1e4))
        for (i in seq_len(ceiling(nrow(x) / 1e4))) {
            end <- (ctr + 1e4)
            if (end > nrow(x)) end <- nrow(x)
            out[[i]] <- .features(x[(ctr + 1):end, ], ds = ds)
            ctr <- ctr + 1e4
        }
        do.call("rbind", out)
    } else {
        .features(out, ds = ds)
    }
}

hd <- function(x, n = NULL) {
    if (is.null(n)) n <- nrow(x)
    if ("test_id" %in% names(x)) {
            dplyr::select(x[1:n, ], -question1, -question2, -q1tokens, -q2tokens,
                          -test_id, -overlap)
    } else {
        dplyr::select(x[1:n, ], -question1, -question2, -q1tokens, -q2tokens,
                      -id, -qid1, -qid2, -overlap)
    }
}


#' model
#'
#' @param f Formula to estimate model.
#' @param x Data set.
#' @param ntrees Number of decision trees.
mdl <- function(x, f, ntrees = 3500) {
    gbm::gbm(as.formula(f),
             data = data.frame(x),
             distribution = "bernoulli",
             n.trees = ntrees,
             interaction.depth = 4,
             shrinkage = 0.1,
             n.minobsinnode = 10,
             bag.fraction = 0.5,
             train.fraction = 1.0)
}

#' predictions
#'
#' @param x Data set
#' @param mod GBM model
#' @param ntrees Number of trees
prd <- function(x, mod, ntrees = 3500) {
    predict(mod, newdata = data.frame(x), n.trees = ntrees, type = "response")
}

#' random sampler
#'
#' @param x Data set.
#' @param n Number of rows to return.
rsamp <- function(x, n = 10) x[sample(seq_len(nrow(x)), n), ]



## add features training data or load saved version
if (!"finale.rds" %in% list.files(file.path("..", "data"))) {
    ## load training data
    e <- suppressMessages(
        readr::read_csv("../input/train.csv", n_max = ntrain))
    ## extract features
    e <- make_features(e, 1L)
    ## preview
    hd(e, 5)
    ## save mutated training data
    ##readr::write_rds(e, "../data/finale.rds")
} else {
    ## read mutated training data
    e <- readr::read_rds("../input/finale.rds")
}

## add features test data or load saved version
if (!"finaltd.rds" %in% list.files(file.path("..", "data"))) {
    ## load training data
    td <- read.csv("../data/test.csv")
    ## extract features
    td <- make_features(td, 0L)
    ## preview
    hd(td, 5)
    ## save mutated training data
    ## readr::write_rds(td, "../data/finaltd.rds")
} else {
    ## read mutated test data
    td <- readr::read_rds("../data/finaltd.rds")
}

## combine train and test data
etd <- rbind(hd(e), hd(td))

## predict data set
fds <- "ds ~ 0 + overlap_count * token_count + overlap_prop"

## predict data set
dsmod <- mdl(etd, fds, ntrees)

## view results
summary(dsmod, n.trees = ntrees)

## save predicted probability values
etd$dsp <- prd(etd, dsmod, ntrees)

## model predicting duplicate
fis <- "is_duplicate ~ 0 + overlap_count + token_count + overlap_prop + corrs + dsp"
ismod <- mdl(subset(etd, !is.na(etd$is_duplicate)), fis, ntrees)

## view results
summary(ismod, n.trees = ntrees)

## adjust ntrees
##best.iter <- gbm::gbm.perf(ismod, method = "OOB")
##ntrees <- ceiling(mean(c(best.iter, best.iter, ntrees, ntrees, ntrees)))

## get predictions
etd$pred <- prd(etd, ismod, ntrees)

## calculate log loss
if (require(Metrics)) {
    Metrics::logLoss(etd$is_duplicate[etd$ds == 1],
                     etd$pred[etd$ds == 1])
}

## check out mean estimates
etd %>%
    group_by(ds) %>%
    summarise(pred = mean(pred, na.rm = TRUE),
              dsp = mean(dsp, na.rm = TRUE),
              isdup = mean(is_duplicate, na.rm = TRUE))

## and if the dsp param is adjusted...
etd %>%
    mutate(pred = ifelse(dsp < .95, 0, pred)) %>%
    group_by(ds) %>%
    summarise(pred = mean(pred, na.rm = TRUE),
              dsp = mean(dsp, na.rm = TRUE),
              isdup = mean(is_duplicate, na.rm = TRUE))
