
## cpp funcs
Rcpp::sourceCpp("../src/boost2.cpp")

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
