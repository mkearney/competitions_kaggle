
## specify trees
##ntrees <- ntrees
##foo <- function(x) ifelse(is.na(x), 0, x)
##e[, 1:ncol(e)] <- lapply(e, foo)
##foo <- function(x) ifelse(x == 0, 0, log(x))
##e[, 2:ncol(e)] <- lapply(e[, 2:ncol(e)], foo)
ntrees <- 1000
##e <- e %>%
##    dplyr::select(is_duplicate, nmatchpct1, last, sames, wordcor,
##                  r, dfchars)
apply(cor(e, use= "pairwise"), 2, function(x) sort(abs(x), decreasing = TRUE)[1:5])
cor(e, use= "pairwise")[, 1]
