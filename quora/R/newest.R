

sum(x$is_duplicate == 1)
adjdat <- function(x, p = .17) {
    n <- nrow(x)
    n1 <- n * p
    n0 <- n * (1 - p)
    x1 <- subset(x, is_duplicate == 1)
    x0 <- subset(x, is_duplicate == 0)
    x1 <- x1[sample(1:nrow(x1), n1, replace = TRUE), ]
    x0 <- x0[sample(1:nrow(x0), n0, replace = TRUE), ]
    x <- rbind(x1, x0)
    row.names(x) <- NULL
    x
}
ee <- adjdat(e)

mean(ee$is_duplicate)

td <- readr::read_csv("../data/test.csv")
td$q1stems <- stems(td$question1)
td$q2stems <- stems(td$question2)
totstems <- vapply(td$q1stems, length, double(1))
td$totstems <- totstems + vapply(td$q2stems, length, double(1))
td$dffstems <- abs(totstems - vapply(td$q2stems, length, double(1)))
td$nstem <- mapply(countmatches, td$q1stems, td$q2stems)
td$corrs <- mapply(corrmatches, td$q1stems, td$q2stems)
td$plexs <- mapply(plexmatches, td$q1stems, td$q2stems)
td$plexs[is.nan(td$plexs)] <- 0
td$corrs[is.na(td$corrs)] <- 0
ntrees <- 50
e
mod <- mdl(e, ntrees = ntrees)
##best.iter <- gbm::gbm.perf(mod, method = "OOB")
##best.iter <- ceiling(mean(c(best.iter, best.iter, ntrees, ntrees, ntrees)))

summary(mod, n.trees = ntrees)
td$pred <- predict(mod, newdata = data.frame(td), type = "response", n.trees = ntrees)
td$pred2 <- td$pred
mean(td$pred)
mean(td$pred[td$invalid])
td$pred2[td$invalid] <- 0
mean(td$pred2)

names(td)
x <- rsamp(e, 10000)[, c("question1", "question2", "totstems", "dffstems", "nstem", "corrs", "plexs",
                         "q1nwords", "q2nwords")]
x <- rbind(x, td[sample(seq_len(nrow(td)), 10000), c("question1", "question2", "totstems", "dffstems",
                                                     "nstem", "corrs", "plexs",
                                                     "q1nwords", "q2nwords")])
x <- td[sample(seq_len(nrow(td)), 10000), c("question1", "question2", "totstems", "dffstems", "nstem",
                                            "corrs", "plexs",
                                            "q1nwords", "q2nwords")]
x$data <- NA
x$data[1:10000] <- "e"
x$data[10001:20000] <- "td"
head(x, 20)
f <- function(x) length(x[abs(scale(x)) > 2])
x %>%
    group_by(data) %>%
    summarise_if(is.numeric, funs(f))

sum(grepl("?$", x$question2))

head(x)
f <- function(x) {
    x <- x[, grep("pred", names(x), invert = TRUE)]
    if (any(grepl("id$", names(x)))) x$id <- as.character(x[[grep("id$", names(x))]])
    og <- x[, !vapply(x, is.numeric, logical(1))]
    x <- x[, vapply(x, is.numeric, logical(1))]
    xx <- x
    xx[, 1:ncol(x)] <- lapply(x, function(.) abs(scale(.)))
    x$var <- rowSums(xx)
    x <- cbind(og[, c("question1", "question2")], x)
    x <- x[order(x$q1nwords, x$q2nwords, decreasing = FALSE), ]
    row.names(x) <- NULL
    x
}
xx <- f(x)

head(xx, 20)
names(e)
sum(!grepl("\\?$", xx$question2))
sum(!grepl("\\?$", xx$question2))
filter(xx, !grepl("\\?$", xx$question2))
f <- function(n) {
    c(mean(c(sum(td$q1nwords < n) / nrow(td),
           sum(td$q2nwords < n) / nrow(td))),
    mean(c(sum(e$q2nwords < n) / nrow(e),
      sum(e$q1nwords < n) / nrow(e))))
}
z <- lapply(2:121, f)
z <- data.frame(matrix(unlist(z), 120, byrow = TRUE))
names(z) <- c("td", "e")
z[[3]] <- z[[1]] - z[[2]]
names(z)[3] <- "diff"
z$n <- 1:120
z <- z[, c(4, 1, 2, 3)]
print(z, digits = 2)
print(tail(arrange(z, diff), 10), digits = 5)

par(family = "Roboto", tcl = -.15, mar = c(4, 3, 2, 1))

with(z[1:50,], plot(n, abs(diff), pch = 20))
with(z[1:50,], lines(n, abs(diff)))
f <- function(x) x^4
.25^4
td$pred2[td$q1nwords < 7 | td$q2nwords < 7] <- td$pred[td$q1nwords < 7 | td$q2nwords < 7]^2
mean(td$pred2)
mean(td$pred2[td$q1nwords < 7 | td$q2nwords < 7])
mean(td$pred[td$q1nwords < 7 | td$q2nwords < 7])
quantile( e$q1nwords, na.rm = TRUE)
quantile(td$q2nwords, na.rm = TRUE)

e
arrange(z, -diff)
td$invalid1 <- !grepl("\\?$", td$question1)
td$invalid2 <- !grepl("\\?$", td$question2)
td$invalid <- td$invalid1 | td$invalid2
sum(td$invalid)
td$invalid1[1:10] | td$invalid2[1:10]
head(x, 20)
x <- x[order(x$totstems, decreasing = FALSE), ]
par(mfrow = c(2, 3), family = "Roboto", tcl = -.15)
hist(e$totstems, col = "#3366ffaa", breaks = 100, xlim = c(0, 100))
hist(e$q1nwords, col = "#3366ffaa", breaks = 75, xlim = c(0, 100))
hist(e$q2nwords, col = "#3366ffaa", breaks = 100, xlim = c(0, 100))
hist(td$totstems, col = "#dd3333aa", breaks = 100, xlim = c(0, 100))
hist(td$q1nwords, col = "#dd3333aa", breaks = 100, xlim = c(0, 100))
hist(td$q2nwords, col = "#dd3333aa", breaks = 100, xlim = c(0, 100))

e$q1nwords <- nwords(e$question1)
e$q2nwords <- nwords(e$question2)

td$q1nwords <- nwords(td$question1)
td$q2nwords <- nwords(td$question2)



head(x, 20)

lapply(
head(f(rsamp(td, n = 100000)), 20)
td
vapply(td, is.numeric, logical(1)
mean(td$pred)

td$pred3 <- (td$pred2 + td$pred2^2) / 2
td$pred4 <- td$pred3^2
td$pred4 <- (td$pred3 + td$pred4) / 2
mean(td$pred4)
td %>%
    dplyr::select(test_id, is_duplicate = pred) %>%
    write.csv("../data/mwk-submission-apr25p.csv",
              row.names = FALSE)

e$q1stems
x <- rsamp(e, 100)$question1
parsesuffix <- function(x) {
    m <- gregexpr("([^ ][[:alnum:]]{3}){1}\\b{1}", x)
##    regmatches(x, m)[[1]] <- gsub("[[:alnum:]]{2}$", "", regmatches(x, m)[[1]])
##    x <- paste(regmatches(x, m)[[1]], collapse = " ")
  ##  m <- gregexpr("[[:alnum:]]{3}\\b", x)
    regmatches(x, m)
}
sfx <- parsesuffix(rsamp(e, 1000)$question1)
head(as.data.frame(sort(table(unlist(sfx)), decreasing = TRUE)), 20)
parsesuffix(e$q1stems[1:5])
q <- e$q1stems[[1]]

2
Rcpp::sourceCpp("../src/boost2.cpp")
Rcpp::sourceCpp("../src/intersect.cpp")

x <- e$question1[1:1000]
Rsplittr <- function(x) {
    x <- tolower(x)
    x <- gsub("^[[:punct:]]|[[:punct:]]$", "", x)
    strsplit(x, " ")
}

td$q1tokens <- splittr(td$question1)
td$q2tokens <- splittr(td$question2)
td$q1token_count <- vapply(td$q1tokens, length, double(1))
td$q2token_count <- vapply(td$q2tokens, length, double(1))
td$token_count <- mapply(function(a, b) sum(c(a, b), na.rm = TRUE),
                        td$q1token_count, td$q2token_count)
td$overlap <- mapply(intersect, td$q1tokens, td$q2tokens)
td$overlap_count <- vapply(td$overlap, length, double(1))
td$overlap_prop <- td$overlap_count / td$token_count
td$corrs <- mapply(corranks, td$q1tokens, td$q2tokens, td$overlap)
td$dataset <- "td"

ntrain <- Inf

## load training data
e <- suppressMessages(
    readr::read_csv("../data/train.csv", n_max = ntrain))
e$q1tokens <- splittr(e$question1)
e$q2tokens <- splittr(e$question2)
e$q1token_count <- vapply(e$q1tokens, length, double(1))
e$q2token_count <- vapply(e$q2tokens, length, double(1))
e$token_count <- mapply(function(a, b) sum(c(a, b), na.rm = TRUE),
                        e$q1token_count, e$q2token_count)
e$overlap <- mapply(intersect, e$q1tokens, e$q2tokens)
e$overlap_count <- vapply(e$overlap, length, double(1))
e$overlap_prop <- e$overlap_count / e$token_count
corranks <- function(a, b, c) {
    if (length(c) == 0) return(0)
    a <- unique(a[a %in% c])
    b <- unique(b[b %in% c])
    b <- factor(b, levels = c)
    a <- factor(a, levels = c)
    cor(as.numeric(a), as.numeric(b), use = "pairwise")
}

e$corrs <- mapply(corranks, e$q1tokens, e$q2tokens, e$overlap)
e$dataset <- "e"

mapply(corrmatches, e$q1tokens[1:10], e$q2tokens[1:10])


rbind(e, td)
names(e)[names(e) %in% names(td)]
rbindnames <- c("overlap_count", "token_count", "overlap_prop", "corrs", "dataset")
td
etd <- rbind(e[, rbindnames], td[, rbindnames])
etd$dataset <- as.numeric(factor(etd$dataset))
etd
etd$dataset <- 2 - etd$dataset
etd$corrs[is.na(etd$corrs)] <- 0
tail(etd)
glm1 <- glm(dataset ~ -1 + overlap_count + token_count, etd,
            family = binomial)
etd$ds3 <- glm1$fitted
median(etd$ds3)

nrow(e) / nrow(etd)
f <- function(m) {
    eeee <- etd[etd$dataset == 1, ]
    n <- nrow(eeee)
    with(eeee, sum(ds3 > m)) / n
}
f(.185)
x <- vapply(seq(.01, .31, .02), f, double(1))
plot(x, seq(.01, .31, .02), type = "b", pch = 20)
library(dplyr)
etd$dsint <- 0
etd$dsint[etd$ds3 > .1] <- 1
etd %>% group_by(dataset) %>% summarise(m = median(ds, na.rm = TRUE))
etd %>% group_by(dataset, dsint) %>% summarise_at(mean)



x <- vapply(seq(.25, .45, .001), f, double(1))
par(family = "Roboto", tcl = -.15, mar = c(4, 3, 2, 1), bty = "n")
plot(seq(.25, .45, .001), 1 - x, type = "b", pch = 20)
with(etd, sum(dataset == 0 & ds < median(ds)))
with(etd, sum(ds > .50))
f(.05)

etd$dsint <- 0
etd$dsint[etd$ds3 > .125] <- 1
subset(etd, dataset == 0) %>% group_by(dsint) %>% summarise(n = n())
etd %>% group_by(dataset, dsint) %>% summarise_all(funs(mean))

etd2 <- dplyr::arrange(etd, -ds)
print(etd2[400:500, ], n = 100)
summary(glm1)

summary(lm(is_duplicate ~ overlap_count + token_count + overlap_prop + corrs, e))

e$corrs[is.na(e$corrs)] <- 0
options(tibble.width = 1000)
tokenize(remove_punctuation(lowerCase(e$question1[1])))
str_split(e$question1[1], "\\w|\\?", FALSE)



x <- read.csv("~/r/kaggle/quora/data/mwk-submission-apr25p.csv")
mean(x$is_duplicate)
x$is_duplicate <- x$is_duplicate / 2
write.csv(x, "~/r/kaggle/quora/data/mwk-submission-apr25r.csv", row.names = FALSE)
head(x)
