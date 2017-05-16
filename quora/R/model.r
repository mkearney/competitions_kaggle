##mwk
##quora training model
##kaggle competition data set
##04/02/2017
## warcraft mode
warcraft::warcraft_mode(p = .5)
options(tibble.width = 1000)

## install package if not already
pkgs <- c("dplyr", "gbm", "caret")
if (any(!pkgs %in% installed.packages())) {
    install.packages(pkgs)
}

## update packages
update.packages(ask = FALSE)

## load dplyr and gbm
suppressPackageStartupMessages(library(dplyr))
##suppressPackageStartupMessages(library(gbm))

ntrees <- 500
ntest <- 1000
ntrain <- Inf

## load training data
e <- suppressMessages(
    readr::read_csv("../data/train.csv", n_max = ntrain))

e$q1stems <- stems(e$question1)
e$q2stems <- stems(e$question2)
totstems <- vapply(e$q1stems, length, double(1))
e$totstems <- totstems + vapply(e$q2stems, length, double(1))
e$dffstems <- abs(totstems - vapply(e$q2stems, length, double(1)))

countmatches <- function(a, b) sum(unique(a) %in% unique(b), na.rm = TRUE)
plexmatches <- function(a, b) {
    a <- unique(a)
    b <- unique(b)
    n <- nchar(a)
    mean(n[a %in% b], na.rm = TRUE)
}
corrmatches <- function(a, b) {
    a <- unique(a)
    b <- unique(b)
    a <- a[a %in% b]
    if (length(a) == 0) return(0)
    b <- b[b %in% a]
    cor(as.numeric(factor(b, levels = a)),
        as.numeric(factor(a, levels = a)))
}
e$nstem <- mapply(countmatches, e$q1stems, e$q2stems)
e$corrs <- mapply(corrmatches, e$q1stems, e$q2stems)
e$plexs <- mapply(plexmatches, e$q1stems, e$q2stems)

summary(lm(is_duplicate ~ totstems * nstem * dffstems * corrs * plexs, e))
summary(lm(is_duplicate ~ totstems * nstem + nstem * plexs + dffstems + totstems * corrs, e))

rsamp <- function(x, n = 10) x[sample(seq_len(nrow(x)), n), ]
dfs <- function(x) {
    n <- ceiling(nrow(x) / 2)
    x <- x[sample(seq_len(nrow(x)), nrow(x)), ]
    seqs <- mapply(seq, c(1, seq(n, nrow(x), length.out = 6)[-6] + 1),
                   c(seq(n, nrow(x), length.out = 6)))
    lapply(seqs, function(.) x[., ])
}
mdl <- function(x, ntrees = 3500) {
    gbm::gbm(is_duplicate ~ totstems + nstem + plexs + dffstems + corrs,
             x, distribution = "bernoulli",
             n.trees = ntrees,
             interaction.depth = 4,
             shrinkage = 0.1,
             n.minobsinnode = 10,
             bag.fraction = 0.5,
             train.fraction = 1.0)
}
pmdl <- function(x, mod, ntrees = 3500) {
    pred <- predict(mod, newdata = data.frame(x), type = "response", n.trees = ntrees)
    Metrics::logLoss(x$is_duplicate, pred)
}

mdlr <- function(x) {
    mod <- mdl(x[[1]])
    lapply(x[2:length(x)], pmdl, mod = mod)
}
e$plexs[is.nan(e$plexs)] <- 0
e$corrs[is.na(e$corrs)] <- 0
e
mdlr(dfs(e))

stems(e$question1[1:10])
names(e)

lapply(e, mdl)
x <- e$question1[1:100]
?gregexpr
nwords <- function(x) vapply(gregexpr("[[:alnum:]]{1,}", x), length, double(1))
stems <- function(x) {
    f <- function(x) {
        m <- gregexpr("[[:alnum:]]{5,}", x)
        regmatches(x, m)[[1]] <- gsub("[[:alnum:]]{2}$", "", regmatches(x, m)[[1]])
        m <- gregexpr("[[:alnum:]]{3,}", x)
        x <- regmatches(x, m)[[1]]
        tolower(x)
    }
    lapply(x, f)
}
allstems <- function(x) {
    f <- function(x) {
        m <- gregexpr("[[:alnum:]]{2,}", x)
        x <- regmatches(x, m)[[1]]
        tolowefr(x)
    }
    lapply(x, f)
}


substr(x, 1, 9)
lapply(c(5000, rep(1000, 4)) rsamp(e)
dps <- function(...)
    vapply(lazyeval::dots_capture(...),
           function(x) as.character(x)[2], character(1))
rs <- function(x, ..., n = 1000) {
    seq <- seq_len(nrow(x))
    x[sample(seq, n), dps(...)]
}

tbl <- function(x, n = 20) {
    x <- table(x)
    x <- sort(x, decreasing = TRUE)
    names(x) <- substr(names(x), 1, 10)
    head(as.data.frame(x), n)
}
cutr <- function(x, y) {
    x <- tolower(x)
    y <- tolower(y)
    x <- gsub("[ ]{2,}", " ", x)
    x <- strsplit(x, " ")
    y <- gsub("[ ]{2,}", " ", y)
    y <- strsplit(y, " ")
    mapply(function(a, b) suppressWarnings(sum(a == b, na.rm = TRUE)), x, y)
}

## read stop words
stopwords <- readLines("../data/stopwords.csv")
## split string to create stop words vector
stopwords <- strsplit(stopwords, ", ")[[1]]

## source funs
source("funs.R")


n <- nchar(x)
?gsub
x <- e$question1[1:10]
regexec("[[:alnum:]{3,}]", x[[1]])
if (n > 4) {
x <- substr(x, 1, n - 1)
}

counter <- function(f, e = e) {
    a <- f(e$question1)
    b <- f(e$question2)
    x <- abs(a - b)
    f <- function(a, b) mean(c(a, b), na.rm = TRUE)
    w <- unlist(mapply(f, a, b, SIMPLIFY = FALSE))
    w <- (max(w) + 1) - w
    as.numeric(x * w)
}
ltrs <- function(x) strsplit(gsub("[ ]{1,}", "", x), "")
wrds <- function(x) {
    x <- paste0("z ", x)
    strsplit(gsub("[ ]{2,}", " ", gsub("[[:punct:]]", "", tolower(x))), " ")
}
corltr <- function(a, b) {
    lvs <- mapply(function(a, b) unique(a)[unique(a) %in% unique(c(a, b))], a, b)
    f1 <- function(a, b) {
        if (length(a) > length(b)) b <- c(b, rep(NA, length(a)-length(b)))
        b
    }
    f2 <- function(a, b) {
        if (length(b) > length(a)) a <- c(a, rep(NA, length(b)-length(a)))
        a
    }
    b <- mapply(f1, a, b)
    a <- mapply(f2, a, b)
    a <- mapply(function(a, b) as.numeric(factor(a, levels = b)), a, lvs)
    b <- mapply(function(a, b) as.numeric(factor(a, levels = b)), b, lvs)
    f3 <- function(a, b) suppressWarnings(cor(as.numeric(a), as.numeric(b), use = "pairwise"))
    mapply(f3, a, b)
}
fcounter <- function(a, b, f) {
    a <- f(a)
    b <- f(b)
    f <- function(a, b) {
        a <- a + 1
        b <- b + 1
        x <- abs(a - b)
        a / (a + b)
    }
    mapply(f, a, b)
}
Y <- e$is_duplicate
e$ms <- cutr(e$question1, e$question2)
e$commas <- fcounter(e$question1, e$question2, count_commas)
e$periods <- fcounter(e$question1, e$question2, count_periods)
e$uppers <- fcounter(e$question1, e$question2, count_uppers)
e$numbers <- fcounter(e$question1, e$question2, count_numbers)
e$punts <- fcounter(e$question1, e$question2, count_punts)
e$nmatch1 <- nmatch(fullstop(wtoken(e$question1)),
                    fullstop(wtoken(e$question2)))
e$question1 <- fullstop(wtoken(e$question1), FALSE)
e$question2 <- fullstop(wtoken(e$question2), FALSE)
e$nq1 <- vapply(e$question1, length, double(1))
e$nq2 <- vapply(e$question2, length, double(1))

syn <- readRDS("~/syndex/data/thesaurus.rds")
getsyns <- function(x) {
    f <- function(x) syn$syn[syn$word %in% unique(x)]
    lapply(x, f)
}

q1syn <- getsyns(e$question1)
q1syn <- lapply(q1syn, strsplit, ",")
e$q1syn <- nmatch(q1syn, e$question2)

mod <- lm(is_duplicate ~ x, data = e)
summary(mod)
predict(mod, newdata = e)
e$pred <- mod$fitted
smrz <- function(var) {
    means <- vapply(c(0, 1), function(.) mean(subset(e, is_duplicate == .)[[var]], na.rm = TRUE), double(1))
    stdev <- sd(e[[var]], na.rm = TRUE)
    meandiff <- means[1] - means[2]
    t <- meandiff / stdev
    data.frame(no = sprintf("%.3f", means[1]), yes = sprintf("%.3f", means[2]), t = sprintf("%.3f", t))
}

e$x <- with(e, sqrt(nmatch1 / (nq1 + nq2)))

smrz("x")
e
subset(e, is_duplicate == 1) %>%
    rs("question1", "question2", "nmatch1", "nq1", "nq2", "q1syn", n = 10) %>%
    data.frame()
sdm <- function(x) mean(x, na.rm = TRUE) / sd(x)
e %>%
    mutate(nq12 = nq1 * nq2 * nmatch1) %>%
    group_by(is_duplicate) %>%
    summarise_at(vars(nmatch1, nq1, nq2, q1syn, nq12), funs(mean))

?summarise_all
q2syn <- getsyns(e$question2)

e$nmatchpct1 <- e$nmatch1 / nword(e$question1)
e$nmatch2 <- nmatch(e$question1, e$question2)
e$bigword <- bigwordmatch(e$question1, e$question2)
e$sames <- as.numeric(samesies(e$question1, e$question2))
e$first <- as.numeric(wfirst(e$question1, e$question2))
e$last <- as.numeric(wlast(e$question1, e$question2))
e$wordcor <- wordorder(e$question1, e$question2)
e$r <- mapply(cor_words, e$question1, e$question2)
e$dfchars <- chardiffs(e$question1, e$question2)

fmean <- function(x) mean(x, na.rm = TRUE)
fscale <- function(x) as.numeric(scale(x))
options(tibble.width = 10000)

## reduce and format data
logit <- function(x) log(x + 1)
elog <- e %>%
    mutate_if(is.integer, logit)
elog$is_duplicate <- Y
## create test set #1
d1 <- rsamp(e, ntest)

## create test set #2
d2 <- rsamp(e, ntest)

## boosted gradiant model
library(gbm)
ntrees <- 5000
?gbm
mod <- gbm(is_duplicate ~ .,
           data = e[, !names(e) %in% c("question1", "question2", "qid1", "qid2", "id")],
           interaction.depth = 4,
           shrinkage = 0.1,
           n.minobsinnode = 30,
           bag.fraction = 0.5,
           train.fraction = 1.0,
           cv.folds = 2,
           distribution = "bernoulli",
           n.trees = ntrees)
summary(mod)
## best iteration
best.iter <- gbm.perf(mod, method = "cv")

## view model results
##summary(mod)
## based on optimal number of trees
summary(mod, n.trees = ntrees)

## calculate predicted probabilities
d1$pred <- predict(
    mod, newdata = d1, type = "response",
    n.trees = ntrees)
d2$pred <- predict(
    mod, newdata = d2, type = "response",
    n.trees = ntrees)

## calculate logloss
ll1 <- logloss(d1$is_duplicate, d1$pred)
ll2 <- logloss(d2$is_duplicate, d2$pred)
## print LL output
message(paste(
    "Average logloss of",
    sprintf("%.2f", mean(c(ll1, ll2)))))

## save model
saveRDS(mod, "../data/mod8.rds")

## save data
readr::write_rds(e, "../data/mutated-training6.rds")
readr::write_rds(elog, "../data/mutated-training6log.rds")
## wipe training data set from memory
##rm(e, d1, d2)


##----------------------------------------------------------------##
##                      CREATE SUBMISSION                         ##
##----------------------------------------------------------------##

## read model
list.files("../data")
mod <- readRDS("../data/mod5.rds")

## read test data
td <- readr::read_rds("../data/test-mutate-3.rds")
td$ms <- cutr(td$question1, td$question2)
td$nmatch2 <- nmatch(td$question1, td$question2)
td$bigword <- bigwordmatch(td$question1, td$question2)

logit <- function(x) log(x + 1)
tdlog <- td %>%
    mutate_if(is.integer, logit)

td1 <- readr::read_csv("../data/test.csv")
td$q1 <- td1$question1
td$q2 <- td1$question2
rm(td1)
## mutate and add variables to test data
##td$nmatch1 <- nmatch(fullstop(wtoken(td$question1)),
##                    fullstop(wtoken(td$question2)))
##td$question1 <- fullstop(wtoken(td$question1), FALSE)
##td$question2 <- fullstop(wtoken(td$question2), FALSE)
##td$nmatchpct1 <- td$nmatch1 / nword(td$question1)
##td$sames <- samesies(td$question1, td$question2)
##td$last <- wlast(td$question1, td$question2)
##td$wordcor <- wordorder(td$question1, td$question2)
##td$r <- mapply(cor_words, td$question1, td$question2)
##td$dfchars <- chardiffs(td$question1, td$question2)
td$commas <- fcounter(td$q1, td$q2, count_commas)
td$periods <- fcounter(td$q1, td$q2, count_periods)
td$uppers <- fcounter(td$q1, td$q2, count_uppers)
td$numbers <- fcounter(td$q1, td$q2, count_numbers)
td$first <- wfirst(td$question1, td$question2)
td$sames <- as.numeric(td$sames)
td$first <- as.numeric(td$first)
td$last <- as.numeric(td$last)
td$punts <- fcounter(td$question1, td$question2, count_punts)

readr::write_rds(td, "../data/test-mutate-3e.rds")

## generate predictions
library(gbm)
td$pred <- predict(
    mod, newdata = td, type = "response", n.trees = ntrees,
    na.action = na.omit)
td

attr(mod$Terms, "term.labels")[!attr(mod$Terms, "term.labels") %in% names(td)]
## sum missing
sum(is.na(td$pred))
## few items are missing. not sure if by design
##td$pred[which(is.na(td$pred))] <- .5
par(tcl = -.125, family = "Roboto Condensed")
mean(sqrt(td$pred[sample(seq_len(nrow(td)), 50000)]))

mean(td$pred[sample(seq_len(nrow(td)), 50000)])
mean(td$pred2[sample(seq_len(nrow(td)), 50000)])
x <- td[sample(seq_len(nrow(td)), 50000), ] %>%
    .[, c("pred", "pred2")]
ggplot(x, aes(pred, pred2)) + geom_point()
x
e <- readr::read_rds("../data/mutated-training5.rds")
mod2 <- glm(is_duplicate~nmatchpct1 + first + last, e, family = binomial)

plot(density(sqrt(td$pred[sample(seq_len(nrow(td)), 50000)])), col = "blue", lwd = 3, bty = "n")
ggplot(td, aes(x = pred)) + geom_density()
## re-read for id
tdid <- readr::read_csv("../data/test.csv")
td$test_id <- tdid$test_id
rm(tdid)
tdlog
mean(tdlog$pred)
td

## save data as submission file
td %>%
    dplyr::select(test_id, is_duplicate = pred) %>%
    write.csv("../data/mwk-submission-apr21h.csv",
              row.names = FALSE)


td <- td %>%
    mutate(nchars = nchar_diff(question1, question2),
           nmatch1 = nmatch(fullstop(wtoken(question1)),
                            fullstop(wtoken(question2))),
           question1 = fullstop(wtoken(question1), FALSE),
           question2 = fullstop(wtoken(question2), FALSE),
           nmatchpct1 = nmatch1 / nword(question1),
           nmatchpct2 = nmatch1 / nword(question2)) %>%
    mutate(nwords = nword_diff(question1, question2),
           nmatch2 = nmatch(question1, question2),
           bigword = bigwordmatch(question1, question2),
           chunks = chunker(question1, question2),
           sames = samesies(question1, question2),
           first = wfirst(question1, question2),
           last = wlast(question1, question2),
           wordcor = wordorder(question1, question2),
           nonwords = nonwords(question1, question2))

## reduce and format data
td <- td %>%
    mutate(sames = as.factor(sames),
           first = as.factor(first),
           last = as.factor(last)) %>%
    dplyr::select(-id, -qid1, -qid2,
                  -question1, -question2)

## generate predictions
td$pred <- predict(
    mod, newdata = td, type = "response")

## few items are missing. not sure if by design
td$pred[which(is.na(td$pred))] <- .5

## save data as submission file
td %>%
    dplyr::select(test_id = id, is_duplicate = pred) %>%
    write.csv("../data/mwk-submission-apr4.csv",
              row.names = FALSE)

## save calculated data
td %>%
    write.csv("../data/mwk-submission-apr4-fulldata.csv",
              row.names = FALSE)


model <- "binda"
levels(e[["first"]]) <- c(0, 1)
levels(e[["last"]]) <- c(0, 1)
levels(e[["sames"]]) <- c(0, 1)
e[["nonwords"]] <- as.numeric(e[["nonwords"]])
e[, 1:ncol(e)] <- lapply(e, as.numeric)

e <- data.frame(na.omit(e))
e
trainX <- as.matrix(e[, -1])
e$is_duplicate <- factor(e[, 1])

levels(e$is_duplicate) <- c("no", "yes")
install.packages("QSARdata")
library(QSARdata)
data(bbb2)

training <- e[, -1]
training$Class <- factor(e[, 1])
testing <- training[1:2000,]
apply(trainX, 2, class)
cctrl1 <- trainControl(method = "cv", number = 2, returnResamp = "all",
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(845)
e

test_class_cv_model <- train(is_duplicate ~ .,
                             data = e,
                             method = "binda",
                             trControl = cctrl1,
                             verbose = FALSE,
                             metric = "ROC")
trainX[1:5, ]
trainY[1:5]
set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training,
                            method = "binda",
                            trControl = cctrl1,
                            verbose = FALSE,
                            metric = "ROC")


