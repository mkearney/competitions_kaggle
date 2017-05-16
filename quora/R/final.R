##mwk
##quora training model
##kaggle competition data set
##04/25/2017

## install package if not already
pkgs <- c("dplyr", "gbm", "readr", "Metrics", "Rcpp")
if (any(!pkgs %in% installed.packages())) {
    install.packages(pkgs)
}

## update packages
update.packages(ask = FALSE)

## load dplyr
suppressPackageStartupMessages(library(dplyr))

## number of trees
ntrees <- 1000
## load all training data
ntrain <- Inf

## read in functions
source("make_features.R")

## add features training data or load saved version
if (!"finale.rds" %in% list.files(file.path("..", "data"))) {
    ## load training data
    e <- suppressMessages(
        readr::read_csv("../data/train.csv", n_max = ntrain))
    ## extract features
    e <- make_features(e, 1L)
    ## preview
    hd(e, 5)
    ## save mutated training data
    readr::write_rds(e, "../data/finale.rds")
} else {
    ## read mutated training data
    e <- readr::read_rds("../data/finale.rds")
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
    readr::write_rds(td, "../data/finaltd.rds")
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
fis <- "is_duplicate ~ overlap_count + token_count + overlap_prop + corrs + dsp"
ismod <- mdl(subset(etd, !is.na(etd$is_duplicate)), fis, ntrees)

## view results
summary(ismod, n.trees = ntrees)

## adjust ntrees
##best.iter <- gbm::gbm.perf(ismod, method = "OOB")
##ntrees <- ceiling(mean(c(best.iter, best.iter, ntrees, ntrees, ntrees)))

## get predictions
etd$pred <- prd(etd, ismod, ntrees)

## calculate log loss
Metrics::logLoss(etd$is_duplicate[etd$ds == 1],
                 etd$pred[etd$ds == 1])

## check out mean estimates
etd %>%
    mutate(pred = revsq(pred)) %>%
    group_by(ds) %>%
    summarise(pred = mean(pred, na.rm = TRUE),
              dsp = mean(dsp, na.rm = TRUE),
              isdup = mean(is_duplicate, na.rm = TRUE))
library(ggplot2)
etd %>%
    rsamp(100000) %>%
    ggplot(aes(x = pred, fill = ds)) +
    geom_density(bw = .05) +
    facet_wrap(~ ds)

## plot
etd %>%
    ##mutate(pred = ifelse(dsp < .15, 0, pred)) %>%
    rsamp(100000) %>%
    ggplot2::ggplot(ggplot2::aes(x = dsp2, y = pred, color = factor(ds))) +
    ggplot2::geom_point(alpha = .5, size = 1) + ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = c("#dd3333", "#3366ff")) +
    ggplot2::theme(legend.position = "none", text = ggplot2::element_text(family = "Roboto")) +
    ggplot2::facet_wrap(~ factor(ds, labels = c("Test Data", "Training Data"))) +
    ggplot2::labs(x = "Probability of being training obs",
                  y = "Probability of positive classification")

## save submission
revsq <- function(x) (1 - (1 - x)^2)^2
par(tcl = -.15, family = "Roboto", bty = "n")
plot(seq(.01, .99, .01), seq(.01, .99, .01), type = "n", xlab = "", ylab = "")
points(seq(.01, .99, .01), revsq(seq(.01, .99, .01)), pch = 21,
     cex = .75, bg = "#aa0000aa")
points(seq(.01, .99, .01), seq(.01, .99, .01)^2, pch = 21,
       cex = .75, bg = "#0022aaaa")
points(seq(.01, .99, .01), seq(.01, .99, .01)^2^2, pch = 21,
       cex = .75, bg = "#aa00aaaa")
points(seq(.01, .99, .01), seq(.01, .99, .01)/2, pch = 21,
       cex = .75, bg = "#00aa00aa")
points(seq(.01, .99, .01), seq(.01, .99, .01), pch = 21,
     cex = .75, bg = "#777777aa")
etd %>%
    dplyr::filter(ds == 0) %>%
    mutate(pred = revsq(pred)) %>%
    bind_cols(td[, "test_id"]) %>%
    dplyr::select(test_id, is_duplicate = pred) %>%
    write.csv(paste0("mwk-quora-", Sys.time(), ".csv"),
              row.names = FALSE)

install.packages("imager")
