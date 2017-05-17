##mwk
##2017/05/08

## data directory
ddir <- file.path("..", "input")

## read training data
train <- read.csv(file.path(ddir, "train.csv"))

## read test data
test <- read.csv(file.path(ddir, "test.csv"))

## missing on DV
test$price_doc <- NA

## read in funs
source("funs.R")

##----------------------------------------------------------------------------##
##                        WRANGLE/MUNGE TRAIN DATA
##----------------------------------------------------------------------------##

## id numeric (but not id or dv) vars
nums <- sapply(train, is.numeric) & !names(train) %in% c("id", "price_doc")

## convert to numeric (int to numeric)
train[nums] <- lapply(train[nums], as.numeric)

## create year var from timestamp var
train$year <- as.double(substr(as.character(train$timestamp), 1, 4))

## convert timestamp to numeric type
train$timestamp <- as.numeric(as.Date(as.character(train$timestamp)))

## id factor (but not id or dv) vars
fcts <- (sapply(train, is.character) | sapply(train, is.factor)) &
    !names(train) %in% c("id", "price_doc")

## function to est freq proportions
fct_prop <- function(x) {
    table(x) / sum(table(x), na.rm = TRUE)
}

## exclude sub area
fctprops <- lapply(train[which(fcts)], fct_prop)

## drop unbalanced factors and keep those with at least 5% var
fcts2kp <- which(fcts)[sapply(fctprops, function(x) all(x > .05))]

## convert keepers to numeric
train[fcts2kp] <- lapply(train[fcts2kp], function(x) as.numeric(as.factor(x)))

## checkout sub area variable
sort(table(train$sub_area), decreasing = TRUE)[1:20]

## try dummy coding by "poselenie" keyword
train$sub_area <- as.numeric(grepl("Poselenie", train$sub_area))

## drop non-numeric for final data set
train <- train[sapply(train, is.numeric) | names(train) %in% c("id", "price_doc")]

##----------------------------------------------------------------------------##
##                        WRANGLE/MUNGE TEST DATA
##----------------------------------------------------------------------------##

## do corresponding conversions to test data
nums <- sapply(test, is.numeric) & !names(test) %in% c("id", "price_doc")
test[nums] <- lapply(test[nums], as.numeric)
test$year <- as.double(substr(as.character(test$timestamp), 1, 4))
test$timestamp <- as.numeric(as.Date(as.character(test$timestamp)))
fcts <- (sapply(test, is.character) | sapply(test, is.factor)) &
    !names(test) %in% c("id", "price_doc")
test[fcts] <- lapply(test[fcts], function(x) as.numeric(as.factor(x)))
test$sub_area <- as.numeric(grepl("Poselenie", test$sub_area))
test <- test[names(test) %in% names(train)]

##----------------------------------------------------------------------------##
##                         MERGE WITH EXTERNAL DATA
##----------------------------------------------------------------------------##

## load dplyr
library(dplyr)

## spas data
spas <- readr::read_csv("../input/SPASTT01RUM661N.csv")
spas$year <- as.double(substr(spas$DATE, 1, 4))
spas <- spas %>%
    group_by(year) %>%
    summarise(spas = mean(SPASTT01RUM661N, na.rm = TRUE)) %>%
    ungroup()

## int data
int <- readr::read_csv("../input/INTDSRRUM193N.csv")
int$year <- as.double(substr(int$DATE, 1, 4))
int <- int %>%
    group_by(year) %>%
    summarise(int = mean(INTDSRRUM193N, na.rm = TRUE)) %>%
    ungroup()

## api data
api <- readr::read_csv("../input/API_RUS_DS2_en_csv_v2.csv", skip = 3)
api <- data.frame(year = 2009:2015, apirusds2 = as.double(unclass(api[60, 54:60])))
api <- api %>%
    group_by(year) %>%
    summarise(api = mean(apirusds2, na.rm = TRUE)) %>%
    ungroup()

## merge
train <- dplyr::left_join(train, spas, by = "year")
test <- dplyr::left_join(test, spas, by = "year")

train <- dplyr::left_join(train, int, by = "year")
test <- dplyr::left_join(test, int, by = "year")

train <- dplyr::left_join(train, api, by = "year")
test <- dplyr::left_join(test, api, by = "year")

##train <- train[complete.cases(train), ]

## load gbm package
library(gbm)

## spec number of trees
n.trees <- 500

## discard misssing
tc <- train[!apply(train[, names(train) != "int"], 1, function(x) any(is.na(x))), ]
tc <- tc[, sapply(tc, var, na.rm = TRUE) > 0]

## distribution
dist <- gbm:::guessDist(tc$price_doc)
dist <- "poisson"

## set params and run model
m1 <- gbm(price_doc ~ .,
          data = var.omit(tc, "id"),
          n.trees = n.trees,
          interaction.depth = 5,
          shrinkage = .05,
          n.minobsinnode = 5,
          distribution = dist,
          train.fraction = .5,
          bag.fraction = .5)

## summary
summary(m1, n.trees = n.trees)

## if CV, find best iter (otherwise assume n.trees)
##best.iter <- gbm.perf(m1, method = "cv")
##summary(m1, n.trees = best.iter)

## generate predictions
test$price_doc <- predict(m1, newdata = test,
                          n.trees = n.trees,
                          type = "response")

## reshape?
##test$price_doc <- reshapedist(test$price_doc, mean(train$price_doc))
test$price_doc <- ((test$price_doc - min(test$price_doc)) + 100000) * .675
## check distribution of preds
hist(test$price_doc, col = "gray", breaks = 40, xlim = c(0, 60000000))
hist(train$price_doc, col = "gray", breaks = 40, xlim = c(0, 60000000))
sum(is.na(test$price_doc))
range(test$price_doc)
range(train$price_doc)
sort(test$price_doc, decreasing = TRUE)[1:20]
mean(test$price_doc)
mean(train$price_doc)

bst <- read.csv("../input/submit22.csv")

plot(bst$price_doc, test$price_doc,
     pch = 20,
     cex = 2,
     bty = "n",
     tcl = -.15,
     col = "#00000022")
mean(bst$price_doc)
mean(test$price_doc)

## save predictions
write.csv(test[, c("id", "price_doc")],
          "../input/submit24.csv",
          row.names = FALSE)

