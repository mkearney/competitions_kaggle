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

## combine data sets
##tt <- rbind(test, train)

## read in funs
source("funs.R")

## train data
nums <- sapply(train, is.numeric) & !names(train) %in% c("id", "price_doc")
train[nums] <- lapply(train[nums], as.numeric)
train$timestamp <- as.numeric(as.Date(as.character(train$timestamp)))

## test data
nums <- sapply(test, is.numeric) & !names(test) %in% c("id", "price_doc")
test[nums] <- lapply(test[nums], as.numeric)
test$timestamp <- as.numeric(as.Date(as.character(test$timestamp)))

##
##train[, !names(train) %in% c("id", "price_doc")] <- intnum(var.omit(train))
##test[, !names(test) %in% c("id", "price_doc")] <- intnum(var.omit(test))

## drop missing
##train <- var.omit(train[complete.cases(train), ], "id")

## factors
fcts <- (sapply(train, is.character) | sapply(train, is.factor)) &
    !names(train) %in% c("id", "price_doc")

## frequency proportions
fct_prop <- function(x) {
    table(x) / sum(table(x), na.rm = TRUE)
}

## exclude sub area
fctprops <- lapply(train[which(fcts)], fct_prop)

## drop and keep these ones
fcts2kp <- which(fcts)[sapply(fctprops, function(x) all(x > .05))]

## convert keepers to numeric
train[fcts2kp] <- lapply(train[fcts2kp], function(x) as.numeric(as.factor(x)))

## checkout sub area
sort(table(train$sub_area))

## try dummy coding by poselenie keywords
train$sub_area <- as.numeric(grepl("Poselenie", train$sub_area))

## drop non-numeric
train <- train[sapply(train, is.numeric) | names(train) %in% c("id", "price_doc")]

## do same to test
fcts <- (sapply(test, is.character) | sapply(test, is.factor)) &
    !names(test) %in% c("id", "price_doc")
test[fcts] <- lapply(test[fcts], function(x) as.numeric(as.factor(x)))
test$sub_area <- as.numeric(grepl("Poselenie", test$sub_area))
test <- test[names(test) %in% names(train)]


## gbm model
library(gbm)

spas <- readr::read_csv("../input/SPASTT01RUM661N.csv")
spas$build_year <- as.double(difftime(spas$DATE, as.Date("2010-01-01"), units = "days") / 365)

int <- readr::read_csv("../input/INTDSRRUM193N.csv")
int$build_year <- as.double(difftime(int$DATE, as.Date("2010-01-01"), units = "days") / 365)

api <- readr::read_csv("../input/API_RUS_DS2_en_csv_v2.csv", skip = 3)
api <- data.frame(build_year = 0:6, apirusds2 = as.double(unclass(api[60, 54:60])))

train.trim <- left_join(train.trim, spas[, -1], by = "build_year")
test <- left_join(test, spas[, -1], by = "build_year")

train.trim <- left_join(train.trim, int[, -1], by = "build_year")
test <- left_join(test, int[, -1], by = "build_year")

train.trim <- left_join(train.trim, api, by = "build_year")
test <- left_join(test, api, by = "build_year")

## set params and run model
n.trees <- 1e3
m1 <- gbm(price_doc ~ .,
          data = var.omit(train, "id"),
          n.trees = n.trees,
          interaction.depth = 3,
          shrinkage = .05,
          n.minobsinnode = 5,
          distribution = "poisson",
          train.fraction = .5,
          bag.fraction = .5)

## summary
summary(m1, n.trees = n.trees)

## find best iter
best.iter <- gbm.perf(m1, method = "OOB")
summary(m1, n.trees = best.iter)


## generate predictions
test$price_doc <- predict(m1, newdata = test,
                          n.trees = n.trees,
                          type = "response")

## reshape?
##test$price_doc <- reshapedist(test$price_doc, mean(train$price_doc))

## check distribution of preds
hist(test$price_doc, col = "gray", breaks = 30)
sum(is.na(test$price_doc))
range(test$price_doc)
sort(test$price_doc, decreasing = TRUE)[1:20]
mean(test$price_doc)
mean(train$price_doc)

bst <- read.csv("../input/submit7.csv")

plot(bst$price_doc, test$price_doc, pch = 1, bty = "n", tcl = -.15, col = "#00000066")
mean(bst$price_doc)

## save predictions
write.csv(test[, c("id", "price_doc")],
          "../input/submit18.csv",
          row.names = FALSE)


reshapedist <- function(x, avg) {
    x <- x - (min(x, na.rm = TRUE) - 0)
    (avg / mean(x, na.rm = TRUE)) * x
}
mean(test$price_doc)
mean(train$price_doc)
plot(reshapedist(test$price_doc, mean(train$price_doc)),
     test$price_doc)
