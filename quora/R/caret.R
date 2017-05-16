library(caret)
traindata <- na.omit(e)[, -1]
trainclass <- na.omit(e)[, 1]
trainclass$is_duplicate <- factor(trainclass$is_duplicate)
mod_caret <- train(is_duplicate ~ .,
                   data = data.frame(na.omit(e)),
                   method = "binda",
                   lambda.freqs = .01,
                   preProcess = c("log"))

summary(mod_caret)

?getModelInfo()
library(randomForest)
model <- randomForest(taste ~ . - quality, data = train)


