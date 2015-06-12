#Question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
library(caret)
set.seed(33833)
modelVowelRf <- train(y ~ ., data = vowel.train, method = "rf")
set.seed(33833)
modelVowelGbm <- train(y ~ ., data = vowel.train, method = "gbm")
predVowelRf <- predict(modelVowelRf, newdata = vowel.test)
predVowelGbm <- predict(modelVowelGbm, newdata = vowel.test)
mean(predVowelRf == vowel.test$y)
mean(predVowelGbm == vowel.test$y)
indAgree <- which(predVowelRf == predVowelGbm)
mean(predVowelGbm[indAgree] == vowel.test$y[indAgree])

#Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
modelAlRf <- train(diagnosis ~ ., data = training, method = "rf")
set.seed(62433)
modelAlGbm <- train(diagnosis ~ ., data = training, method = "gbm")
set.seed(62433)
modelAlLda <- train(diagnosis ~ ., data = training, method = "lda")
#as we don't have a validation data set, we will stack our model on training set and measure accuracy on test set for once.
predAlRf <- predict(modelAlRf, newdata = training)
predAlGbm <- predict(modelAlGbm, newdata = training)
predAlLda <- predict(modelAlLda, newdata = training)
predictData <- data.frame(rf = predAlRf, gbm = predAlGbm, lda = predAlLda, truth = training$diagnosis)
modelStackRf <- train(truth ~ ., data = predictData, method = "rf")
#calculating accuracy for each method and stack
predTestRf <- predict(modelAlRf, newdata = testing)
predTestGbm <- predict(modelAlGbm, newdata = testing)
predTestLda <- predict(modelAlLda, newdata = testing)
predictTestData <- data.frame(rf = predTestRf, gbm = predTestGbm, lda = predTestLda, truth = testing$diagnosis)
predTestStackRf <- predict(modelStackRf, newdata = predictTestData)
mean(predTestRf == testing$diagnosis)
mean(predTestGbm == testing$diagnosis)
mean(predTestLda == testing$diagnosis)
mean(predTestStackRf == testing$diagnosis)

#Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
modelCon <- train(CompressiveStrength ~ ., data = training, method = "lasso")
?plot.enet
plot.enet(modelCon$finalModel, xvar = "penalty")
plot.enet(modelCon$finalModel)

#Question 4
fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(fileUrl, destfile = "gaData.csv", method = "curl")
library(lubridate)  # For year() function below
dat = read.csv("./gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest <- ts(testing$visitsTumblr, start = 366)
library(forecast)
fit.bats <- bats(y = tstrain)
forecastBats <- forecast(fit.bats, level = 95, h = 235)
mean((tstest < forecastBats$upper) & (tstest > forecastBats$lower))

#Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
library(e1071)
set.seed(325)
fit.svm <- svm(CompressiveStrength ~ ., data = training)
predConSvm <- predict(fit.svm, newdata = testing)
sqrt(mean((predConSvm - testing$CompressiveStrength)^2))