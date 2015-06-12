#Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData <- data.frame(diagnosis, predictors)
testIndex <- createDataPartition(diagnosis, p = 0.5, list = FALSE)
training <- adData[-testIndex, ]
testing <- adData[testIndex, ]

#Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain <- createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training <- mixtures[inTrain, ]
testing <- mixtures[-inTrain, ]

hist(training$Superplasticizer)
#or
library(ggplot2)
qplot(Superplasticizer, data = training)
hist(log(training$Superplasticizer + 1))

#Question 3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

ILindex <- grep(pattern = "^IL", x = names(training))
preObj <- preProcess(training[, ILindex], method = "pca", thresh = .9)
preObj
preObj$numComp

#Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

ILindex <- grep(pattern = "^IL", x = names(training))
ILindex <- c(1, ILindex)
newTrain <- training[, ILindex]
newTest <- testing[, ILindex]
#glm
modelFit1 <- train(diagnosis ~ ., data = newTrain, method = "glm")
pred1 <- predict(modelFit1, newdata = newTest)
mean(pred1 == newTest$diagnosis)

#glm with pca
modelFit2 <- train(diagnosis ~ ., data = newTrain, preProcess = "pca", trControl = trainControl(preProcOptions = list(thresh = 0.8)), method = "glm")
pred2 <- predict(modelFit2, newdata = newTest)
mean(pred2 == newTest$diagnosis)