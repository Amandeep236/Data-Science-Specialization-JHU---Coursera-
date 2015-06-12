#Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
#1. Subset the data to a training set and testing set based on the Case variable in the data set. 
inTrain <- createDataPartition(segmentationOriginal$Case, p = 3/4, list = FALSE)
training <- segmentationOriginal[inTrain, ]
testing <- segmentationOriginal[-inTrain, ]
#2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
set.seed(125)
#It seems that the question wants us to train model on the whole segmentationOriginal data set (not on training!)
modelCart <- train(Class ~ ., data = segmentationOriginal, method = "rpart")
# 3. In the final model what would be the final model prediction for cases with the following variable values:
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
modelCart$finalModel
plot(modelCart$finalModel, uniform = T)
text(modelCart$finalModel, pretty = T)

#Question 3
library(pgmm)
data(olive)
olive = olive[,-1]
modelOlive <- train(Area ~ ., data = olive, method = "rpart")
newdata = as.data.frame(t(colMeans(olive)))
predict(modelOlive, newdata = newdata)
# library(tree)
# fit.tree <- tree(Area ~ ., data = olive)
# predict(fit.tree, newdata = newdata)

#Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
fit.glm <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, family = "binomial")
pred.glm <- predict(fit.glm, newdata = testSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(fit.glm))
missClass(testSA$chd, pred.glm)
#or
set.seed(13234)
modelSA <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, method = "glm", family = "binomial")
missClass(trainSA$chd, predict(modelSA$finalModel))
missClass(testSA$chd, predict(modelSA$finalModel, newdata = testSA))

#Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modelVowel <- train(y ~ ., data = vowel.train, method = "rf")
varImp(modelVowel)