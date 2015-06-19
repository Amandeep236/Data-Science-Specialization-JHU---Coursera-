#loading mtcars dataset
data(mtcars)
#looking at structure
head(mtcars)
str(mtcars)
#changing variable types
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("automatic", "manual")
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)
#looking at summary
summary(mtcars)
#exploratory plots
plot(mpg ~ am, data = mtcars)
#fitting models
#fitting with am as the only predictor
fit.lm1 <- lm(mpg ~ am, data = mtcars)
summary(fit.lm1)
#ploting residuals for fit.lm1
par(mfrow = c(2, 2))
plot(fit.lm1)
par(mfrow = c(1, 1))
#tapply(mtcars$mpg, INDEX = mtcars$am, FUN = mean)
#investigating the effect of am on mpg using t.test
t.test(mpg ~ am, data = mtcars)
#selecting the best model
fit.lmFull <- lm(mpg ~ ., data = mtcars)
summary(fit.lmFull)
selectedModel <- step(object = fit.lmFull)
summary(selectedModel)
selectedModel$anova
#using leaps package
library(leaps)
regfit.full <- regsubsets(mpg ~ ., data = mtcars, nvmax = 16)
reg.summary <- summary(regfit.full)
which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, type = "b")
points(5, reg.summary$adjr2[5], col = "red", pch = 19)
reg.summary$adjr2[5]
#plot(regfit.full, scale = "adjr2")
coef(regfit.full, 5)
fit.lm2 <- lm(mpg ~ I(cyl == 6) + hp + wt + vs + am, data = mtcars)
summary(fit.lm2)$coef
plot(fit.lm2$fitted.values, fit.lm2$residuals)
abline(h = 0, col = "red", lwd = 2)
