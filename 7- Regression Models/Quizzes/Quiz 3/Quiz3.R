#Question 1
data(mtcars)
mtcars$cylFac <- as.factor(mtcars$cyl)
fit <- lm(mpg ~ cylFac + wt, data = mtcars)
summary(fit)

#Question 2
fit2 <- lm(mpg ~ cylFac, data = mtcars)
summary(fit)
summary(fit2)

#Question 3
fit3 <- lm(mpg ~ cylFac*wt, data = mtcars)
summary(fit)
summary(fit3)
library(lmtest)
lrtest(fit, fit3)

#Question 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
max(hatvalues(fit))

#Question 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
hatvalues(fit)
dfbetas(fit)