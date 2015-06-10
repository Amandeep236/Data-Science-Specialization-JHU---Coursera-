#Question 1
library(MASS)
data(shuttle)
?shuttle
str(shuttle)
levels(shuttle$use)
shuttle$use <- relevel(shuttle$use, ref = "noauto")
levels(shuttle$use)
fit <- glm(use ~ wind, data = shuttle, family = "binomial")
summary(fit)
#log(p/1-p) = B0 + B1 * windtail => p/1-p = exp(B0 + B1 * windtail)
exp(0.25131)/exp(0.25131+0.03181)
exp(-0.03181)

#Question 2
fit2 <- glm(use ~ wind + magn, data = shuttle, family = "binomial")
summary(fit2)
exp(-3.201e-02)

#Question 3
#log(p/1-p) = log(p) - log(1-p) = B0 + B1 * X1
#log(1-p/p) = log(1-p) - log(p) = -B0 - B1 * X1

#Question 4
data(InsectSprays)
?InsectSprays
str(InsectSprays)
fit3 <- glm(count ~ spray, data = InsectSprays, family = "poisson")
summary(fit3)
exp(-0.05588)

#Question 6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots <- 0
splineTerm <- sapply(knots, function(knot){(x > knot) * (x - knot)})
xMat <- c(1, x, splineTerm)
yhat <- predict(lm(y ~ x + splineTerm))
plot(x, yhat)
(yhat[7]-yhat[6])/(x[7]-x[6])