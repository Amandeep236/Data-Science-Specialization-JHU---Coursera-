#Question 1 & 2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)

#Question 3
data(mtcars)
fit <- lm(mpg ~ wt, data = mtcars)
summary(fit)
predict(fit, newdata = data.frame(wt = mean(mtcars$wt)), interval = "confidence")

#Question 5
predict(fit, newdata = data.frame(wt = 3), interval = "prediction")

#Question 6
mtcars$short <- mtcars$wt / 2
fit <- lm(mpg ~ short, data = mtcars)
summary(fit)
summary(fit)$coef[2, 1] + qt(.975, df = length(mtcars$mpg)-1, lower.tail = FALSE) * summary(fit)$coef[2, 2]

#Question 9
fit <- lm(mpg ~ wt, data = mtcars)
sum((mtcars$mpg - fit$fitted.values)^2)/sum((mean(mtcars$mpg) - mtcars$mpg)^2)
