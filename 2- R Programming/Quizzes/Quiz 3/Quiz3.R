library(datasets)
#loading iris data set for question 1 & 2
data(iris)
?iris
str(iris)

#Question 1
tapply(X = iris$Sepal.Length, INDEX = iris$Species, FUN = mean)
#or
mean(iris[iris$Species == "virginica", "Sepal.Length"])

#Question 2
apply(X = iris[, 1:4], MARGIN = 2, FUN = mean)

#loading mtcars data set for question 3
data(mtcars)
?mtcars
str(mtcars)

#Question 3
with(mtcars, tapply(mpg, cyl, mean))

#Question 4
with(mtcars, abs(mean(mtcars[cyl == 4, "hp"]) - mean(mtcars[cyl == 8, "hp"])))
#or
hpMeans <- with(mtcars, sapply(X = split(hp, cyl), FUN = mean))
abs(hpMeans[1] - hpMeans[3])

#Question 5
debug(ls)
ls