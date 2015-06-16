set.seed(1)
lambda = 0.2
nsim = 1000
n = 40
#hist(rexp(nsim, lambda))
mns <- NULL
for(i in 1:nsim){
    mns[i] <- mean(rexp(n, lambda))
}

library(ggplot2)

#Part 1
theoMean <- 1/lambda
simMean <- mean(mns)
theoMean
simMean
#ploting
plotData <- data.frame(mns)
plot1 <- ggplot(data = plotData, aes(x = mns))
plot1 <- plot1 + geom_histogram(aes(y = ..density..), fill = "blue", color = "black")
plot1 <- plot1 + geom_line(aes(x = c(simMean, simMean), y = c(0, .6), col = "Simulation Mean"))
plot1 <- plot1 + geom_line(aes(x = c(theoMean, theoMean), y = c(0, .6), col = "Theoretical Mean"))
plot1 <- plot1 + guides(col = guide_legend(title = NULL)) #removing legent title
plot1 <- plot1 + labs(title = "Distribution of Means of rexp")
plot1

#hist(mns)
#theoretical mean is 1/lambda = 1/.2=5
#abline(v = mean(mns), col = "blue", lwd = 2)
#abline(v = 1/lambda, col = "red", lwd = 2)

#part 2
theoVar <- 1/((lambda^2)*n)
simVar <- var(mns)
theoVar
simVar

theoSd <- sqrt(theoVar)
simSd <- sqrt(simVar)

plot2 <- ggplot(data = plotData, aes(x = mns))
plot2 <- plot2 + geom_histogram(aes(y = ..density..), fill = "blue", color = "black")
plot2 <- plot2 + geom_line(aes(x = c(simMean+2*simSd, simMean+2*simSd), y = c(0, .6), col = "Simulation sd"))
plot2 <- plot2 + geom_line(aes(x = c(simMean-2*simSd, simMean-2*simSd), y = c(0, .6), col = "Simulation sd"))
plot2 <- plot2 + geom_line(aes(x = c(theoMean+2*theoSd, theoMean+2*theoSd), y = c(0, .6), col = "Theoretical sd"))
plot2 <- plot2 + geom_line(aes(x = c(theoMean-2*theoSd, theoMean-2*theoSd), y = c(0, .6), col = "Theoretical sd"))
plot2 <- plot2 + guides(col = guide_legend(title = NULL)) #removing legent title
plot2 <- plot2 + labs(title = "Distribution of Means of rexp")
plot2

#part 3
theoSd <- sqrt(theoVar)
simSd <- sqrt(simVar)
#normalized <- (mns-5)/sqrt(var(mns)*n)
# hist(normalized)
# lines(density(rnorm(nsim)), col = "red")
plot3 <- ggplot(plotData, aes(x = mns))
plot3 <- plot3 + geom_histogram(aes(y = ..density..), fill = "blue", color = "black")
plot3 <- plot3 + stat_function(fun = dnorm, args = list(mean = theoMean, sd = theoSd), color = "red", size = 1)
plot3 <- plot3 + labs(title = "Distribution of Means of rexp")
plot3
#qqplot
qqnorm(mns, main = "Normal Q-Q Plot for Means of rexp")
qqline(mns, col = "red")
