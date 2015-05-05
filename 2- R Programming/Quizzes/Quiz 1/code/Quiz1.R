#Loading Quiz1 Data Set
data <- read.csv(file = "./data/hw1_data.csv")

#Question 11
colnames(data)

#Question 12
data[1:2, ]

#Question 13
nrow(data)

#Question 14
data[152:153, ]

#Question 15
data[47, 1]

#Question 16
sum(is.na(data[, 1]))

#Question 17
badOzone <- is.na(data[, 1])
mean(data[!badOzone, 1])

#Question 18
subData <- data[(data$Ozone>31 & data$Temp>90), 2]
mean(subData, na.rm = T)

#Question 19
mean(data[data$Month == 6, 4], na.rm = T)

#Question 20
max(data[data$Month == 5, 1], na.rm = T)