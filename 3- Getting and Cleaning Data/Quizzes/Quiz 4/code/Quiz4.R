#check & create "data" folder
if(!dir.exists("./data")){dir.create("./data")}
#Question 1
#downloading American Community Survey data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
codeBookURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
download.file(fileURL, destfile = "./data/ACSData.csv", method = "curl")
download.file(codeBookURL, destfile = "./data/ACSCodeBook.pdf", method = "curl")
downloadTime <- date()
acsData <- read.csv("./data/ACSData.csv")
str(acsData)
acsNamesSplit <- strsplit(x = names(acsData), split = "wgtp")
acsNamesSplit[[123]]

#Question 2
#downloading Gross Domestic Product data
fileURL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileURL2, destfile = "./data/gdpData.csv", method = "curl")
downloadTime2 <- date()
gdpData <- read.csv("./data//gdpData.csv", colClasses = "character")
str(gdpData)
gdpData <- gdpData[5:194, ]
str(gdpData)    #we need "X.3"
gdpData$X.3 <- as.integer(gsub(",", "", x = gdpData$X.3))
mean(gdpData$X.3)

#Question 3
library(dplyr)
gdpData <- rename(gdpData, countryNames = X.2)
grep(pattern = "^United", x = gdpData$countryNames)
gdpData$countryNames[c(1, 6, 32)]

#Question 4
#downloading educational data
fileURL3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileURL3, destfile = "./data/eduData.csv", method = "curl")
downloadTime3 <- date()
eduData <- read.csv("./data//eduData.csv", colClasses = "character")
str(eduData)    #we need "CountryCode"
matchedData <- merge(x = gdpData, y = eduData, by.x = "X", by.y = "CountryCode")
str(matchedData)
names(matchedData)
matchedDataFilter <- filter(matchedData, grepl(pattern = "Fiscal", x = Special.Notes))
matchedDataFilter <- select(matchedDataFilter, Special.Notes)
sum(grepl(pattern = "end: June", x = matchedDataFilter$Special.Notes))

#Question 5
#download data on Amazon's stock price and get the times the data was sampled
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 
library(lubridate)
sum(year(sampleTimes) == 2012)
sum((year(sampleTimes) == 2012) & (wday(sampleTimes, label = T) == "Mon"))