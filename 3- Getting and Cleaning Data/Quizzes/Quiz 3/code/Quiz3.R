#check & create data folder
if(!dir.exists("./data")){dir.create("./data")}
#Question 1
#downloading American Community Survey data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
codeBookURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
download.file(fileURL, destfile = "./data/American-Community-Survey.csv", method = "curl")
download.file(codeBookURL, destfile = "./data/ACSCodeBook.pdf", method = "curl")
downloadTime <- date()
#reading American Community Survey data
acsData <- read.csv("./data//American-Community-Survey.csv")
str(acsData)
names(acsData)  #we need "ACR = 3" and "AGS = 6"
agricultureLogical <- (acsData$ACR == 3) & (acsData$AGS == 6)
which(agricultureLogical)
acsData[c(125, 238, 262), c("ACR", "AGS")]

#Question 2
fileURL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileURL2, destfile = "./data/Fjeff.jpg", method = "curl")
downloadTime2 <- date()
library(jpeg)
?readJPEG
fjeffPic <- readJPEG(source = "./data//Fjeff.jpg", native = TRUE)
head(fjeffPic)
quantile(fjeffPic, probs = c(.3, .8))

#Question 3
#downloading Gross Domestic Product data
fileURL3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileURL3, destfile = "./data/GDPdata.csv", method = "curl")
#downloading educational data
fileURL4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileURL4, destfile = "./data/educational-data.csv", method = "curl")
downloadTime3 <- date()
gdpData <- read.csv("./data//GDPdata.csv", colClasses = "character")
eduData <- read.csv("./data//educational-data.csv", colClasses = "character")
str(gdpData)    #we need "X"
str(eduData)    #we need "CountryCode"
library(dplyr)
gdpData <- gdpData[5:194, ]
gdpData <- mutate(gdpData, Gross.domestic.product.2012 = as.integer(gdpData$Gross.domestic.product.2012))
str(gdpData)
length(intersect(gdpData$X, eduData$CountryCode))
gdpDataSorted <- arrange(gdpData, desc(Gross.domestic.product.2012))
gdpDataSorted[13, 4]

#Question 4
mergedData <- merge(x = gdpData, y = eduData, by.x = "X", by.y = "CountryCode")
str(mergedData$Income.Group)
mergedData <- group_by(mergedData, Income.Group)
summarise(mergedData, averageGDP = mean(Gross.domestic.product.2012))

#Question 5
library(Hmisc)
table(mergedData$Income.Group, cut2(mergedData$Gross.domestic.product.2012, g = 5))