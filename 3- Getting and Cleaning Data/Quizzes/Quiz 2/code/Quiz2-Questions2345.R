#check to see if data folder doesn't exist
if(!dir.exists("./data")){dir.create("./data")}
#downloading American Community Survey data
file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(file, destfile = "./data/American-Community-Survey.csv", method = "curl")
downloadTime <- date()
acs <- read.csv("./data/American-Community-Survey.csv")
library(sqldf)
str(acs)

#Question 2
sqldf("select pwgtp1 from acs where AGEP < 50")

#Question 3
sqldf("select distinct AGEP from acs")

#Question 4
htmlCode <- readLines(con = "http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode
htmlChars <- c(nchar(htmlCode[10]), nchar(htmlCode[20]), nchar(htmlCode[30]), nchar(htmlCode[100]))
htmlChars

#Question 5
file2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for "
download.file(file2, destfile = "./data/getdata-wksst8110.for", method = "curl")
downloadTime2 <- date()
sstData <- read.fwf(file = "./data//getdata-wksst8110.for", widths = c(12, 7, 4, 9, 4, 9, 4, 9, 4), skip = 4)
str(sstData)
sum(sstData$V4)