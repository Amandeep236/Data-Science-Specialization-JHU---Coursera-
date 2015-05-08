#checking to see if the data folder exists & create it if it doesn't exist
if(!dir.exists("./data")){dir.create("./data")}
#Question 1
#downloading American Community Survey data & code book
file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(file, destfile = "./data/American-Community-Surver.csv", method = "curl")
ACSdownloadTime <- date()
codeBookFile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
download.file(codeBookFile, destfile = "./data/ACSCodeBook.pdf", method = "curl")
#reading American Community Survey data
ACSData <- read.csv(file = "./data/American-Community-Surver.csv")
str(ACSData)
#removing rows with VAL = NA and subseting the ones with VAL = 24 (worth 1000000+)
worth1m <- ACSData[!is.na(ACSData$VAL), ]
worth1m <- worth1m[worth1m$VAL == 24, ]
nrow(worth1m)

#Question 3
#downloading Natural Gas Aquisition data
file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(file, destfile = "./data/Natural-Gas-Aquisition.xlsx", method = "curl")
NGAdownloadTime <- date()
library(xlsx)
dat <- read.xlsx(file = "./data//Natural-Gas-Aquisition.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)
sum(dat$Zip*dat$Ext,na.rm=T) 

#Question 4
#reading Baltimore restaurants data
library(XML)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileURL, destfile = "./data/Baltimore-restaurants.xml", method = "curl")
doc <- xmlTreeParse("./data/Baltimore-restaurants.xml", useInternal = T)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]][[1]]
sum(xpathSApply(doc, path = "//zipcode", xmlValue) == 21231)

#Question 5
#reading American Community Survey data
file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(file, destfile = "./data/American-Community-Survey-2.csv", method = "curl")
ACSdownloadTime2 <- date()
library(data.table)
DT <- fread(input = "./data/American-Community-Survey-2.csv", sep = ",", header = TRUE)
any(is.na(DT$pwgtp15))
#examining different options of question
#option 1: mean doesn't calculate mean of pwgtp15 broken down by sex
mean(DT$pwgtp15,by=DT$SEX)
mean(DT$pwgtp15)    #it's equivalent to the above mean function!
#option 2: it doesn't use data.table package and also is very slow
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
option2Time <- system.time(expr = {mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)})
#option 3: rowMeans doesn't calculate means of column pwgtp15
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
#option 4:it seems good!
DT[,mean(pwgtp15),by=SEX]
option4Time <- system.time(DT[,mean(pwgtp15),by=SEX])
#option 5:it doesn't use data.table package but is fast like option 4
sapply(split(DT$pwgtp15,DT$SEX),mean)
option5Time <- system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
#option 6:it doesn't use data.table package and is slightly slower than options 4 & 5
tapply(DT$pwgtp15,DT$SEX,mean)
option6Time <- system.time(tapply(DT$pwgtp15,DT$SEX,mean))