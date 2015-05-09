#checking & creating data folder
if(!dir.exists("./data")){dir.create("./data")}
#downloading Individual household electric power consumption Data Set
file <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(file, destfile = "./data/household electric power.zip", method = "curl", mode = "wb")
downloadTime <- date()
#unzip the downloaded file & delete the zip file
unzip(zipfile = "./data//household electric power.zip", files = "household_power_consumption.txt", exdir = "./data")
file.remove("./data//household electric power.zip")
#experimenting for reading the file.
#calculating an estimate of memory needed for this data set
2075259*9*8/(2^20)
#it would be approximately 150mb & will need 300mb of RAM.
data <- read.table(file = "./data//household_power_consumption.txt", header = T, sep = ";", na.strings = "?")
#data$Date <- as.Date(data$Date, format = "%d/%m/%Y") - it is not required yet.
#data$Time <- strptime(x = data$Time, format = "%H:%M:%S") - it is not required yet.
#finding first row, last row & number of rows corresponding to dates "1/2/2007" & "2/2/2007". we will use them for
#reading data of just these days.
firstRow <- min(which(data$Date == "1/2/2007")) #66637
lastRow <- max(which(data$Date == "2/2/2007"))  #69516
nrows <- lastRow - firstRow     #2879
rm(data, firstRow, lastRow, nrows)
#reading data of specified days and adding colnames to it.
data2 <- read.table(file = "./data//household_power_consumption.txt", header = T, sep = ";", skip = 66636, nrows = 2880, na.strings = "?")
headerD <- read.table(file = "./data//household_power_consumption.txt", header = T, sep = ";", nrows = 1)
colnames(data2) <- colnames(headerD)
rm(headerD)

#changing format of Date & Time columns to Date & POSIXlt
data2$Date <- as.Date(data2$Date, format = "%d/%m/%Y")
dateTime <- paste(data2$Date, data2$Time, sep = " ")
data2$Time <- strptime(x = dateTime, format = "%Y-%m-%d %H:%M:%S")