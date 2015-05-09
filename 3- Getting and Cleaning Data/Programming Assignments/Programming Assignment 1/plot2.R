#reading data of specified days and adding colnames to it. specified dates are in rows 66637 to 69516.
houseData <- read.table(file = "./data/household_power_consumption.txt", header = T, sep = ";", skip = 66636, nrows = 2880, na.strings = "?")
headerD <- read.table(file = "./data/household_power_consumption.txt", header = T, sep = ";", nrows = 1)
colnames(houseData) <- colnames(headerD)
rm(headerD)
#changing format of Date & Time columns to Date & POSIXlt. 
#Date is 1/2/2007 or 2/2/2007, so it's pattern is "%d/%m/%Y".
houseData$Date <- as.Date(houseData$Date, format = "%d/%m/%Y")
dateTime <- paste(houseData$Date, houseData$Time, sep = " ")
#now Date is formatted as %Y-%m-%d, and dateTime pattern is "%Y-%m-%d %H:%M:%S".
houseData$Time <- strptime(x = dateTime, format = "%Y-%m-%d %H:%M:%S")

#plot 1
png("./plot2.png", width = 480, height = 480, bg = "transparent")
with(houseData, plot(Time, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))
dev.off()