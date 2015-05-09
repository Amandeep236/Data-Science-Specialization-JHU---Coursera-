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

#plot4
png("./plot4.png", width = 480, height = 480, bg = "transparent")
#setting mfrow for a 2x2 layout
par(mfrow = c(2, 2))
with(houseData, {
    #(1, 1)
    plot(Time, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
    #(2, 1)
    plot(Time, Voltage, type = "l", xlab = "datetime")
    #(1, 2)
    plot(Time, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
    lines(Time, Sub_metering_2, col = "red")
    lines(Time, Sub_metering_3, col = "blue")
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"),
    lty = "solid", bty = "n")
    #(2, 2)
    plot(Time, Global_reactive_power, type = "l", xlab = "datetime")
})
dev.off()
#reseting mfrow, so that next plots will not be affected by this plot's settings.
par(mfrow = c(1, 1))