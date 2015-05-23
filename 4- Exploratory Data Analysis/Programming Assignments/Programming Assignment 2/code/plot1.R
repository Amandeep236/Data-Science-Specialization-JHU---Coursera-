#downloading National Emissions Inventory data
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileURL, destfile = "./NEI-Data.zip", method = "curl")
downloadTime <- date()
#unzip National Emissions Inventory zip file and extract its files to working directory
unzip("./NEI-Data.zip", junkpaths = T)

#reading PM2.5 Emissions Data and Source Classification Code Table
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

library(dplyr)
#grouping "NEI" by "year", then summing "Emissions" for each year and storing them in "yearlyEmissions" data frame
yearlyEmissions <- group_by(NEI, year) %>%
    summarize(totalEmission = sum(Emissions))
png("plot1.png")
plot(yearlyEmissions, type = "b", xlab = "Year", ylab = "Total Emissions from PM2.5 (Tons)",
     main = "Total emissions from PM2.5 in US for each year")
abline(lm(totalEmission ~ year, data = yearlyEmissions), col = "red", lty = 2)
legend("topright", legend = c("line", "lm"), col = c("black", "red"), lty = c(1, 2))
dev.off()
#we see that total emissions has decreased in US over years