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
#filtering "NEI" to select baltimore city, then grouping it by "year", then summing "Emissions" for each year and
#storing them in "baltimoreYearlyEmissions" data frame
baltimoreYearlyEmissions <- filter(NEI, fips == "24510") %>%
    group_by(year) %>%
    summarize(totalEmission = sum(Emissions))
png("plot2.png")
plot(baltimoreYearlyEmissions, type = "b", xlab = "Year", ylab = "Total Emissions from PM2.5 (Tons)",
     main = "Total emissions from PM2.5 in Baltimore City for each year")
abline(lm(totalEmission ~ year, data = baltimoreYearlyEmissions), col = "red", lty = 2)
legend("bottomleft", legend = c("line", "lm"), col = c("black", "red"), lty = c(1, 2))
dev.off()
#we see that total emissions has overally decreased in Baltimore city over years. It decreased from 1999 to 2002,
#then increased in 2005 and again decreased in 2008.