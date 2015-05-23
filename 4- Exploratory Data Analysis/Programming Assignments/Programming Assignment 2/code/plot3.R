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
library(ggplot2)
#filtering "NEI" to select baltimore city, then grouping it by "type" and "year", then summing "Emissions" for each group
#and storing them in "baltimoreTypesYearlyEmissions" data frame
baltimoreTypesYearlyEmissions <- filter(NEI, fips == "24510") %>%
    group_by(type, year) %>%
    summarize(totalEmission = sum(Emissions))
png("plot3.png")
qplot(x = year, y = totalEmission, data = baltimoreTypesYearlyEmissions, facets = .~type, geom = c("point", "smooth"), method = "lm") +
      labs(y = "Total Emission from PM2.5 (Tons)", x = "Year", title = "Total emission from PM2.5 in Baltimore City for each type")
dev.off()
#we that total emissions in Baltimore city has decreased over the years for "NON-ROAD", "NONPOINT" and "ON-ROAD" types,
#but it has increased for "POINT".