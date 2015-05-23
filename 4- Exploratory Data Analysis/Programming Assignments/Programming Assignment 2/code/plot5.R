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
#after reading a lot about motor vehicles (in NEI website's documentations, wikipedia & ...), i figured that type = "ON-ROAD"
#would be good enough for this explotary analysis, as it contains cars, busses, truck, motorcycles... . There are also other
#opinions about motor vehicles and you can filter based them and store them in "onraodNEI" and the rest of the code will
#work with it.
#filtering "NEI" to select baltimore city and "ON-ROAD" type, then grouping it by "year", then summing "Emissions" for each group
#and storing them in "onroadNEI" data frame
onroadNEI <- filter(NEI, type == "ON-ROAD", fips == "24510") %>%
    group_by(year) %>%
    summarize(totalEmission = sum(Emissions))
png("plot5.png")
qplot(x = year, y = totalEmission, data = onroadNEI, geom = c("point", "smooth"), method = "lm") +
    labs(x = "Year", y = "Total Emission from PM2.5 (Tons)", title = "Total PM2.5 of motor vehicles in Baltimore City")
dev.off()
#we can see that total emissions from motor vehicles has decreased in Baltimore City over years overally.
#in fact it decreased alot from 1999 to 2002 and then decreased a little from 2002 to 2008.