#check and create "data" folder
if(!dir.exists("./data/")){dir.create("./data/")}
#downloading National Emissions Inventory data
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileURL, destfile = "./data/NEI-Data.zip", method = "curl")
downloadTime <- date()
unzip("./data/NEI-Data.zip", junkpaths = T, exdir = "./data/")
#reading PM2.5 Emissions Data and Source Classification Code Table
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data//Source_Classification_Code.rds")

#summaries of data
str(NEI)
summary(NEI)
str(SCC)
summary(SCC)

#plot 1
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

#plot 2
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

#plot 3
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

#plot 4
library(dplyr)
library(ggplot2)
#searching to see if "Coal" is in different columns of SCC (I have searched for "coal" too!)
sapply(SCC, FUN = function(x){grep("Coal", x)})
#for this question, records with "Coal" in "EI.Sector" seem pretty good to me, but you can select with different columns
#and the rest of the code will work, if you store selected records in "coalIndex".
coalIndex <- grep("Coal", SCC$EI.Sector)
#selecting SCC numbers for "coalIndex" and storing them in "SCCIndex"
SCCIndex <- as.numeric(as.character(SCC[coalIndex, 1]))
#filtering "NEI" to select "SCCIndex", then grouping it by "year", then summing "Emissions" for each group
#and storing them in "coalNEIYearlyEmissions" data frame
coalNEIYearlyEmissions <- filter(NEI, SCC %in% SCCIndex) %>%
    group_by(year) %>%
    summarize(totalEmission = sum(Emissions))
png("plot4.png")
qplot(x = year, y = totalEmission, data = coalNEIYearlyEmissions, geom = c("point", "smooth"), method = "lm") +
    labs(x = "Year", y = "Total Emission from PM2.5 (Tons)", title = "Total PM2.5 of coal combustion-related sources in US")
dev.off()
#we can see that total emissions from coal combustion-related sources has decreased in US over years overally.
#in fact it decreased a little from 1999 to 2005 and then decreased significantly from 2005 to 2008.

#plot 5
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

#plot 6
library(dplyr)
library(ggplot2)
#after reading a lot about motor vehicles (in NEI website's documentations, wikipedia & ...), i figured that type = "ON-ROAD"
#would be good enough for this explotary analysis, as it contains cars, busses, truck, motorcycles... . There are also other
#opinions about motor vehicles and you can filter based them and store them in "onroadNEICities" and the rest of the code
#will work with it.
#filtering "NEI" to select baltimore city or Los Angeles County, and "ON-ROAD" type, then grouping it by "year" and city,
#then summing "Emissions" for each group and storing them in "onroadNEICities" data frame
onroadNEICities <- filter(NEI, type == "ON-ROAD", fips == "24510" | fips == "06037") %>%
    group_by(year, fips) %>%
    summarize(totalEmission = sum(Emissions))
#changing "fips" factor levels, so that qplot facets will show correct names for them (instead of their fips ids)
onroadNEICities$fips <- factor(onroadNEICities$fips, labels = c("Los Angeles County", "Baltimore City"))
png("plot6.png")
qplot(x = year, y = totalEmission, data = onroadNEICities, facets = .~fips, geom = c("point", "smooth"), method = "lm") +
    labs(x = "Year", y = "Total Emission from PM2.5 (Tons)", title = "Total PM2.5 of motor vehicles in Baltimore vs LA")
dev.off()
#we can see that total emissions from motor vehicles in LA are a lot more that its valu in Baltimore city.
#this value in Baltimore city has decreased, but its change is far less than its change in LA.
#this value in LA has increased a lot from 1999 to 2005, and then decreased a lot from 2005 to 2008.