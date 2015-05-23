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