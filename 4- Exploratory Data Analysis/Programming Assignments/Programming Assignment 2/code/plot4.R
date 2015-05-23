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