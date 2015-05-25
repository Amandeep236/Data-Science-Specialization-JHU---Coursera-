#check and create data folder
if(!dir.exists("./data")){dir.create("./data")}
#check to see if "repdatadataStormData.csv.bz2" exist, and if not, dowload it
if(!file.exists("./data//repdatadataStormData.csv.bz2")){
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileURL, destfile = "./data/repdatadataStormData.csv.bz2", method = "curl")
    downloadTime <- date()
}
#reading Storm Data
stormData <- read.csv("./data/repdatadataStormData.csv.bz2")

head(stormData)
tail(stormData)

#summaries
dim(stormData)
str(stormData)
summary(stormData)

# #cleaning up some of the time zones
# stormData$TIME_ZONE <- toupper(stormData$TIME_ZONE)
# stormData$TIME_ZONE <- gsub("ESY", "EST", stormData$TIME_ZONE)
# stormData$TIME_ZONE <- gsub("CSC", "CST", stormData$TIME_ZONE)
# 
# #changing format of date and adding BGN_YEAR
# stormData$BGN_DATE <- as.Date(stormData$BGN_DATE, tz = stormData$TIME_ZONE, format = "%m/%d/%Y %H:%M:%S")
# stormData$BGN_YEAR <- as.POSIXlt(stormData$BGN_DATE2)$year

#exploring EVTYPE and cleaning it to comply with 48 groups mentioned in document
library(dplyr)
#stormData$EVTYPE2 <- stormData$EVTYPE
View(stormData %>% group_by(EVTYPE) %>% summarize(n = n()) %>% arrange(EVTYPE))
View(stormData %>% group_by(EVTYPE) %>% summarize(n = n()) %>% arrange(desc(n)))
#changing the names to upper letters and removing starting spaces
stormData <- mutate(stormData, EVTYPE = toupper(EVTYPE))
stormData$EVTYPE <- sub("^ +", "", x = stormData$EVTYPE)
#changing events for HAIL
stormData$EVTYPE <- gsub("^HAIL.+|SMALL HAIL", "HAIL", x = stormData$EVTYPE)
#changing events for THUNDERSTORM
stormData$EVTYPE <- gsub("^TSTM", "THUNDERSTORM", x = stormData$EVTYPE)
stormData$EVTYPE <- gsub("^THU.+|.*MICROBURST.*", "THUNDERSTORM WIND", x = stormData$EVTYPE)
stormData$EVTYPE <- gsub("TUNDERSTORM WIND", "THUNDERSTORM WIND", x = stormData$EVTYPE)
#changing events for TORNADO
stormData$EVTYPE <- gsub("^TORN.+", "TORNADO", x = stormData$EVTYPE)
#changing events for FLASH FLOOD
stormData$EVTYPE <- gsub("^FLASH.+", "FLASH FLOOD", x = stormData$EVTYPE)
#changing events for FLOOD
stormData$EVTYPE <- gsub("^FLOOD.+|^RIVER.+", "FLOOD", x = stormData$EVTYPE)
#changing events for HIGH WIND
stormData$EVTYPE <- gsub("^HIGH.+WIND.+", "HIGH WIND", x = stormData$EVTYPE)
#changing events for LIGHTNING
stormData$EVTYPE <- gsub("^LIGHTNING.+|LIGHTING", "LIGHTNING", x = stormData$EVTYPE)
#changing events for HEAVY SNOW
stormData$EVTYPE <- gsub("^HEAVY SNOW.+|HEAVY WET SNOW|HEAVY MIX|^SNOW.*|^EXCESSIVE SNOW.*", "HEAVY SNOW", x = stormData$EVTYPE)
#changing events for HEAVY RAIN
stormData$EVTYPE <- gsub("^HEAVY RAIN.+|^HEAVY PRECIP.+|^HEAVY SHOWER.*", "HEAVY RAIN", x = stormData$EVTYPE)
#changing events for WINTER STORM
stormData$EVTYPE <- gsub("^WINTER STORM.+|^FREEZING RAIN.*|^FREEZING DRIZZLE", "WINTER STORM", x = stormData$EVTYPE)
#changing events for WINTER WEATHER
stormData$EVTYPE <- gsub("^WINTER WEATHER.+|^WINT.+MIX|^LIGHT SNOW.*|^MODERATE SNOW.*", "WINTER WEATHER", x = stormData$EVTYPE)
#changing events for FUNNEL CLOUD
stormData$EVTYPE <- gsub(".*FUNNEL.*|.*CLOUD.*", "FUNNEL CLOUD", x = stormData$EVTYPE)
#changing TSTM in events to THUNDERSTORM
stormData$EVTYPE <- gsub("TSTM", "THUNDERSTORM", x = stormData$EVTYPE)
#changing events for WATERSPOUT
stormData$EVTYPE <- gsub("^WA.*TER.+", "WATERSPOUT", x = stormData$EVTYPE)
#changing events for STRONG WIND
stormData$EVTYPE <- gsub("^STRONG WIND.+|^WIND|^WIND [ADGS].+|^GUSTY WIND.*", "STRONG WIND", x = stormData$EVTYPE)
#changing events of URBAN... (small floods) to HEAVY RAIN
stormData$EVTYPE <- gsub("^URBAN.+|^SMALL STREAM.*|^STREET FLOOD.*", "HEAVY RAIN", x = stormData$EVTYPE)
#changing events for WILDFIRE
stormData$EVTYPE <- gsub("^WILD.+", "WILDFIRE", x = stormData$EVTYPE)
#changing events for BLIZZARD
stormData$EVTYPE <- gsub("^BLIZZARD.+", "BLIZZARD", x = stormData$EVTYPE)
#changing events for DROUGHT
stormData$EVTYPE <- gsub("^DROUGHT.+", "DROUGHT", x = stormData$EVTYPE)
#changing events for ICE STORM
stormData$EVTYPE <- gsub("^ICE.*", "ICE STORM", x = stormData$EVTYPE)
#changing events for EXCESSIVE HEAT
stormData$EVTYPE <- gsub("EXTREME HEAT|^EXCESSIVE HEAT.+", "EXCESSIVE HEAT", x = stormData$EVTYPE)
#changing events for HIGH SURF
stormData$EVTYPE <- gsub(".*SURF.*|^HIGH WA|^HIGH TIDES|^HIGH.+SWELLS", "HIGH SURF", x = stormData$EVTYPE)
#changing events for FROST/FREEZE
stormData$EVTYPE <- gsub(".*FROST.*|^FREEZE", "FROST/FREEZE", x = stormData$EVTYPE)
#changing events for DENSE FOG
stormData$EVTYPE <- gsub("^FOG.*", "DENSE FOG", x = stormData$EVTYPE)
#changing events for EXTREME COLD/WIND CHILL
stormData$EVTYPE <- gsub("^EXTREME WIND.+|^EXTREME.* COLD.*|EXCESSIVE COLD", "EXTREME COLD/WIND CHILL", x = stormData$EVTYPE)
#changing events for HEAT
stormData$EVTYPE <- gsub("^HEAT.+|^UNSEASONABLY WARM|^UNSEASONABLY HOT|^UNUSUAL.*WARM|^RECORD HEAT.*|^RECORD WARM.*", "HEAT", x = stormData$EVTYPE)
#changing events for TROPICAL STORM
stormData$EVTYPE <- gsub("^TROPICAL STORM.+", "TROPICAL STORM", x = stormData$EVTYPE)
#changing events for COASTAL FLOOD
stormData$EVTYPE <- gsub("^COASTAL.*FLOOD.*|^COASTAL.*STORM", "COASTAL FLOOD", x = stormData$EVTYPE)
#changing events for LAKE-EFFECT SNOW
stormData$EVTYPE <- gsub(".*LAKE.* SNOW.*", "LAKE-EFFECT SNOW", x = stormData$EVTYPE)
#changing events for LANDSLIDE (Debris Flow)
stormData$EVTYPE <- gsub("^LAND.+", "LANDSLIDE", x = stormData$EVTYPE)
#changing events for COLD/WIND CHILL
stormData$EVTYPE <- gsub("^COLD.*|^COOL.*|^WIND CHILL.*|^RECORD.*COLD.*|.*LOW.*TEMP.*", "COLD/WIND CHILL", x = stormData$EVTYPE)
#changing events for RIP CURRENT
stormData$EVTYPE <- gsub("RIP CURRENTS", "RIP CURRENT", x = stormData$EVTYPE)
#changing events for STORM SURGE/TIDE
stormData$EVTYPE <- gsub("^STORM SURGE.*", "STORM SURGE/TIDE", x = stormData$EVTYPE)
#changing events for ASTRONOMICAL LOW TIDE
stormData$EVTYPE <- gsub(".*ASTRONOMICAL.*", "ASTRONOMICAL LOW TIDE", x = stormData$EVTYPE)
#changing events for HURRICANE (Typhoon)
stormData$EVTYPE <- gsub("^HURRICANE.*|^TYPHOON", "HURRICANE/TYPHOON", x = stormData$EVTYPE)
#changing events for SLEET
stormData$EVTYPE <- gsub("^SLEET.*", "SLEET", x = stormData$EVTYPE)
#changing events for FREEZING FOG
stormData$EVTYPE <- gsub("^GLAZE.*", "FREEZING FOG", x = stormData$EVTYPE)
#viewing EVTYPE numbers!
View(stormData %>% group_by(EVTYPE) %>% summarize(n = n()) %>% arrange(EVTYPE))
View(stormData %>% group_by(EVTYPE) %>% summarize(n = n()) %>% arrange(desc(n)))

#calculating percentage of cleaned data for EVTYPE
a <- stormData %>% group_by(EVTYPE) %>% summarize(n = n()) %>% arrange(desc(n))
sum(a[1:42, 2])/sum(a[, 2])
#Question 1
q1StormData <- group_by(stormData, EVTYPE) %>% summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES), population = sum(FATALITIES*3 + INJURIES)) %>% arrange(desc(population))
#about 86 percentage of harm to population health is for first 7 event types
sum(q1StormData$population[1:7])/sum(q1StormData$population)
barplot(q1StormData$population[1:7], names.arg = q1StormData$EVTYPE[1:7], cex.names = 0.7)
#as percentage
barplot(q1StormData$population[1:7]/sum(q1StormData$population), names.arg = q1StormData$EVTYPE[1:7], cex.names = 0.7)

#Question 2
#stormData2 <- stormData
stormData$PROPDMGEXP <- as.character(stormData$PROPDMGEXP)
stormData$CROPDMGEXP <- as.character(stormData$CROPDMGEXP)
#changing unknown exponential multiplires to the common one, which is "K"
stormData[stormData$PROPDMGEXP %in% c("", "-", "?", "+"), "PROPDMGEXP"] <- "K"
stormData[stormData$CROPDMGEXP %in% c("", "?"), "CROPDMGEXP"] <- "K"
#changing abbreviations of exponential multipliers to numbers
stormData[stormData$PROPDMGEXP %in% c("h", "H"), "PROPDMGEXP"] <- "2"
stormData[stormData$PROPDMGEXP == "K", "PROPDMGEXP"] <- "3"
stormData[stormData$PROPDMGEXP %in% c("m", "M"), "PROPDMGEXP"] <- "6"
stormData[stormData$PROPDMGEXP == "B", "PROPDMGEXP"] <- "9"

stormData[stormData$CROPDMGEXP %in% c("k", "K"), "CROPDMGEXP"] <- "3"
stormData[stormData$CROPDMGEXP %in% c("m", "M"), "CROPDMGEXP"] <- "6"
stormData[stormData$CROPDMGEXP == "B", "CROPDMGEXP"] <- "9"

stormData$PROPDMGEXP <- as.numeric(stormData$PROPDMGEXP)
stormData$CROPDMGEXP <- as.numeric(stormData$CROPDMGEXP)

q2StormData <- group_by(stormData, EVTYPE) %>%
    summarize(property = sum(PROPDMG*(10^PROPDMGEXP)), crop = sum(CROPDMG*(10^CROPDMGEXP)), totalDMG = sum(property + crop)) %>%
    arrange(desc(totalDMG))

#about 86 percentage of economic damage is for first 7 event types
sum(q2StormData$totalDMG[1:7])/sum(q2StormData$totalDMG)
barplot(q2StormData$totalDMG[1:7], names.arg = q2StormData$EVTYPE[1:7], cex.names = 0.7)
#as percentage
barplot(q2StormData$totalDMG[1:7]/sum(q2StormData$totalDMG), names.arg = q2StormData$EVTYPE[1:7], cex.names = 0.7)
