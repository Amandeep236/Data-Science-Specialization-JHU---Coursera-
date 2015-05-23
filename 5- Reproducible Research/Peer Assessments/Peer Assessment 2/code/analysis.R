#check and create data folder
if(!dir.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileURL, destfile = "./data/StormData.csv.bz2", method = "curl")
downloadTime <- date()
stormData <- read.csv("./data/StormData.csv.bz2")
