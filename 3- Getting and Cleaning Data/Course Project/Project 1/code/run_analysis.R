#check & create data folder
if(!dir.exists("./data")){dir.create("./data")}
#downloading Human Activity Recognition data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./data/Human-Activity-Recognition.zip", method = "curl")
downloadTime <- date()
#getting list of files in the zip, and storing path of 8 files which are required for this analysis
listFiles <- unzip("./data/Human-Activity-Recognition.zip", list = T)
listFiles <- listFiles[c(1, 2, 16, 17, 18, 30, 31, 32), 1]
#unziping required files from zip file to "data" folder
unzip("./data/Human-Activity-Recognition.zip", files = listFiles, exdir = "./data", junkpaths = T)
#file.remove("./data//Human-Activity-Recognition.zip") #we can delete zip file after extraction of required files!
#reading features data. this will be used for naming columns of train and test data.
features <- read.table("./data/features.txt", colClasses = "character")
features <- features[, 2]
#reading train data and storing them in trainData data frame.
X.train <- read.table("./data/X_train.txt")
names(X.train) <- features
Y.train <- read.table("./data/y_train.txt", col.names = "activity", colClasses = "integer")
Sub.train <- read.table("./data/subject_train.txt", col.names = "subjectId", colClasses = "integer")
trainData <- cbind(X.train, Sub.train, Y.train)
#reading test data and storing them in testData data frame.
X.test <- read.table("./data/X_test.txt")
names(X.test) <- features
Y.test <- read.table("./data/y_test.txt", col.names = "activity", colClasses = "integer")
Sub.test <- read.table("./data/subject_test.txt", col.names = "subjectId", colClasses = "integer")
testData <- cbind(X.test, Sub.test, Y.test)
##Merges the training and the test sets to create one data set.
mergedData <- rbind(trainData, testData)
mergedData <- mergedData[order(mergedData$subjectId),]
#removing temporary data frames
rm(X.train, Y.train, Sub.train, X.test, Y.test, Sub.test, trainData, testData)

##Extracts only the measurements on the mean and standard deviation for each measurement
#selecting columns that have "mean()" and "std()", plus the "subjectId" and "activity" and storing them in selectedData.
meanstdIndex <- grep(pattern = "mean\\(\\)|std\\(\\)", x = names(mergedData))
meanstdIndex <- c(meanstdIndex, 562, 563)
selectedData <- mergedData[, meanstdIndex]

##Uses descriptive activity names to name the activities in the data set
#reading activity names and ids and storing them in "activityLabels" data frame to replace selectedData activity ids
#with their names using regular subsets of R.
activityLabels <- read.table("./data/activity_labels.txt")
selectedData[, "activity"] <- activityLabels[selectedData[, "activity"], 2]

##Appropriately labels the data set with descriptive variable names
names(selectedData)
#substituting starting "t" and "f" with "time" and "frequency".
names(selectedData) <- gsub("^t", replacement = "time", x = names(selectedData))
names(selectedData) <- gsub("^f", replacement = "frequency", x = names(selectedData))
#substituting "Acc", "Gyro" and "Mag" with "Accelerometer", "Gyroscope" and "Magnitude".
names(selectedData) <- gsub("Acc", replacement = "Accelerometer", x = names(selectedData))
names(selectedData) <- gsub("Gyro", replacement = "Gyroscope", x = names(selectedData))
names(selectedData) <- gsub("Mag", replacement = "Magnitude", x = names(selectedData))
#removing duplicated "Body" from "BodyBody".
names(selectedData) <- gsub("BodyBody", replacement = "Body", x = names(selectedData))
#removing parentheses.
names(selectedData) <- gsub("mean\\(\\)", replacement = "mean", x = names(selectedData))
names(selectedData) <- gsub("std\\(\\)", replacement = "std", x = names(selectedData))

##creates a second, independent tidy data set with the average of each variable for each activity and each subject
#melting with "subjectId" and "activity" as ids and storing it in "meltData", then casting with "mean" as aggregate
#function. Storing this tidy data in "tidyData" data frame. it has 180 (30x6) rows and 68 columns.
library(reshape2)
meltData <- melt(selectedData, id.vars = c("subjectId", "activity"))
tidyData <- dcast(meltData, subjectId + activity ~ variable, mean)
#adding "mean" in front of columns for better labeling. making starting "time" and "frequency" uppercase, for being
#consistant in labeling with camelCase standard!
names(tidyData) <- gsub("^time", "Time", names(tidyData))
names(tidyData) <- gsub("^frequency", "Frequency", names(tidyData))
names(tidyData)[-c(1, 2)] <- paste0("mean", names(tidyData)[-c(1, 2)])

#writing the final tidy data to "tidyAverages.txt"
write.table(tidyData, file = "./tidyAverages.txt", row.names = FALSE)