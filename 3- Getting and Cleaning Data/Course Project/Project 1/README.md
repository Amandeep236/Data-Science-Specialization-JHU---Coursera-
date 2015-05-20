#Getting and Cleaning Data Course Project
This repository is for files associated with getting and cleaning data course project from John Hopkins University course on Coursera.

## Introduction

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) where the data was obtained.

The purpose of this project is to demonstrate ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. We will use Human Activity Recognition Using Smartphones Data Set.

## Files

There are 3 important files (other than this README.md file) in this repository.

### run_analysis.R

run_analysis.R file contains all the code for the analysis. In this file we create a "data" folder, download Human Activity Recognition data set zip file to "data" folder, unzip this file and after loading the data, we start clean it. we are required to do 5 specific things in this file:

1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement. 
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive variable names. 
5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

finally we write the tidy data to a file named tidyAverages.txt.

**Note: run_analysis.R automatically downloads the data set. For other information about getting and loading data, refer to Step 0 section in Transformations of CodeBook.md.**

### CodeBook.md

This file is the code book that describes the variables, the data, and any transformations or work that I performed to clean up the data. You can read detailed information about source of data, data set information, data set variables, transformations and tidy data information in this code book.

### tidyAverages.txt

This is a tidy data set with the average of each variable for each activity and each subject. It has 180 rows and 68 columns. First column is subject Id, second column is activity and other columns are averages (means) of mean and standard deviation of measurements (in X, Y and Z directions & ...) about movements of the specified subject while doing specified activity. Name of these 66 columns have the form meanTime/Frequency...-mean/-sd... . You can see detailed information about these variables in "Tidy Data Information" section of the code book.