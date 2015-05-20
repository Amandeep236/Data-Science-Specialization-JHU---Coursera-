# Code Book for Getting and Cleaning Data Course Project

## Description

This file is the code book that describes the variables, the data, and any transformations or work that I performed to clean up the data.

## Source of Data

Human Activity Recognition Using Smartphones Data Set is available at [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones). The original publication
related to this data set is shown bellow:

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

## Data Set Information

Information about this data set can be obtained from README.txt file.

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details.

For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

Notes: 
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

These data are separated into train and test files and contain total number of 10299 observations of 561 measurements, plus subject id and activity id.

### Data Set Variables

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation
* mad(): Median absolute deviation 
* max(): Largest value in array
* min(): Smallest value in array
* sma(): Signal magnitude area
* energy(): Energy measure. Sum of the squares divided by the number of values. 
* iqr(): Interquartile range 
* entropy(): Signal entropy
* arCoeff(): Autorregresion coefficients with Burg order equal to 4
* correlation(): correlation coefficient between two signals
* maxInds(): index of the frequency component with largest magnitude
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency
* skewness(): skewness of the frequency domain signal 
* kurtosis(): kurtosis of the frequency domain signal 
* bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
* angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean

## Transformations

I have done these loading and transformation steps, which I will describe in details in subsequent sections:

* Step 0: Downloading, unzipping and loading train and tets data sets with appropriate column names and classes.
* Step 1: Merging training and test sets.
* Step 2: Extracting only the features related to mean and standard deviation for each measurement.
* Step 3: Replacing activity ids with their corresponding names.
* Step 4: Changing column names to more meaningful names.
* Step 5: Creating a tidy data set with the average of each variable for each activity and each subject and then writing it to a file.

### Step 0: Downloading, Unzipping and Loading data

First we check to see if there exist a folder named "data" in working directory. If there isn't ther, we will create it.
Then we download the data set zip file and save it in "data" folder as "Human-Activity-Recognition.zip".
In the next step, we check the files in the zip file. elements 1, 2, 16, 17, 18, 30, 31 and 32 correspond to "activity_labels.txt", "features.txt", "subject_test.txt", "X_test.txt", "y_test.txt", "subject_train.txt", "X_train.txt" and "y_train.txt" files respectively.
So we extract these files from zip from and store them in "data" folder.

So you can let the file download the data set, or you can place the zip file (named "Human-Activity-Recognition.zip") in the "data" folder in the working directory, or you can place the specified files in the "data" folder.

Next we read training variables (X_train.txt), subjectId (subject_train.txt) and activity (y_train.txt) using read.table() and store them in a variable named "trainData". We also read feature names (features.txt) and set the column names. We do the same thing for test data.

We will remove these temporary variables and data after step 1! (Just to make global environment neat and free up a little space)

### Step 1: Merging training and test sets

Merging training and test sets can be easily done using rbind(). We stored it in "mergedData" data frame and also ordered it using subjectId.

### Step 2: Extracting feature realted to mean and standard deviation

We use grep() for selecting columns that have "mean()" and "std()" indices and store them in "meanstdIndex", add "subjectId" and "activity" to this variable. Then we subset these columns and store them in a data frame named "selectedData".

### Step 3: Replacing activity ids with names

First we read activity names and ids and store them in "activityLabels" data frame and then replace selectedData activity ids
with their names, using regular subsets of R.

### Step 4: Changing column names

First we will substitute starting "t" and "f" with "time" and "frequency". Then substitute "Acc", "Gyro" and "Mag" with "Accelerometer", "Gyroscope" and "Magnitude". Finally we remove duplicated "Body" from "BodyBody" and parentheses "()".

### Step 5: Creating tidy data set with required averages

We want to create a second, independent tidy data set with the average of each variable for each activity and each subject. For this purpose we use "reshape2" package. First we melt our "selectedData" set with "subjectId" and "activity" as id.vars and store it in "meltData". Then we dcast "meldData" with "mean" as aggregate function and store this tidy data in "tidyData" data frame. It has 180 (30 subjectId x 6 activity) rows and 68 columns. Finally, as these are averaged features, we add "mean" to the beginning of column names and write it to "tidyAverages.txt".

## Tidy Data Information

There are 180 rows and 68 columns in this data set. Each row is for average of each variable for each activity and each subject. First and second columns correspond to subject Id and activity, and other 66 columns correspond to averages of mean and standard deviation of measurements (in X, Y and Z directions & ...) about movements of the subject while doing different activities.

'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

Time:

* meanTimeBodyAccelerometer-mean-XYZ
* meanTimeBodyAccelerometer-std-XYZ
* meanTimeGravityAccelerometer-mean-XYZ
* meanTimeGravityAccelerometer-std-XYZ
* meanTimeBodyAccelerometerJerk-mean-XYZ
* meanTimeBodyAccelerometerJerk-std-XYZ
* meanTimeBodyGyroscope-mean-XYZ
* meanTimeBodyGyroscope-std-XYZ
* meanTimeBodyGyroscopeJerk-mean-XYZ
* meanTimeBodyGyroscopeJerk-std-XYZ
* meanTimeBodyAccelerometerMagnitude-mean
* meanTimeBodyAccelerometerMagnitude-std
* meanTimeGravityAccelerometerMagnitude-mean
* meanTimeGravityAccelerometerMagnitude-std
* meanTimeBodyAccelerometerJerkMagnitude-mean
* meanTimeBodyAccelerometerJerkMagnitude-std
* meanTimeBodyGyroscopeMagnitude-mean
* meanTimeBodyGyroscopeMagnitude-std
* meanTimeBodyGyroscopeJerkMagnitude-mean
* meanTimeBodyGyroscopeJerkMagnitude-std

Frequency:

* meanFrequencyBodyAccelerometer-mean-XYZ
* meanFrequencyBodyAccelerometer-std-XYZ
* meanFrequencyGravityAccelerometer-mean-XYZ
* meanFrequencyGravityAccelerometer-std-XYZ
* meanFrequencyBodyAccelerometerJerk-mean-XYZ
* meanFrequencyBodyAccelerometerJerk-std-XYZ
* meanFrequencyBodyGyroscope-mean-XYZ
* meanFrequencyBodyGyroscope-std-XYZ
* meanFrequencyBodyGyroscopeJerk-mean-XYZ
* meanFrequencyBodyGyroscopeJerk-std-XYZ
* meanFrequencyBodyAccelerometerMagnitude-mean
* meanFrequencyBodyAccelerometerMagnitude-std
* meanFrequencyBodyAccelerometerJerkMagnitude-mean
* meanFrequencyBodyAccelerometerJerkMagnitude-std
* meanFrequencyBodyGyroscopeMagnitude-mean
* meanFrequencyBodyGyroscopeMagnitude-std
* meanFrequencyBodyGyroscopeJerkMagnitude-mean
* meanFrequencyBodyGyroscopeJerkMagnitude-std