# Assessment 1 - Code Book
===============================

## Input Data Info:

Info from the source data book, 

> The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
> 
> The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.
> 
> For each record it is provided:
> - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
> - Triaxial Angular velocity from the gyroscope. 
> - A 561-feature vector with time and frequency domain variables. 
> - Its activity label. 
> - An identifier of the subject who carried out the experiment.
> Which can be found at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.
> 
> The dataset includes the following files:
> 
> - 'README.txt'
> 
> - 'features_info.txt': Shows information about the variables used on the feature vector.
> 
> - 'features.txt': List of all features.
> 
> - 'activity_labels.txt': Links the class labels with their activity name.
> 
> - 'train/X_train.txt': Training set.
> 
> - 'train/y_train.txt': Training labels.
> 
> - 'test/X_test.txt': Test set.
> 
> - 'test/y_test.txt': Test labels.
> 
> The following files are available for the train and test data. Their descriptions are equivalent. 
> 
> - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 


===============================
##The Transformations:

First I read and bind training and test data. Next extracts only the measurements on the mean and standard deviation for each measurement. Next bind Activity and Subject information. In the end I calculate mean for each variable per Subject and per Activity.

## Output Data Info
The resulting `./tidy_data.txt` dataset includes mean per subject and per activity for features:
"tBodyAcc-mean()-X"               "tBodyAcc-mean()-Y"               "tBodyAcc-mean()-Z"               "tBodyAcc-std()-X"               
"tBodyAcc-std()-Y"                "tBodyAcc-std()-Z"                "tGravityAcc-mean()-X"            "tGravityAcc-mean()-Y"           
"tGravityAcc-mean()-Z"            "tGravityAcc-std()-X"             "tGravityAcc-std()-Y"             "tGravityAcc-std()-Z"            
"tBodyAccJerk-mean()-X"           "tBodyAccJerk-mean()-Y"           "tBodyAccJerk-mean()-Z"           "tBodyAccJerk-std()-X"           
"tBodyAccJerk-std()-Y"            "tBodyAccJerk-std()-Z"            "tBodyGyro-mean()-X"              "tBodyGyro-mean()-Y"             
"tBodyGyro-mean()-Z"              "tBodyGyro-std()-X"               "tBodyGyro-std()-Y"               "tBodyGyro-std()-Z"              
"tBodyGyroJerk-mean()-X"          "tBodyGyroJerk-mean()-Y"          "tBodyGyroJerk-mean()-Z"          "tBodyGyroJerk-std()-X"          
"tBodyGyroJerk-std()-Y"           "tBodyGyroJerk-std()-Z"           "tBodyAccMag-mean()"              "tBodyAccMag-std()"              
"tGravityAccMag-mean()"           "tGravityAccMag-std()"            "tBodyAccJerkMag-mean()"          "tBodyAccJerkMag-std()"          
"tBodyGyroMag-mean()"             "tBodyGyroMag-std()"              "tBodyGyroJerkMag-mean()"         "tBodyGyroJerkMag-std()"         
"fBodyAcc-mean()-X"               "fBodyAcc-mean()-Y"               "fBodyAcc-mean()-Z"               "fBodyAcc-std()-X"               
"fBodyAcc-std()-Y"                "fBodyAcc-std()-Z"                "fBodyAcc-meanFreq()-X"           "fBodyAcc-meanFreq()-Y"          
"fBodyAcc-meanFreq()-Z"           "fBodyAccJerk-mean()-X"           "fBodyAccJerk-mean()-Y"           "fBodyAccJerk-mean()-Z"          
"fBodyAccJerk-std()-X"            "fBodyAccJerk-std()-Y"            "fBodyAccJerk-std()-Z"            "fBodyAccJerk-meanFreq()-X"      
"fBodyAccJerk-meanFreq()-Y"       "fBodyAccJerk-meanFreq()-Z"       "fBodyGyro-mean()-X"              "fBodyGyro-mean()-Y"             
"fBodyGyro-mean()-Z"              "fBodyGyro-std()-X"               "fBodyGyro-std()-Y"               "fBodyGyro-std()-Z"              
"fBodyGyro-meanFreq()-X"          "fBodyGyro-meanFreq()-Y"          "fBodyGyro-meanFreq()-Z"          "fBodyAccMag-mean()"             
"fBodyAccMag-std()"               "fBodyAccMag-meanFreq()"          "fBodyBodyAccJerkMag-mean()"      "fBodyBodyAccJerkMag-std()"      
"fBodyBodyAccJerkMag-meanFreq()"  "fBodyBodyGyroMag-mean()"         "fBodyBodyGyroMag-std()"          "fBodyBodyGyroMag-meanFreq()"    
"fBodyBodyGyroJerkMag-mean()"     "fBodyBodyGyroJerkMag-std()"      "fBodyBodyGyroJerkMag-meanFreq()"

The above variables where choosen as they included either -mean or -std in their original names.