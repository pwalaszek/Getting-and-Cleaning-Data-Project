run_analysis <- function()
{
  # 1) Merges the training and the test sets to create one data set.
  trainX <- read.table("./UCI HAR Dataset/train/X_train.txt")
  testX <- read.table("./UCI HAR Dataset/test/X_test.txt")
  
  dataX <- rbind(trainX, testX)
  
  # 2) Extracts only the measurements on the mean and standard deviation for each measurement.
  features <- read.table("./UCI HAR Dataset/features.txt")
  
  features <- c(as.character(features$V2))
  colnames(dataX) <- features
  
  columnsWithMeanOrStd <-grep("-mean|-std",features)
  
  # 3) Uses descriptive activity names to name the activities in the data set
  trainY <- read.table("./UCI HAR Dataset/train/y_train.txt")
  testY <- read.table("./UCI HAR Dataset/test/y_test.txt")
  
  dataY <- rbind(trainY, testY)
  colnames(dataY) <- c("Activity")
  data <- cbind(dataY, dataX[, columnsWithMeanOrStd])
  
  # 4) Appropriately labels the data set with descriptive variable names. 
  activityLables <- read.table("./UCI HAR Dataset/activity_labels.txt")
  
  data$Activity <- as.factor(data$Activity)
  levels(data$Activity) <- levels(activityLables$V2)
  
  # 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  subject <- rbind(subjectTrain, subjectTest)
  colnames(subject) <- c("Subject")
  
  data <- cbind(subject, data)
  
  if (!require("reshape2")) {
    install.packages("reshape2")
    library(reshape2)
  }
  
  melted = melt(data, id.var = c("Subject", "Activity"))
  tidyData = dcast(melted , Subject + Activity ~ variable, mean)
  
  write.table(tidyData, file = "tidyData.txt", row.name=FALSE)
  tidyData
}