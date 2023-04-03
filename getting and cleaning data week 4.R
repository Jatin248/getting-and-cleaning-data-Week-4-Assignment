### Reading train dataset
X_train <- read.table("/home/rstudio/UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("/home/rstudio/UCI HAR Dataset/train/y_train.txt")
subject_teain <-read.table("/home/rstudio/UCI HAR Dataset/train/subject_train.txt")

### Reading test dataset
X_test <- read.table("/home/rstudio/UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("/home/rstudio/UCI HAR Dataset/test/y_test.txt")
subject_test <-read.table("/home/rstudio/UCI HAR Dataset/test/subject_test.txt")

###Reading feature vector

features <- read.table("/home/rstudio/UCI HAR Dataset/features.txt")

###Reading Activity_labels
activitylabels <- read.table("/home/rstudio/UCI HAR Dataset/activity_labels.txt")

colnames(activitylabels) <- c("activityId","activitylabels")


humanactivity <- rbind(cbind(X_train,Y_train,subject_teain),
                       cbind(X_test,Y_test,subject_test))

colnames(humanactivity) <- c("subject", features[, 2], "activity")


### 2 Extracts only the measurements on the mean and standard deviation for each measurement.

columntokeep <- grepl("subject|activity|mean|std",colnames(humanactivity))

humanactivity <- humanactivity[,columntokeep]

### 3 Uses descriptive activity names to name the activities in the data set

humanactivity$activity <- factor(humanactivity$activity, levels = activitylabels[,1], labels = activitylabels[,2])

### 4 Appropriately labels the data set with descriptive variable names. 

humanActivityCols <- colnames(humanactivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)


#### 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

humanActivityMeans <- humanactivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
