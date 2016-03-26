#The following one R script called run_analysis.R that does the following.

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# The following loads data into R
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_training <- read.table("UCI HAR Dataset/train/X_train.txt")
y_training <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_training <- read.table("UCI HAR Dataset/train/subject_train.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

#The following is used to combine or merge the training and test sets to create one set
x_total <- rbind(x_test, x_training)
y_total <- rbind(y_test, y_training)
subject_total <- rbind(subject_test, subject_training)
names(x_total) <- features$V2
names(y_total) <- "activity"
names(subject_total) <- "subject_id"

# The following is used for extracting mean and standard deviation
x_mean_std <- x_total[, grep(("mean|std"), features[, 2])]
total_data <- cbind(subject_total, y_total, x_mean_std)

# The following is done to put descriptive activity names to name the activities in the data set
activity_labels$V2 <- tolower(activity_labels$V2)
activity_labels$V2 <- sub("_"," ", activity_labels$V2)
for(i in 1:nrow(activity_labels)){
  total_data$activity[total_data$activity == activity_labels[i,1]] <- activity_labels[i,2]
}

#Appropriately labels the data set with descriptive variable names.
names(total_data) <- sub("^t", "time_", names(total_data))
names(total_data) <- sub("^f", "frequency_", names(total_data))
names(total_data) <- sub("BodyBody", "body_", names(total_data))
names(total_data) <- sub("Body", "body_", names(total_data))
names(total_data) <- sub("mean\\(\\)", "mean", names(total_data))
names(total_data) <- sub("std\\(\\)", "standard_deviation", names(total_data))
names(total_data) <- sub("Acc", "acceleration_", names(total_data))
names(total_data) <- sub("Gyro", "gyroscope_", names(total_data))
names(total_data) <- sub("Gravity", "gravity_", names(total_data))
names(total_data) <- sub("Jerk", "_jerk_", names(total_data))
names(total_data) <- sub("Mag", "magnitude", names(total_data))
names(total_data) <- sub("__", "_", names(total_data))
names(total_data) <- sub("_-", "-", names(total_data))
names(total_data) <- tolower(names(total_data))
names(total_data) <- sub("meanfreq\\(\\)", "mean-frequency", names(total_data))

#Created a tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
tidymeanoutput <- total_data %>% 
group_by(subject_id, activity) %>% 
summarise_each(funs(mean), -subject_id, -activity)
write.table(tidymeanoutput, "UCI HAR Dataset/tidymean.txt", sep="\t", col.names = TRUE, row.names = FALSE
