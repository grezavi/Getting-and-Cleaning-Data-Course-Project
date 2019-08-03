# 1. Merge the training and the test sets to create one data set.

library(dplyr)
pathdata = file.path("./UCI HAR Dataset")


features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
One_Dataset <- cbind(Subject, Y, X)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

Extracted_Dataset <- One_Dataset %>% select(subject, code, contains("mean"), contains("std"))
Extracted_Dataset$code <- activities[Extracted_Dataset$code, 2]

# 3/4. Assigning descriptive activity names to name the activities in the data set. Appropriately labels the data set with descriptive variable names.

names(Extracted_Dataset)[2] = "activity"
names(Extracted_Dataset)<-gsub("Acc", "Accelerometer", names(Extracted_Dataset))
names(Extracted_Dataset)<-gsub("Gyro", "Gyroscope", names(Extracted_Dataset))
names(Extracted_Dataset)<-gsub("BodyBody", "Body", names(Extracted_Dataset))
names(Extracted_Dataset)<-gsub("Mag", "Magnitude", names(Extracted_Dataset))
names(Extracted_Dataset)<-gsub("^t", "Time", names(Extracted_Dataset))
names(Extracted_Dataset)<-gsub("^f", "Frequency", names(Extracted_Dataset))
names(Extracted_Dataset)<-gsub("tBody", "TimeBody", names(Extracted_Dataset))
names(Extracted_Dataset)<-gsub("-mean()", "Mean", names(Extracted_Dataset), ignore.case = TRUE)
names(Extracted_Dataset)<-gsub("-std()", "STD", names(Extracted_Dataset), ignore.case = TRUE)
names(Extracted_Dataset)<-gsub("-freq()", "Frequency", names(Extracted_Dataset), ignore.case = TRUE)
names(Extracted_Dataset)<-gsub("angle", "Angle", names(Extracted_Dataset))
names(Extracted_Dataset)<-gsub("gravity", "Gravity", names(Extracted_Dataset))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Tidy_Dataset <- Extracted_Dataset %>% group_by(subject, activity) %>% summarise_all(funs(mean))
write.table(Tidy_Dataset, "Tidy_Dataset.txt", row.name=FALSE)

str(Tidy_Dataset)
head(Tidy_Dataset,10)
tail(Tidy_Dataset,10)



