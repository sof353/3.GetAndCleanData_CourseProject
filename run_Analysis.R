# 3. Coursera, Getting & Cleaning Data, Course Project 
# 11-23-2014
#
# This script will perform Tasks 1-5 on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Navigate to working directory, 
# Script asumes that data has been downloaded and unziped into "UCI_HAR_Dataset" folder 
if(!file.exists("UCI_HAR_Dataset/features.txt")){
     setwd("./sof/datsci/coursework/3.GetAndCleanData/courseProject")
}
# Clear workspace
rm(list=ls())

# Read data names & parameters to be used for col names 
features <- read.table("./UCI_HAR_Dataset/features.txt", header=FALSE)
activity_label <- read.table("./UCI_HAR_Dataset/activity_labels.txt", header=FALSE, col.names = c('activityID', 'activityType'))

# Get training datasets & assign col names
subject_train <- read.table("./UCI_HAR_Dataset/train/subject_train.txt", header=FALSE)
x_train <- read.table("./UCI_HAR_Dataset/train/x_train.txt", header=FALSE)
y_train <- read.table("./UCI_HAR_Dataset/train/y_train.txt", header=FALSE)
names(subject_train) <- "subjectID"
names(x_train) <- features[,2]
names(y_train) <- "activityID"

# Get the test datasets & assign col names
subject_test <- read.table("./UCI_HAR_Dataset/test/subject_test.txt", header=FALSE)
x_test <- read.table("./UCI_HAR_Dataset/test/x_test.txt", header=FALSE)
y_test <- read.table("./UCI_HAR_Dataset/test/y_test.txt", header=FALSE)
names(subject_test) <- "subjectID"
names(x_test) <- features[,2]
names(y_test) <- "activityID"

# Join separate parts of the test & train dataset together, then merge to create total dataset
dat_train <- cbind(subject_train, x_train, y_train)
dat_test <- cbind(subject_test, x_test, y_test)

## Task 1: create dat, a data.frame that is the combined test and training datasets
dat <- rbind(dat_train, dat_test)

# Reduce the dataset, extracting only the mean and std.dev columns for each variable
# Select mean, std, subjectID, and activityID col names in a logical vector, use as index to select desired subset
col_select <- grepl("mean|std|subjectID|activityID", names(dat)) 

## Task 2: create dat_reduced, a data.frame w/ only the mean and standard deviation variables with subjectID & activiyID
dat_reduced <- dat[,col_select]

## Task 3: Merge dat_reduced & activity_label to deliver descriptive activity names in the data set 
dat_final = merge(dat_reduced,activity_label,by='activityID',all.x=TRUE);

## Task 4: Appropriately label the data set with descriptive activity names. 
col_names <- colnames(dat_final)

for (i in 1:length(col_names)) 
{
     # Assign descriptive names to the variable names
     col_names[i] = gsub("\\()","",col_names[i])
     col_names[i] = gsub("-std","StandardDeviation",col_names[i])
     col_names[i] = gsub("-mean","Mean",col_names[i])
     col_names[i] = gsub("BodyBody","Body",col_names[i])
     col_names[i] = gsub("Mag","Magnitude",col_names[i])
     col_names[i] = gsub("Freq","Frequency",col_names[i])
     col_names[i] = gsub("-X","X",col_names[i])
     col_names[i] = gsub("-Y","Y",col_names[i])
     col_names[i] = gsub("-Z","Z",col_names[i])
}
colnames(dat_final) <- col_names

## Task 5: Create tidy data set with the average of each variable for each activity and each subject.
# Remove activityID as it is no nonger required
# Summarize & tidy the data arranging by activity type and subject ID by taking means of the observations 
tidy_dat_final <- aggregate(dat_final[,!(names(dat_final) %in% c("activityType","subjectID", "activityID"))], 
                        by=list(activityType = dat_final$activityType, subjectID = dat_final$subjectID), mean)

# Write the tidy data set out to a tab delimited text file
write.table(tidy_dat_final, file = "./tidyData_UCI_HAR_Dataset.txt",row.names=FALSE,sep="\t")
