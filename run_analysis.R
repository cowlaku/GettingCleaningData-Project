library(dplyr)

# 1. Merges the training and the test sets to create one data set.

# Load the data files
df.test <- read.table("./UCI HAR Dataset/test/X_test.txt")
test.label <- read.table("./UCI HAR Dataset/test/Y_test.txt")
test.subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
df.train <- read.table("./UCI HAR Dataset/train/X_train.txt")
train.label <- read.table("./UCI HAR Dataset/train/Y_train.txt")
train.subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
act.names <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

# Combine data
df.test <- cbind(test.subject, test.label, df.test)
df.train <- cbind(train.subject, train.label, df.train)
mydata <- rbind(df.test, df.train)

# Name columns
names(mydata) <- c("Subject", "Activity", as.character(features$V2))
rm(list = setdiff(ls(), c("mydata", "act.names", "features")))


# 2. Extracts only the measurements on the mean and standard deviation 
#    for each measurement. 
with.mean.std <- 
    grepl("mean", features$V2) & !grepl("meanFreq", features$V2) | 
    grepl("std", features$V2)
mydata <- mydata[,c(TRUE, TRUE, with.mean.std)]


# 3. Uses descriptive activity names to name the activities in the data 
#    set
mydata$Activity <- act.names$V2[mydata$Activity]


# 4. Appropriately labels the data set with descriptive variable names. 
# It is necessary to remove symbols as "()" and "-" or dplyr would read 
# them as separators or operators.
# Names including "BodyBody" are typos of "Body"
names(mydata) <- gsub("-", "_", names(mydata), perl=TRUE)
names(mydata) <- gsub("[()]+", "", names(mydata), perl=TRUE)
names(mydata) <- gsub("BodyBody", "Body", names(mydata), perl=TRUE)


# 5. From the data set in step 4, creates a second, independent tidy 
#    data set with the average of each variable for each activity and 
#    each subject.
groupmean <- group_by(mydata, Activity, Subject) 
groupmean <- summarise_each(groupmean, funs(mean))


# Save the tidy data as a txt file
write.table(groupmean, file = "Tidydata.txt", row.name = FALSE)
