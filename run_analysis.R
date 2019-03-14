library(plyr)

###----------------------------------
# Get data file
###----------------------------------

dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataFile <- "UCI HAR Dataset.zip"

if (!file.exists(dataFile)) {
  download.file(dataUrl, dataFile, mode = "wb")
}

# unzip zip file
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(dataFile)
}


###------------------------------------
# Read data file
###------------------------------------

# read training data
trSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
ttSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
ttValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
ttActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
ractivities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(ractivities) <- c("activityId", "activityLabel")


###----------------------------------------------------------------------
# Step 1 - Merge the training and the test sets to create one data set
###----------------------------------------------------------------------

# one
humAct <- rbind(
  cbind(trSubjects, trValues, trActivity),
  cbind(ttSubjects, ttValues, ttActivity)
)

# two
rm(trSubjects, trValues, trActivity, 
   ttSubjects, ttValues, ttActivity)

# Three
colnames(humAct) <- c("subject", features[, 2], "activity")


###----------------------------------------------------------------------
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
###----------------------------------------------------------------------

# one
colToKeep <- grepl("subject|activity|mean|std", colnames(humAct))

# ... and keep data in these columns only
humAct <- humAct[, colToKeep]

###----------------------------------------------------------------------
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
###----------------------------------------------------------------------

# replace activity values with named factor levels
humAct$activity <- factor(humAct$activity, 
  levels = ractivities[, 1], labels = ractivities[, 2])


###----------------------------------------------------------------------
# Step 4 - Appropriately label the data set with descriptive variable names
###----------------------------------------------------------------------

# get column names
humanActivityCols <- colnames(humAct)

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

# use new labels as column names
colnames(humAct) <- humanActivityCols


###----------------------------------------------------------------------
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
###----------------------------------------------------------------------

# group by subject and activity and summarise using mean
humanActivityMeans <- humAct %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)