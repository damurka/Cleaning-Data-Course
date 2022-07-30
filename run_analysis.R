library(dplyr)
library(reshape2)

## Reads file in test or train folders in table format
## 
getFile <- function(folder, type) {
  path <- c("UCI HAR Dataset", folder)
  filename <- sprintf("%s_%s.txt",type, folder)
  path <- append(path, filename)
  filePath <- paste(path, collapse = "/")
  read.table(filePath)
}

## Merges data in train or test data sets
getTrackData <- function(folder) {
  if (!(folder %in% c("test", "train"))) {
    stop("Invalid folder parameter")
  }
  
  ## Loads the activity labels
  activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
  ## Loads the x data column names
  features <- read.table("UCI HAR Dataset/features.txt")
  
  ## Extracts only the measurements on the mean and standard deviation for each measurement. 
  subject <- getFile(folder, "subject")
  x <- getFile(folder, "x")
  y <- getFile(folder, "y")
  
  ## Appropriately labels the data set with descriptive variable names.
  names(x) = features[,2]
  names(y) = "activityId"
  names(activityLabels) = c("activityId", "activityLabel")
  names(subject) = "subject"
  
  ## Extracts only the measurements on the mean and standard deviation for each measurement. 
  measureCols = grep("mean|std",features[,2])
  x <- select(x, measureCols)
  
  ## Merging y data with activity labels
  y <- merge(y, activityLabels)
  
  ## Merging data on the subject with y and x
  cbind(subject,y,x)
}

## Download the dataset and store in the exercise folder and unzip
## if (!file.exists("exercise")) { dir.create("exercise") }
## downloadUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
## download.file(downloadUrl, destfile = "exercise/dataset.zip")
## unzip("exercise/dataset.zip", exdir = "exercise")

cleanData <- function() {
  ## Merging the training and the test sets to create one data set.
  testData <- getTrackData("test")
  trainData <- getTrackData("train")
  combined <- rbind(testData, trainData)

  ## creates a second, independent tidy data set with the average of each variable 
  ## for each activity and each subject.
  idLabels <- c("subject", "activityId", "activityLabel")
  measureLabels <- setdiff(names(combined), idLabels)
  meltData <- melt(combined, id = idLabels, measure.vars = measureLabels)
  tidyData <- dcast(meltData, subject + activityLabel ~ variable, mean)
  
  ## Creates tidy data set as a txt file 
  write.table(tidyData, file = "tidy_data.txt", row.names = FALSE)
}
