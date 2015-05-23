## 1. Join data sets
# If we start right below the data directory, then enter it
if(file.exists("UCI HAR Dataset")) { setwd("UCI HAR Dataset/")}
# create new directories
if(!file.exists("./joint")){dir.create("./joint")}
if(!file.exists("./joint/Inertial Signals")){dir.create("./joint/Inertial Signals")}
# retrieve list of all files in test directory
files <- list.files("test", recursive=TRUE)
# loop through file list
# read one file at a time from the test directory, and its counterpart
# from the train directory
for(file in files) {
data1 <- read.table(paste0("test/", file));
data2 <- read.table(paste0("train/", sub("test", "train", file)));
# append the data table from the latter to the data table from the former
data <- rbind(data1, data2);
# write file with the joint data table, keeping the name except that
# "test" or "train" is replaced by "joint"
write.table(data, file=paste0("joint/", sub("test", "joint", file)), col.names=FALSE, row.names=FALSE)}

## 2. Extract means and standard deviations
features <- read.table("features.txt", as.is=TRUE)
chosenFeatures <- sort(c(grep("std", features[,2]),grep("mean", features[,2])))
data <- read.table("joint//X_joint.txt")
data <- data[,chosenFeatures]

## 3. Use activity labels
actLabels <- read.table("activity_labels.txt", as.is=TRUE)
actNumbers <- read.table("joint//y_joint.txt")
actData <- actNumbers
# replace activity numbers by labels
for(n in seq(dim(actLabels)[1])) {
actData <- replace(actData, actData==n, actLabels[n,2])}
# add row labels to table
data <- cbind(actData, data)

## 4. Use descriptive column labels
# Move t/f prefix, classification and direction into suffix with parenthesis
tmp <- sub("([tf])(.*?)-(.*)\\(\\)(-)([XYZ])", "\\2\\(\\1, \\3, \\5\\)", features[chosenFeatures,2], perl=TRUE)
# Move t/f prefix, classification without direction into suffix with parenthesis
tmp <- sub("([tf])(.*?)-(.*)\\(\\)", "\\2\\(\\1, \\3\\)", tmp, perl=TRUE)
# replace t by time and f by freq., respectively
tmp <- sub("\\(t", "\\(time", tmp)
tmp <- sub("\\(f", "\\(freq", tmp)
# more tuning possible but not necessary
# change column labels
colnames(data) <- c("Activity", tmp)

## 5. Create clean data set with averages
# Add subjects column
subject <- read.table("joint//subject_joint.txt")[,1]
data <- cbind(subject, data)
# create averages' dataframe
nSubs <- 30
ave <- data.frame()
# calculate and store averages
for(n in seq(nSubs)) {
ave <- rbind(ave,cbind(n, aggregate(data[data[,1]==n,3:81], list(data[data[,1]==n,]$Activity), mean)))}
colnames(ave)[1:2] <- c("Subject", "Activity")
# save data to file
write.table(ave, file="joint//averages.txt", row.name=FALSE)
