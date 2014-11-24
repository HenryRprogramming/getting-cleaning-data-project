#################################################################################################
# Getting and Cleaning Data Course Project

# runAnalysis.r:

# Dataset
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This script perfroms these steps
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##################################################################################################


# Clean up workspace
rm(list=ls())

# 1. Merge the training and the test sets to create one data set.

#working directory
setwd('/Users/zhehe/Documents/coursera/data/UCI\ HAR\ Dataset/');

#read data
features     = read.table('./features.txt',header=FALSE);
activityType = read.table('./activity_labels.txt',header=FALSE);
subjectTrain = read.table('./train/subject_train.txt',header=FALSE);
xTrain       = read.table('./train/x_train.txt',header=FALSE);
yTrain       = read.table('./train/y_train.txt',header=FALSE);

# Assigin column names to activityType, subjectTrian, xTrain, yTrain
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Training dataset: Merge yTrain, subjectTrain, and xTrain
trainingDataset = cbind(yTrain,subjectTrain,xTrain);

# Read test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE);
xTest       = read.table('./test/x_test.txt',header=FALSE);
yTest       = read.table('./test/y_test.txt',header=FALSE);

# Assign column names to the test data
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# Testing dataset: Merge the xTest, yTest and subjectTest data
testDataset = cbind(yTest,subjectTest,xTest);


# Final dataset: Combine training and test data
finalDataset = rbind(trainingDataset,testDataset);

# Create a vector for the column names from the finalDataset
colNames  = colnames(finalDataset);

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table
finalDataset = finalDataset[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table
finalDataset = merge(finalDataset,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector
colNames  = colnames(finalDataset);

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalDataset) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalDataset[,names(finalDataset) != 'activityType'];

# Summarizing the finalDataNoActivityType table
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
