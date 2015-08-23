filesPath <- "C:/Users/jb/Documents/Analytics course/coursera getting and cleaning data/course project/UCI HAR Dataset"
setwd(filesPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

###Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")


##here I load unnecessary packages
library(dplyr)
library(data.table)
library(tidyr)

# Here I am reading subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# I am reading activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

# and here I am reading data files.
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

#activity and subject files merging training and test set by row
#rename variable to "objekt" and  "cisloaktivity"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "objekt")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "cisloaktivity")

# combining data traing and test files
dataTable <- rbind(dataTrain, dataTest)

dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

#merging columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

#features.txt - extracting mena and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# Names after
head(str(dataTable),6)

#write important things to disk

write.table(dataTable, "TidyData.txt", row.name=FALSE)