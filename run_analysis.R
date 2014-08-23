#Coursera: Getting and Cleaning Data--Project

#clearing Global Environmenta and setting working directory
rm(list=ls(all=TRUE))
setwd("/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data") #setting working directory

#finding and downloading data
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./UCI HAR dataset", method="curl") #downloading file

#reading in data
xtest<-read.table("/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt")
ytest<-read.table("/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt")
subjtest<-read.table("/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt")
xtrain<-read.table("/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt")
ytrain<-read.table("/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt")
subjtrain<-read.table("/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt")

#combining test and training data with subject and activity variables
testdata<-cbind(subjtest,ytest,xtest)
traindata<-cbind(subjtrain,ytrain,xtrain)

#combining training and test data into one data frame and extracting columns only including mean/sd for each measurement
alldata<-rbind(traindata,testdata)
alldata2<-alldata[,c(1,2,3,4,5,6,7,8,43,44,45,46,47,48,83,84,85,86,87,88,123,124,125,126,127,128,163,164,165,166,167,168,203,204,216,217,229,230,242,243,255,256,268,269,270,271,272,273,347,348,349,350,351,352,426,427,428,429,430,431,505,506,518,519,531,532,544,545)]

#Subsetting data to add descriptive name of activity as column in data frame
alldata2sub1<-alldata2[alldata2$V1.1==1,]
alldata2sub1$activity<-"Walking"
alldata2sub2<-alldata2[alldata2$V1.1==2,]
alldata2sub2$activity<-"Walking_Upstairs"
alldata2sub3<-alldata2[alldata2$V1.1==3,]
alldata2sub3$activity<-"Walking_Downstairs"
alldata2sub4<-alldata2[alldata2$V1.1==4,]
alldata2sub4$activity<-"Sitting"
alldata2sub5<-alldata2[alldata2$V1.1==5,]
alldata2sub5$activity<-"Standing"
alldata2sub6<-alldata2[alldata2$V1.1==6,]
alldata2sub6$activity<-"Laying"
alldatafinal<-rbind(alldata2sub1,alldata2sub2,alldata2sub3,alldata2sub4,alldata2sub5,alldata2sub6)

#rearranging data frame so activity name was in second column
dat<-alldatafinal[,c("V1","activity","V1.2","V2","V3","V4","V5","V6","V41","V42","V43","V44","V45","V46","V81","V82","V83","V84","V85","V86","V121","V122","V123","V124","V125","V126","V161","V162","V163","V164","V165","V166","V201","V202","V214","V215","V227","V228","V240","V241","V253","V254","V266","V267","V268","V269","V270","V271","V345","V346","V347","V348","V349","V350","V424","V425","V426","V427","V428","V429","V503","V504","V516","V517","V529","V530","V542","V543")]

#adding descriptive column names
colnames(dat) <- c("Subject","Activity"," tBodyAcc-meanX", "tBodyAcc-meanY", "tBodyAcc-meanZ", "tBodyAcc-stdX", "tBodyAcc-stdY", "tBodyAcc-stdZ", "tGravityAcc-meanX", "tGravityAcc-meanY", "tGravityAcc-meanZ", "tGravityAcc-stdX", "tGravityAcc-stdY", "tGravityAcc-stdZ", "tBodyAccJerk-meanX", "tBodyAccJerk-meanY", "tBodyAccJerk-meanZ", "tBodyAccJerk-stdX", "tBodyAccJerk-stdY", "tBodyAccJerk-stdZ", "tBodyGyro-meanX", "tBodyGyro-meanY", "tBodyGyro-meanZ", "tBodyGyro-stdX", "tBodyGyro-stdY", "tBodyGyro-stdZ", "tBodyGyroJerk-meanX", "tBodyGyroJerk-meanY", "tBodyGyroJerk-meanZ", "tBodyGyroJerk-stdX", "tBodyGyroJerk-stdY", "tBodyGyroJerk-stdZ", "tBodyAccMag-mean", "tBodyAccMag-std", "tGravityAccMag-mean", "tGravityAccMag-std", "tBodyAccJerkMag-mean", "tBodyAccJerkMag-std", "tBodyGyroMag-mean", "tBodyGyroMag-std", "tBodyGyroJerkMag-mean", "tBodyGyroJerkMag-std", "fBodyAcc-meanX", "fBodyAcc-meanY", "fBodyAcc-meanZ", "fBodyAcc-stdX", "fBodyAcc-stdY", "fBodyAcc-stdZ", "fBodyAccJerk-meanX", "fBodyAccJerk-meanY", "fBodyAccJerk-meanZ", "fBodyAccJerk-stdX", "fBodyAccJerk-stdY", "fBodyAccJerk-stdZ", "fBodyGyro-meanX", "fBodyGyro-meanY", "fBodyGyro-meanZ", "fBodyGyro-stdX", "fBodyGyro-stdY", "fBodyGyro-stdZ", "fBodyAccMag-mean", "fBodyAccMag-std", "fBodyBodyAccJerkMag-mean", "fBodyBodyAccJerkMag-std", "fBodyBodyGyroMag-mean", "fBodyBodyGyroMag-std", "fBodyBodyGyroJerkMag-mean", "fBodyBodyGyroJerkMag-std") 

#Tidy dataset calculating the average of each variable (n=66) for each activity (n=6) and each subject (n=30) 
install.packages("plyr")
library(plyr)
install.packages("reshape2")
library(reshape2)
meltedDat<-melt(dat, id.vars=c("Subject", "Activity"))
results<-ddply(meltedDat, .(Subject, Activity, variable), summarize, mean=mean(value))
write.table(results, file="/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data/UCI HAR Dataset/tidy_subjdata.txt",col.names=TRUE, row.name=FALSE)

#Load tidy UCI HAR data back into R
UCIdata<-read.table("/Users/kreynolds3/Documents/Coursera R Classes/Class 3-Getting and Cleaning Data/UCI HAR Dataset/tidy_subjdata.txt",header=TRUE)
