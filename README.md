Getting-and-Cleaning-Data-Project
=================================

---
title: "README.md"
author: "Kreynolds3"
date: "August 22, 2014"
---
Experiments were carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (Walking, Walking Upstairs, Walking Downstairs, Sitting, Standing, Laying) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity were captured  at a constant rate of 50Hz. The experiments were video-recorded to label the data manually. The obtained dataset had been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 
---

The data was downloaded and reformatted to produce a tidy data set reporting the means for each combination of subject, activity and variable. 

```{r}
#finding and downloading data
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./UCI HAR dataset", method="curl") #downloading file
```

Next, each part of the data was read in from the working directory where the files had been downloaded, including an object with data for the test sets (xtest), the activity labels (numerically coded 1-6) for the test set (ytest), and the ID of the subjects in the test set (subjtest). The training data was read in the same way (xtrain), with activity labels assigned to the object ytrain and the subject IDs assigned to subjtrain.

```{r}
#reading in data
xtest<-read.table("./UCI HAR Dataset/test/X_test.txt")
ytest<-read.table("./UCI HAR Dataset/test/y_test.txt")
subjtest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
xtrain<-read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain<-read.table("./UCI HAR Dataset/train/y_train.txt")
subjtrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")
```

The individual test and training datasets were created using the cbind function on the previously loaded objects which were each storing some portion of the data.

```{r}
#combining test and training data with subject and activity variables
testdata<-cbind(subjtest,ytest,xtest)
traindata<-cbind(subjtrain,ytrain,xtrain)
```

The training and test data were then combined into one data frame, which housed all of the data. This was subset to include only columns which had the mean or standard deviation for each of the 17 measurements (i.e. estimate variables) which will be later described in the codebook: tBodyAcc-XYZ, tGravityAcc-XYZ, tBodyAccJerk-XYZ, tBodyGyro-XYZ, tBodyGyroJerk-XYZ, tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag, fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccMag, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag as well as maintaining the columns that specified subject and activity. 

```{r}
#combining training and test data into one data frame and extracting columns only including mean/sd for each measurement
alldata<-rbind(traindata,testdata)
alldata2<-alldata[,c(1,2,3,4,5,6,7,8,43,44,45,46,47,48,83,84,85,86,87,88,123,124,125,126,127,128,163,164,165,166,167,168,203,204,216,217,229,230,242,243,255,256,268,269,270,271,272,273,347,348,349,350,351,352,426,427,428,429,430,431,505,506,518,519,531,532,544,545)]
```

Next the data were subset by activity and a separate column was added which described the name of each activity originally labeled numerically 1-6. All of the subset dataframes were then recombined into one data frame.

```{r}
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
```

After this column was added, the data frame was rearranged, dropping the numeric descriptor of the activity and moving the column with the new descriptive name next to the subject column in the data frame. 

```{r}
#rearranging data frame so activity name was in second column
dat<-alldatafinal[,c("V1","activity","V1.2","V2","V3","V4","V5","V6","V41","V42","V43","V44","V45","V46","V81","V82","V83","V84","V85","V86","V121","V122","V123","V124","V125","V126","V161","V162","V163","V164","V165","V166","V201","V202","V214","V215","V227","V228","V240","V241","V253","V254","V266","V267","V268","V269","V270","V271","V345","V346","V347","V348","V349","V350","V424","V425","V426","V427","V428","V429","V503","V504","V516","V517","V529","V530","V542","V543")]
```

Finally, a descriptive column name was added to all 68 columns so that the measured variables for each subject and activity were properly labeled. 

```{r}
#adding descriptive column names
colnames(dat) <- c("Subject","Activity"," tBodyAcc-meanX", "tBodyAcc-meanY", "tBodyAcc-meanZ", "tBodyAcc-stdX", "tBodyAcc-stdY", "tBodyAcc-stdZ", "tGravityAcc-meanX", "tGravityAcc-meanY", "tGravityAcc-meanZ", "tGravityAcc-stdX", "tGravityAcc-stdY", "tGravityAcc-stdZ", "tBodyAccJerk-meanX", "tBodyAccJerk-meanY", "tBodyAccJerk-meanZ", "tBodyAccJerk-stdX", "tBodyAccJerk-stdY", "tBodyAccJerk-stdZ", "tBodyGyro-meanX", "tBodyGyro-meanY", "tBodyGyro-meanZ", "tBodyGyro-stdX", "tBodyGyro-stdY", "tBodyGyro-stdZ", "tBodyGyroJerk-meanX", "tBodyGyroJerk-meanY", "tBodyGyroJerk-meanZ", "tBodyGyroJerk-stdX", "tBodyGyroJerk-stdY", "tBodyGyroJerk-stdZ", "tBodyAccMag-mean", "tBodyAccMag-std", "tGravityAccMag-mean", "tGravityAccMag-std", "tBodyAccJerkMag-mean", "tBodyAccJerkMag-std", "tBodyGyroMag-mean", "tBodyGyroMag-std", "tBodyGyroJerkMag-mean", "tBodyGyroJerkMag-std", "fBodyAcc-meanX", "fBodyAcc-meanY", "fBodyAcc-meanZ", "fBodyAcc-stdX", "fBodyAcc-stdY", "fBodyAcc-stdZ", "fBodyAccJerk-meanX", "fBodyAccJerk-meanY", "fBodyAccJerk-meanZ", "fBodyAccJerk-stdX", "fBodyAccJerk-stdY", "fBodyAccJerk-stdZ", "fBodyGyro-meanX", "fBodyGyro-meanY", "fBodyGyro-meanZ", "fBodyGyro-stdX", "fBodyGyro-stdY", "fBodyGyro-stdZ", "fBodyAccMag-mean", "fBodyAccMag-std", "fBodyBodyAccJerkMag-mean", "fBodyBodyAccJerkMag-std", "fBodyBodyGyroMag-mean", "fBodyBodyGyroMag-std", "fBodyBodyGyroJerkMag-mean", "fBodyBodyGyroJerkMag-std") 
```

Lastly, the packages *plyr* and *reshape2* were used to calculate the mean for each subject, activity, variable combination, which produced a tidy data set with 11,880 observations (rows) and 4 columns. Each row represented a unique combination of subject, activity and measured variable meaning there were 369 combinations for each of the 30 subjects totaling 11,880 observations of the mean of each unique combination. The tidy data set was written to a .txt document using the write.table function. 

```{r}
#Tidy dataset calculating the average of each variable (n=66) for each activity (n=6) and each subject (n=30) 
library(plyr)
library(reshape2)
meltedDat<-melt(dat, id.vars=c("Subject", "Activity"))
results<-ddply(meltedDat, .(Subject, Activity, variable), summarize, mean=mean(value))
write.table(results, file="./UCI HAR Dataset/tidy_subjdata.txt",col.names=TRUE, row.name=FALSE)
```

In the future, the tidy data set can be loaded back into R from the working directory using the read.table function.

```{r}
#Load tidy UCI HAR data back into R
UCIdata<-read.table("./UCI HAR Dataset/tidy_subjdata.txt",header=TRUE)
```
