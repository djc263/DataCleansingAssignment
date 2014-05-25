## This script requires plyr, which checks if functions are installed


is.installed <- function(mypkg){
     is.element(mypkg, installed.packages()[,1])
} 

##sends plyr to the test function for is installed
##if plyr is installed prints a message
if (is.installed("plyr")) {
  print("plyr installed")
} else {
  install.packages("plyr")
}
## if plyr was not installed, it is now

##library call to invoke plyr
library(plyr)

##read in and bind the test and train observation sets from working directory 
XTest<-read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="")
XTrain<-read.delim("./UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="")
Xdata<-rbind(XTrain,XTest)
##read in and bind the test and train activity sets from wd
YTest<-read.delim("./UCI HAR Dataset/test/Y_test.txt",header=FALSE,sep="")
YTrain<-read.delim("./UCI HAR Dataset/train/Y_train.txt",header=FALSE,sep="")
Activity<-rbind(YTrain,YTest)
colnames(Activity)<-"activity"
##gives a column name to activity



##this reads in the proper names of the activities as a dataset and
##names these columns for future use
activity<-read.delim("./UCI HAR Dataset/activity_labels.txt",header=FALSE,sep="")
colnames(activity)<-c("Activity","activityname")

##this reads in and binds the data on which subjects were in test and train rows
SubjectTrain<-read.delim("./UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="")
SubjectTest<-read.delim("./UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="")
Subjectdata<-rbind(SubjectTrain,SubjectTest)

##this reads in the name of the features to be applied to the data set
features<-read.delim("./UCI HAR Dataset/features.txt",header=FALSE,sep="")

##This processes the names to make them more readable and more tidy data compliant
names<-features[,2]
names2<-gsub("-","",names)
names3<-gsub("\\(","",names2)
names4<-gsub("\\)","",names3)
names5<-gsub("\\,","",names4)
names6<-tolower(names5)

## this section pulls out those variables that ahve to do with mean or std for further processing
xgre<-grep("mean",names6)
ygre<-grep("std",names6)
cgre<-c(xgre,ygre)

##assigns column names to the original data based on the feature set
colnames(Xdata)<-names5
##assigns a column name to the subject data set
colnames(Subjectdata)<-c("TestSubject")

##merges activity numbers and subject numbers to full data set and sets aside
##assignment was unclear it sounded like I should do this
XZ<-cbind(Xdata,Activity)
XZ<-cbind(XZ,Subjectdata)

##seperates out the observation X data based on the variables for mean or std

Xspc<-Xdata[,cgre]

##binds mean or std variables to the activity and subject data

XY<-cbind(Xspc,Activity)
XY<-cbind(XY,Subjectdata)

##summarizes new data set to averages by activity and subject

out1<-ddply(XY,.(activity,TestSubject),numcolwise(mean))

##merges proper activity labels
out2<-merge(out1,activity,by.x="activity",by.y="Activity")
##removes extra activity variable per tidy 
out3<-out2[,-out2$activity]
##writes data set to a file

write.csv(out3,"./DataCleaningProject.csv")


