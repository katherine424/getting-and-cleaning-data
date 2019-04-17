

###############################################
#step 1 get data
###############################################
zipname <-"UCI HAR Dataset.zip"
zipUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <-"UCI HAR Dataset"
# download zip file containing data if not already downloaded
if(!file.exists(zipname))
{
  download.file(zipUrl,zipname,method = "curl",mode ="wb")
}

#unzip if not already 

if(!file.exists(filename))
{
  unzip(zipname)
}

###################################################
#step 2 read data 
###################################################

# don't convert text labels to factors using as.is = true
features<-read.table(file.path(filename,"features.txt"),as.is = TRUE)
#step find the index for mean and std
coltokeep <- grep("mean|std|activity|subject", features[,2])
# read only mean and std
trainsubject <-read.table(file.path(filename,"train","subject_train.txt"))
trainvalue <- read.table(file.path(filename,"train","x_train.txt"))[,coltokeep]
trainactivity <- read.table (file.path(filename,"train","y_train.txt"))

testsubject <-read.table(file.path(filename,"test","subject_test.txt"))
testvalue <- read.table(file.path(filename,"test","x_test.txt"))[,coltokeep]
testactivity <- read.table (file.path(filename,"test","y_test.txt"))

#read activity_labels 
activity_labels <- read.table (file.path(filename,"activity_labels.txt"))

#####################################################
#step 3 combine train and test data
#####################################################
activity <-rbind (
      cbind(trainactivity,trainsubject,trainvalue),
      cbind(testactivity,testsubject,testvalue)
)
##remove individual table to save memory 
rm(trainsubject,trainactivity,trainvalue,testactivity,testvalue,testsubject)

#########################################################
#step5 using descriptive activity names 
#########################################################

activity[,1] <- factor(activity[,1],levels = activity_labels[,1],labels = activity_labels[,2])

#########################################################
#step6 using descriptive colnames 
#########################################################

activitycols<-features[coltokeep,2]

activitycols <-gsub("[()-]", "",activitycols)
activitycols<-gsub("mean","Mean",activitycols)
activitycols <-gsub("std","StandardDeviation",activitycols)
activitycols <-gsub("Mag","Magnitude",activitycols)
activitycols <-gsub("Freq","Frequency",activitycols)
activitycols <-gsub("Gyro","Gyroscope",activitycols)
activitycols <-gsub("BodyBody","Body",activitycols)
activitycols <-gsub("^t","TimeDomain",activitycols)
activitycols <-gsub("^f","FrequencyDomain",activitycols)

colnames(activity) <- c("activity","subject",activitycols)

#########################################################
#step7-1 creat a second independent tidy set using dplyr library 
#########################################################

library(dplyr)

activitymean<- activity %>%
      group_by(activity,subject) %>%
      summarize_each(funs(mean))

write.table(activitymean,"tidy_data.txt",row.names = FALSE,quote = FALSE)


#########################################################
#step7-2 creat a second independent tidy set using reshape2 library 
#########################################################

#library(reshape2)
#activitymelt <- melt(activity,id=c("subject","activity"))
#activitymeasn <- dcast(activitymelt,subject+activity~variable,mean)
#write.table(activitymean,"tidy_data.txt",row.names = FALSE,quote = FALSE)



