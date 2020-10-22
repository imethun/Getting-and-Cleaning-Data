
## read the link of data set 
fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# download the file and store on workspace 
download.file(fileurl, "./data/dataset.zip")

# unzip the datasets
unzip("./data/dataset.zip",exdir = "./data")

# check the file in unzip folder 
list.files("./data/UCI HAR Dataset")

# check the file in test and train folder 
list.files("./data/UCI HAR Dataset/train")
list.files("./data/UCI HAR Dataset/test")


# Read datasets 
activity_labels<-read.table("./data/UCI HAR Dataset/activity_labels.txt")
features<-read.table("./data/UCI HAR Dataset/features.txt")
subject_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")
x_train<-read.table("./data/UCI HAR Dataset/train/x_train.txt")
y_train<-read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt")
x_test<-read.table("./data/UCI HAR Dataset/test/x_test.txt")
y_test<-read.table("./data/UCI HAR Dataset/test/y_test.txt")

# add column name 
head(activity_labels)
colnames(activity_labels)<-c("activityCode", "activityName")
head(activity_labels)
head(features)
colnames(features)<-c("featureNo", "feature")
head(features)
head(subject_train)
colnames(subject_test)<-"subject"
colnames(subject_train)<-"subject"
colnames(x_train)<-features$feature
colnames(x_test)<-features$feature
colnames(y_test)<-"code"
colnames(y_train)<-"code"


#1. Merges the training and the test sets to create one data set

## understanding the data sets 
head(subject_train)
head(x_train)
dim(x_train)
dim(y_train)
head(y_train)

# merge data sets 
train<-cbind(subject_train,x_train,y_train)
test<-cbind(subject_test,x_test,y_test)
merged_data<-rbind(train,test)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement
tidy_data<-merged_data%>%select(subject,code,contains("mean"),contains("std"))

# 3.Uses descriptive activity names to name the activities in the data set
tidy_data$code<-activity_labels[tidy_data$code,2]


#-----------------------------------------------------------------------
# 4.Appropriately labels the data set with descriptive variable names.
#---------------------------------------------------------------------------

# assign all column title of tidy_data to a vector called name 
name<-names(tidy_data)

# change the element of name
name[2]<-"activity"
name<-gsub("Acc", "Accelerometer ", name)
name<-gsub("Gyro", "Gyroscope ", name)
name<-gsub("BodyBody", "Body", name)
name<-gsub("Body", "Body ", name)
name<-gsub("Mag", "Magnitude ", name)
name<-gsub("^t", "Time ", name)
name<-gsub("^f", "Frequency ", name)
name<-gsub("tBody", "TimeBody ", name)
name<-gsub("-mean()", "Mean ", name, ignore.case = TRUE)
name<-gsub("-std()", "STD ", name, ignore.case = TRUE)
name<-gsub("-freq()", "Frequency ", name, ignore.case = TRUE)
name<-gsub("angle", "Angle ", name)
name<-gsub("gravity", "Gravity ", name)
names(tidy_data)<-name

#-------------------------------------------------------------------------

# 5.independent tidy data set with the average of each variable for each activity and each subject.

#-------------------------------------------------------------------------

finalData<-tidy_data%>%
  group_by(subject,activity)%>%
  summarise_all(funs(mean))

write.table(finalData, "finalData.txt", row.names = FALSE)
