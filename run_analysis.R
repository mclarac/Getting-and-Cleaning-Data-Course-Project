#load require packages
library(dplyr); library(plyr)
library(reshape2)

#Download files and unzip
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = url, destfile = "Activity_Tracking_DataSet.zip")

#Set files paths

features_file <- "UCI HAR Dataset/features.txt"
activityLabls_file <- "UCI HAR Dataset/activity_labels.txt"

test_files <- c("UCI HAR Dataset/test/X_test.txt", 
                "UCI HAR Dataset/test/y_test.txt",
                "UCI HAR Dataset/test/subject_test.txt")

train_files <- c("UCI HAR Dataset/train/X_train.txt", 
                 "UCI HAR Dataset/train/y_train.txt",
                 "UCI HAR Dataset/train/subject_train.txt")

#Load files
features <- read.table(file = features_file)
activity_labels <- read.table(file = activityLabls_file)

test <- read.table(file = test_files[1], header = F)
test_labls <- read.table(file = test_files[2])
subject_test <- read.table(file = test_files[3])

train <- read.table(file = train_files[1], header = F)
train_labls <- read.table(file = train_files[2])
subject_train <- read.table(file = train_files[3])

#Merge train and test sets and set variables names
activity_tracking <- test %>% 
    rbind(train, make.row.names = TRUE) %>%  
    setNames(as.character(features$V2)) #here was also done step 4

#Extract measurements with mean and stantard deviation
##Identify which variables contains mean() or std() 
wanted_var <- grep("mean()|std()", 
                   names(activity_tracking), value = T)

meanFreq_var <- grep("meanFreq", wanted_var, value = T)
wanted_var <- wanted_var [! wanted_var %in% meanFreq_var]
##Note:I know this may not be the fanciest way to do this, but it is 
##how I managed to remove meanFreq() from the wanted variables

##Drop the unwanted variables from the data set
activity_tracking <- activity_tracking[,wanted_var]

#Create a vector with activities codes 
#Adds descriptive activity names 
activity <- inner_join(rbind(test_labls, train_labls),
                       activity_labels, by = "V1") %>% 
    mutate(V2 = factor(tolower(V2))) %>% 
    select(activity = V2)

activity_tracking <- cbind(activity, activity_tracking)

#Add subject variables to the data set
##Note: always test data was added first, followed by train data
subjects <- rbind(subject_test, subject_train) %>% 
    select(subjects = V1) %>% 
    mutate(subjects = factor(paste("subject", .$subject)))

activity_tracking <- cbind(subjects, activity_tracking)

#Summarizing data:
#average of each variable for each activity and each subject

##data were transformed in order to apply a function by 
##grouped variables more easily
meltData <- activity_tracking %>% 
    melt(id = c("subjects",
                "activity"))

summarizedData <- aggregate(value ~ subjects + activity + variable, 
                            data = meltData, FUN = "mean") %>% 
    select(subjects, activity, variable, mean = value) %>% 
    dcast(subjects + activity ~ variable)
