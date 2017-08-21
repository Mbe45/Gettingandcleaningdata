###### this script is about the curse
###### project in getting and cleaning data,
###### which is part of the Data Science Specialization
##### offer by JHU through Coursera Platform
##### 19/05/2017
####Mbecho Techago Emmanuel


###The task to perform

###1Merges the training and the test sets to create one data set.

###2 Extracts only the measurements on the mean 
###and standard deviation for each measurement.

###3 Uses descriptive activity names to name
###the activities in the data set


###4 Appropriately labels the data set with
###descriptive variable names.


###5From the data set in step 4, creates a second,
###independent tidy data set with the average 
###of each variable for each activity and each subject.

###Approach:
###loading the traing dataset.

train <- read.table("Getting_&_cleaning_dat/UCI HAR Dataset/train/X_train.txt")

###looking at a fraction of the dataset 
##by selecting some fews rows and columns


##reading the subject_id


subject_ID <- read.table("Getting_&_cleaning_dat/UCI HAR Dataset/train/subject_train.txt")



###reading the activity lebels 

y_train <- read.table("Getting_&_cleaning_dat/UCI HAR Dataset/train/y_train.txt")

  
###binding the subject id with the train label set(y_train)

subject_ID <- cbind(subject_ID, y_train)


###It is very important to give a meaniful
###naming to our variables for both ease of
##understanding when going through the codes
##and also when explaining to another person.
##the dataset currently, does not  have a descriptive
##names. we will give it a descriptive one.


##the features contains the names of the variables.

feature <- read.table("Getting_&_cleaning_dat/UCI HAR Dataset/features.txt",
                      header = F)

###using the names from feature to set the variables
###names in the trainingset.

col <- feature$V2

colnames(train) <- col


###binding the subject ID to the training set

train  <- cbind(subject_ID, train)

names(train)[1:2] <- c("subject_ID", "activity")




#################################################################################
##moving to the test dataset.

##the same procedure as above is followed.


###loading the test dataset.

test <- read.table("Getting_&_cleaning_dat/UCI HAR Dataset/test/X_test.txt")

###looking at a fraction of the dataset 
##by selecting some fews rows and columns



##reading the subject_id


subject_ID1 <- read.table("Getting_&_cleaning_dat/UCI HAR Dataset/test/subject_test.txt")



###reading the test label (y_text) 

y_test <- read.table("Getting_&_cleaning_dat/UCI HAR Dataset/test/y_test.txt")

###binding the subject id with the test labels

subject_ID1 <- cbind(subject_ID1, y_test)


###It is very important to give a meaniful
###naming to our variables for both ease of
##understanding when going through the codes
##3nd also when explaining to another persn.
##the dataset currently, does not  have a descriptive
##names. we will give it a descriptive one.


##the features contains the names of the variables.

feature <- read.table("Getting_&_cleaning_dat/UCI HAR Dataset/features.txt",
                      header = F)

###using the names from feature to set the variables
###names in the trainingset.

col <- feature$V2

colnames(test) <- col


###binding the subject ID to the training set

test  <- cbind(subject_ID1, test)

names(test)[1:2] <- c("subject_ID", "activity")




##column bind the test and the training data together

mergedata <- rbind(train, test)



####Extracts only the measurements on the
##mean and standard deviation for each measurement.
###by using regular expression function grep()


mean_std_measurement <- mergedata[grep("(subject_ID|activity|mean()|std())",names(mergedata))]



##labelling the activity variable
##using the factor function

mean_std_measurement$activity <- factor(mean_std_measurement$activity,
                             levels = c(1,2,3,4,5,6),
                             labels = c("WALKING",
                                        "WALKING_UPSTAIRS",
                                        "WALKING_DOWNSTAIRS",
                                        "SITTING",
                                        "STANDING",
                                        "LAYING"
                             ))



#############################################
###creating the tidy_data set which
###the average of each variable for each activity and each subject.

###loading dply

library(dplyr)


tidy_data <- mean_std_measurement %>%
  group_by(activity, subject_ID) %>%
  summarise_all(mean)


write.table(tidy_data, "tidy_data.txt", row.names = F)

