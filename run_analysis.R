#########################################################################################################
## Program Name:    runAnalysis.R
## Purpose:         Assignment: Getting and Cleaning Data Course Project  
## Author:          Jason Peng 

## Output:          final_UCI_HAR_dataset, avg_UCI_HAR_dataset -> tidy_dataset.txt
## Date:            Update
##------------------------------------------------------------------------------------------------------
## 2016-06-02      Initial create                                                                                                                                                                                                                 

## Description
##------------------------------------------------------------------------------------------------------
# One is expected to create one R script called run_analysis.R that does the following.
# 1. Merges the training and the test sets to create one data set
# 2. Extracts only the measurements on the mean and standard deviation for each measurement
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names
# 5. From the data set in step 4, creates a second, independent tidy data set 
#     with the average of each variable for each activity and each subject                                                                                                                                                                                                

## Extra notes
##------------------------------------------------------------------------------------------------------
##  PLEASE READ!!!!!               PLEASE READ!!!!!           PLEASE READ!!!!!

##  I enjoy using                   "=" 
##  as opposed to                   "<-"
##  I also like to add              ";" at the end of the clause
##  due to old habits from other programming languages
##  "=" and "<-" are interchangeable in most cases, ";" is harmless and enhance readability
##  Please don't take points off my program just because of that!!!  Many thanks!!!

#########################################################################################################                                                                                                                    

##------------------------------------------------------------------------------------------------------
# 0: prep work
##------------------------------------------------------------------------------------------------------
#### Attach libraries
library(dplyr);

#### Download and unzip data
if (!file.exists("./data")) {dir.create("./data")}
fileURL = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip";
zipped_file = "./data/getdata_projectfiles_UCI HAR Dataset.zip";
download.file(fileURL, destfile = zipped_file);
unzip(zipped_file, exdir = "./data");  # entire "UCI HAR Dataset" folder is created under ./data
unlink(zipped_file);

#### Read in the raw data
# - 'features_info.txt': Shows information about the variables used on the feature vector.
# - 'features.txt': List of all features.
# - 'activity_labels.txt': Links the class labels with their activity name.
# - 'train/X_train.txt': Training set.
# - 'train/y_train.txt': Training labels.
# - 'test/X_test.txt': Test set.
# - 'test/y_test.txt': Test labels.
activity_labels = read.table("./data/UCI HAR Dataset/activity_labels.txt");
features        = read.table("./data/UCI HAR Dataset/features.txt");
subject_train   = read.table("./data/UCI HAR Dataset/train/subject_train.txt");
x_train         = read.table("./data/UCI HAR Dataset/train/X_train.txt");
y_train         = read.table("./data/UCI HAR Dataset/train/y_train.txt");
subject_test    = read.table("./data/UCI HAR Dataset/test/subject_test.txt");
x_test          = read.table("./data/UCI HAR Dataset/test/X_test.txt");
y_test          = read.table("./data/UCI HAR Dataset/test/y_test.txt");

#### Investigate the data read in
# It turns out that activity_labels is a look up table for y on activity 
# Features contains variables for x
# subject, x and y in trainning/test sets should be column combined
#### uncomment the following codes using R studio to test the resulting data
# head(activity_labels);
# head(features);
# head(subject_train);
# head(x_train);
# head(y_train);
# head(subject_test);
# head(x_test);
# head(y_test);
# str(activity_labels);
# str(features);
# str(subject_train);
# str(x_train);
# str(y_train);
# str(subject_test);
# str(x_test);
# str(y_test);

#### Assign appropriate names
colnames(activity_labels)   = c('activity_ID','activity_labels');                                                                                                                                                                        
colnames(subject_train)     = "subject_ID";                                                                                                                                                                                           
colnames(x_train)           = features[,2];                                                                                                                                                                                          
colnames(y_train)           = "activity_ID";
colnames(subject_test )     = "subject_ID";                                                                                                                                                                                           
colnames(x_test)            = features[,2];                                                                                                                                                                                          
colnames(y_test)            = "activity_ID";

##------------------------------------------------------------------------------------------------------
# 1. Merges the training and the test sets to create one data set.
# (For future uses, the variable study_group is specifically created to retain information on 
#  which study group the subject belong to)
##------------------------------------------------------------------------------------------------------
train_dataset = cbind(subject_train, x_train, y_train);
train_dataset$study_group = "train";
test_dataset = cbind(subject_test, x_test, y_test);
test_dataset$study_group = "test";
combined_UCI_HAR_dataset = rbind(train_dataset, test_dataset);

#### Investigate the data read in
#### uncomment the following codes using R studio to test the resulting data
# table(combined_UCI_HAR_dataset$study_group);
# length(unique(combined_UCI_HAR_dataset$subject_ID));
# length(unique(combined_UCI_HAR_dataset$activity_ID));

##------------------------------------------------------------------------------------------------------
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# (For future uses, these additional variables are remained intentionally:
#  subject_ID, activity_ID, study_group.)

#  !!!! Please note that my intepretation is that we shouldn't include meanFreq() variables (13 in total)
#  according to "features_info.txt", mean(), std() should be included, but meanFreq() should not
# mean(): Mean value
# std(): Standard deviation
# meanFreq(): Weighted average of the frequency components to obtain a mean frequency
##------------------------------------------------------------------------------------------------------
var_names = names(combined_UCI_HAR_dataset);
#  (grepl("mean", var_names) & !grepl("meanFreq", var_names)) equivalent to grepl("mean\\()", var_names) 
var_names_to_keep_logical_vector = grepl("^subject_ID$", var_names) | grepl("^activity_ID$", var_names) | 
    grepl("^study_group$", var_names) | 
    grepl("mean\\()", var_names)  | grepl("std\\()", var_names);
final_UCI_HAR_dataset = combined_UCI_HAR_dataset[var_names_to_keep_logical_vector==TRUE];

##------------------------------------------------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set
##------------------------------------------------------------------------------------------------------
## Use a bit of redundancy here: 
## first to join with actifity_labels by activity_id, which will add "activity_labels" variable 
## secondly turn activity_id as factor (useful for the last step)

final_UCI_HAR_dataset = left_join(final_UCI_HAR_dataset, activity_labels, by = "activity_ID" );
final_UCI_HAR_dataset$activity_ID = factor(final_UCI_HAR_dataset$activity_ID, 
                                           levels = activity_labels[,"activity_ID"], 
                                           labels = activity_labels[,"activity_labels"]);
#### uncomment the following codes using R studio to test the resulting data
# final_var_names;
# table(final_UCI_HAR_dataset$activity_labels)
# table(final_UCI_HAR_dataset$activity_ID)

##------------------------------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names
##------------------------------------------------------------------------------------------------------
final_var_names = names(final_UCI_HAR_dataset);

#### Using information from feature_info.txt to make the variable names more informative
for (i in seq_along(final_var_names)){
    final_var_names[i] = gsub("\\()","",final_var_names[i]);
    final_var_names[i] = gsub("^t","TimeDomainSignal-",final_var_names[i]);
    final_var_names[i] = gsub("^f","FrequencyDomainSignal-",final_var_names[i]);
    final_var_names[i] = gsub("mean","Mean",final_var_names[i]);
    final_var_names[i] = gsub("std","StandardDeviation",final_var_names[i]);
    final_var_names[i] = gsub("-X$", "-XDirection",final_var_names[i]);
    final_var_names[i] = gsub("-Y$", "-YDirection",final_var_names[i]);
    final_var_names[i] = gsub("-Z$", "-ZDirection",final_var_names[i]);
    final_var_names[i] = gsub("Acc", "LinearAcceleration",final_var_names[i]);
    final_var_names[i] = gsub("Gyro", "AngularVelocity",final_var_names[i]);
    final_var_names[i] = gsub("Mag", "Magnitude",final_var_names[i]);
};

colnames(final_UCI_HAR_dataset) = final_var_names;
#### uncomment the following codes using R studio to test the resulting data
#### variable names much more informative now
# head(colnames(final_UCI_HAR_dataset));
# [1] "subject_ID"                                                                     
# [2] "TimeDomainSignal-BodyLinearAcceleration-Mean-XDirection"                             
# [3] "TimeDomainSignal-BodyLinearAcceleration-Mean-YDirection"                             
# [4] "TimeDomainSignal-BodyLinearAcceleration-Mean-ZDirection"                             
# [5] "TimeDomainSignal-BodyLinearAcceleration-StandardDeviation-XDirection" 
# [6] "TimeDomainSignal-BodyLinearAcceleration-StandardDeviation-YDirection"
# tail(colnames(final_UCI_HAR_dataset),5);
# [1] "FrequencyDomainSignal-BodyBodyAngularVelocityJerkMagnitude-Mean"             
# [2] "FrequencyDomainSignal-BodyBodyAngularVelocityJerkMagnitude-StandardDeviation"
# [3] "activity_ID"                                                                 
# [4] "study_group"                                                                 
# [5] "activity_labels"

##------------------------------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, 
#    independent tidy data set with the average of each variable for each activity and each subject.
##------------------------------------------------------------------------------------------------------
avg_UCI_HAR_dataset =   select(final_UCI_HAR_dataset, -activity_labels, -study_group) %>% 
                        group_by(subject_ID, activity_ID) %>% 
                        summarise_each(funs(mean));

#### Tidy dataset output                        
write.table(avg_UCI_HAR_dataset, './data/tidy_dataset.txt', row.names=FALSE);

#### preparing for the codebook
tidy_dataset_names = names(avg_UCI_HAR_dataset);
write.table(tidy_dataset_names, './data/tidy_dataset_codebook.txt', row.names=FALSE, quote=FALSE);
##------------------------------------------------------------------------------------------------------
#                               THE END -- THANKS FOR READING
##------------------------------------------------------------------------------------------------------