#####
#setwd("E:/Cousera_DataAnalysis/03_GettingData/Project/getdata-projectfiles-UCI HAR Dataset")


#finds the test and training sets and merge them into df_full
all_files <- list.files(recursive =  TRUE)
test_set_path <- all_files[grep("X_test.txt", all_files)]
train_set_path <- all_files[grep("X_train.txt", all_files)]

df_test <- read.table(test_set_path)
df_train <- read.table(train_set_path)

df_full <- rbind(df_test, df_train)

#gets mean and std table indexes from features.txt file
features_path <- all_files[grep("features.txt", all_files)]
df_features <- read.table(features_path)
std_index <- df_features[grep("std()", as.character(df_features[[2]])),]
mean_index <- df_features[grep("mean", as.character(df_features[[2]])),]

#extracts only std and mean variables and creates a new df
df_mean_std <- df_full[,c(std_index[[2]],mean_index[[2]])]

#finds and merges activities labels to df
activity_labels_path <- all_files[grep("activity_labels.txt", all_files)]
df_activity_labels <- read.table(activity_labels_path)

test_activity_labels_path <- all_files[grep("/test/y_test.txt", all_files)]
df_test_activity_labels <- read.table(test_activity_labels_path)

train_activity_labels_path <- all_files[grep("/train/y_train.txt", all_files)]
df_train_activity_labels <- read.table(train_activity_labels_path)

df_activities <- rbind(df_test_activity_labels, df_train_activity_labels)
df_activities <- join(df_activities, df_activity_labels)

df_activities <- cbind(df_activities[2], df_mean_std)

#rename df columns with activities_labels
names(df_activities)[1]<- "activity"

x <- 1
for (n in std_index[[2]]){
  names(df_activities)[x+1]<-n
  x = x + 1
}

for (n in mean_index[[2]]){
  names(df_activities)[x+1]<-n
  x = x + 1
}

#insert subjects into df
subject_test_path <- all_files[grep("subject_test.txt", all_files)]
df_subject_test <- read.table(subject_test_path)

subject_train_path <- all_files[grep("subject_train.txt", all_files)]
df_subject_train <- read.table(subject_train_path)

df_subject <- rbind(df_subject_test, df_subject_train)
df_subject <- cbind(df_subject, df_activities)
names(df_subject)[1]<- "subject"

#creates an independent tidy data set with the average of each variable for each activity and each subject.
aggdata <-aggregate(df_subject[-c(1:2)], by=list(df_subject$subject,df_subject$activity), 
                    FUN=mean, na.rm=TRUE)
names(aggdata)[1]<- "subject"
names(aggdata)[2]<- "activity"

write.table(aggdata, file="tidy_data_set.txt", row.names = FALSE)
