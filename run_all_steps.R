run_all_steps <- function(){
  
  #This script returns a tidy data set that include the average of each 
  #mean and std variable from the "Human Activity Recognition Using Smartphones Dataset
  #Version 1.0"
  
  
  #function to find a file and returns a data frame
  find_file <- function(setname){
    all_files <- list.files(getwd(), recursive =  TRUE)
    set_path <- all_files[grep(setname, all_files)]  
    df <- read.table(set_path)
    return(df)
  }
  
  
  #(1)finds and merges test and train sets
  df_test = find_file("X_test.txt")
  df_train = find_file("X_train.txt")
  df_complete = rbind(dfset1, dfset2)
  
  #gets mean and std table indexes from features.txt file
  df_features = find_set("features.txt")
  std_index <- df_features[grep("std()", as.character(df_features[[2]])),]
  mean_index <- df_features[grep("mean", as.character(df_features[[2]])),]
  
  #(2)extracts only std and mean variables and creates a new df
  df_mean_std <- df_complete[,c(std_index[[2]],mean_index[[2]])]
  
  #finds and merges activities labels to df
  df_activity_labels = find_file("activity_labels.txt")
  df_test_activity_labels = find_file("/test/y_test.txt")
  df_train_activity_labels = find_file("/train/y_train.txt")
  
  #(3)merges both activity_label sets and matches labels to activity
  df_activities <- rbind(df_test_activity_labels, df_train_activity_labels)
  df_activities <- join(df_activities, df_activity_labels)
  
  #adds activities to working df
  df_activities <- cbind(df_activities[2], df_mean_std)
  
  #(4)adds labels to variables
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
  
  #(5)insert subjects into df
  df_subject_test = find_file("subject_test.txt")
  df_subject_train = find_file("subject_train.txt")
  
  df_subject <- rbind(df_subject_test, df_subject_train)
  df_subject <- cbind(df_subject, df_activities)
  names(df_subject)[1]<- "subject"
  
  #creates an independent tidy data set with the average of each variable for each activity and each subject.
  aggdata <-aggregate(df_subject[-c(1:2)], by=list(df_subject$subject,df_subject$activity), 
                      FUN=mean, na.rm=TRUE)
  names(aggdata)[1]<- "subject"
  names(aggdata)[2]<- "activity"
  
  return(aggdata)
}