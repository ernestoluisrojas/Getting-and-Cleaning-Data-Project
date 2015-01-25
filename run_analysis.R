run_analysis <- function(){

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
  df_complete = rbind(df_test, df_train)
  
  return(df_complete)
}