run_analysis <- function(trainfile, testfile) {
	
	## 'trainfile' is the file containing the training dataset (7352 observations, 563 variables)
  ## 'testfile' is the file containing the test dataset (2947 observations, 563 variables)
	
	## this program combines the training and test data files to make one file,
	## selects out the variables that are means and SDs for each measurement,
	## calculates the average of these values for each activity and subject,
  ## and returns a summary file in "tidy data" format with descriptive activity names

  ## combine two data files into one
  bigfile <- rbind(trainfile, testfile)
  
  ## pull out relevant columns
  smallfile <- bigfile[, c(1:8, 43:48, 83:88, 123:128, 163:168, 203:204, 216:217, 229:230, 242:243, 255:256, 268:273, 347:352, 426:431, 505:506, 518:519, 531:532, 544:545)]
  
  ## calculate means by activity/subject
  meanfile <- aggregate(smallfile, by=list(smallfile$subject, smallfile$activity), mean)

  ## clean up resulting file to contain descriptive activity names, variable names
  cleanfile <- meanfile[, c(3:70)]
  cleanfile$activity <- as.factor(cleanfile$activity)
  levels(cleanfile$activity) <- c("Walking", "WalkingUpstairs", "WalkingDownstairs", "Sitting", "Standing", "Laying")
  colnames(cleanfile) <- c("Subject", "Activity", "Mean.tBodyAccX.Mean", "Mean.tBodyAccY.Mean", "Mean.tBodyAccZ.Mean", "Mean.tBodyAccX.SD", "Mean.tBodyAccY.SD", "Mean.tBodyAccZ.SD",
                           "Mean.tGravityAccX.Mean", "Mean.tGravityAccY.Mean", "Mean.tGravityAccZ.Mean", "Mean.tGravityAccX.SD", "Mean.tGravityAccY.SD", "Mean.tGravityAccZ.SD",
                           "Mean.tBodyAccJerkX.Mean", "Mean.tBodyAccJerkY.Mean", "Mean.tBodyAccJerkZ.Mean", "Mean.tBodyAccJerkX.SD", "Mean.tBodyAccJerkY.SD", "Mean.tBodyAccJerkZ.SD",
                           "Mean.tBodyGyroX.Mean", "Mean.tBodyGyroY.Mean", "Mean.tBodyGyroZ.Mean", "Mean.tBodyGyroX.SD", "Mean.tBodyGyroY.SD", "Mean.tBodyGyroZ.SD",
                           "Mean.tBodyGyroJerkX.Mean", "Mean.tBodyGyroJerkY.Mean", "Mean.tBodyGyroJerkZ.Mean", "Mean.tBodyGyroJerkX.SD", "Mean.tBodyGyroJerkY.SD", "Mean.tBodyGyroJerkZ.SD",
                           "Mean.tBodyAccMag.Mean", "Mean.tBodyAccMag.SD", "Mean.tGravityAccMag.Mean", "Mean.tGravityAccMag.SD", "Mean.tBodyAccJerkMag.Mean", "Mean.tBodyAccJerkMag.SD", 
                           "Mean.tBodyGyroMag.Mean", "Mean.tBodyGyroMag.SD", "Mean.tBodyGyroJerkMag.Mean", "Mean.tBodyGyroJerkMag.SD",
                           "Mean.fBodyAccX.Mean", "Mean.fBodyAccY.Mean", "Mean.fBodyAccZ.Mean", "Mean.fBodyAccX.SD", "Mean.fBodyAccY.SD", "Mean.fBodyAccZ.SD",
                           "Mean.fBodyAccJerkX.Mean", "Mean.fBodyAccJerkY.Mean", "Mean.fBodyAccJerkZ.Mean", "Mean.fBodyAccJerkX.SD", "Mean.fBodyAccJerkY.SD", "Mean.fBodyAccJerkZ.SD",
                           "Mean.fBodyGyroX.Mean", "Mean.fBodyGyroY.Mean", "Mean.fBodyGyroZ.Mean", "Mean.fBodyGyroX.SD", "Mean.fBodyGyroY.SD", "Mean.fBodyGyroZ.SD",
                           "Mean.fBodyAccMag.Mean", "Mean.fBodyAccMag.SD", "Mean.fBodyAccJerkMag.Mean", "Mean.fBodyAccJerkMag.SD", "Mean.fBodyGyroMag.Mean", "Mean.fBodyGyroMag.SD", "Mean.fBodyGyroJerkMag.Mean", "Mean.fBodyGyroJerkMag.SD")
  
  return(cleanfile)
  
}