Documentation for Course Project: Getting and Cleaning Data
===================


Program run_analysis.R
----------------------
This program performs the following functions:
* Combines training and test datasets to form a single dataset
* Extracts the columns containing Means and SDs for 33 different measurement variables
* Calculates the average of each of these 66 variables separately for each subject (n=30) and activity (n=6)
* Creates a resulting _tidy dataset_ that has the following structure:
  * 180 observations, one for each combination of subject and activity
  * 68 variables, listed in Codebook below
  * the first two variables are Identifiers -- Subject and Activity
  * the other 66 variables are calculated means for measurement variables
  * this data is Tidy because each measurement variable has its own column, each observation (subject/activity) has its own row, and it is one cohesive table containing a single kind of data (means of activity measurements)
  * the output data has been saved as a text file names ProjectTidy.txt, described below


Codebook: ProjectTidy.Txt
-------------------------
  ### Identifying Variables (2)
  *Subject: Subject ID number, ranges from 1 to 30
  *Activity: One of six activities (Walking, WalkingUpstairs, WalkingDownstairs, Sitting, Standing, Laying

  ### Summary Variables (66)
  *The following variables follow the same naming convention: Mean.<measurement>.Mean -- mean of means; Mean.<measurement>.SD -- mean of SDs
  *Mean.tBodyAccX.Mean
  *Mean.tBodyAccY.Mean
  *Mean.tBodyAccZ.Mean
  *Mean.tBodyAccX.SD
  *Mean.tBodyAccY.SD
  *Mean.tBodyAccZ.SD
  *Mean.tGravityAccX.Mean
  *Mean.tGravityAccY.Mean
  *Mean.tGravityAccZ.Mean
  *Mean.tGravityAccX.SD
  *Mean.tGravityAccY.SD
  *Mean.tGravityAccZ.SD
  *Mean.tBodyAccJerkX.Mean
  *Mean.tBodyAccJerkY.Mean
  *Mean.tBodyAccJerkZ.Mean
  *Mean.tBodyAccJerkX.SD
  *Mean.tBodyAccJerkY.SD
  *Mean.tBodyAccJerkZ.SD
  *Mean.tBodyGyroX.Mean
  *Mean.tBodyGyroY.Mean
  *Mean.tBodyGyroZ.Mean
  *Mean.tBodyGyroX.SD
  *Mean.tBodyGyroY.SD
  *Mean.tBodyGyroZ.SD
  *Mean.tBodyGyroJerkX.Mean
  *Mean.tBodyGyroJerkY.Mean
  *Mean.tBodyGyroJerkZ.Mean
  *Mean.tBodyGyroJerkX.SD
  *Mean.tBodyGyroJerkY.SD
  *Mean.tBodyGyroJerkZ.SD
  *Mean.tBodyAccMag.Mean
  *Mean.tBodyAccMag.SD
  *Mean.tGravityAccMag.Mean
  *Mean.tGravityAccMag.SD
  *Mean.tBodyAccJerkMag.Mean
  *Mean.tBodyAccJerkMag.SD 
  *Mean.tBodyGyroMag.Mean
  *Mean.tBodyGyroMag.SD
  *Mean.tBodyGyroJerkMag.Mean
  *Mean.tBodyGyroJerkMag.SD
  *Mean.fBodyAccX.Mean
  *Mean.fBodyAccY.Mean
  *Mean.fBodyAccZ.Mean
  *Mean.fBodyAccX.SD
  *Mean.fBodyAccY.SD
  *Mean.fBodyAccZ.SD
  *Mean.fBodyAccJerkX.Mean
  *Mean.fBodyAccJerkY.Mean
  *Mean.fBodyAccJerkZ.Mean
  *Mean.fBodyAccJerkX.SD
  *Mean.fBodyAccJerkY.SD
  *Mean.fBodyAccJerkZ.SD
  *Mean.fBodyGyroX.Mean
  *Mean.fBodyGyroY.Mean
  *Mean.fBodyGyroZ.Mean
  *Mean.fBodyGyroX.SD
  *Mean.fBodyGyroY.SD
  *Mean.fBodyGyroZ.SD
  *Mean.fBodyAccMag.Mean
  *Mean.fBodyAccMag.SD
  *Mean.fBodyAccJerkMag.Mean
  *Mean.fBodyAccJerkMag.SD
  *Mean.fBodyGyroMag.Mean
  *Mean.fBodyGyroMag.SD
  *Mean.fBodyGyroJerkMag.Mean
  *Mean.fBodyGyroJerkMag.SD
  