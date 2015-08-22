# Loading necessary libraries
library("reshape2")
library("plyr")
library("dplyr")

# Loading the test and training data sets into data frames
ytest <- read.table("./rawdata/test/y_test.txt")
ytrain <- read.table("./rawdata/train/y_train.txt")
xtest <- read.table("./rawdata/test/X_test.txt")
xtrain <- read.table("./rawdata/train/X_train.txt")
trainsubject <- read.table("./rawdata/train/subject_train.txt")
testsubject <- read.table("./rawdata/test/subject_test.txt")

# Renaming columns to "Activity" and "ProbandID" and putting talbes together
ytest <- rename(ytest, Activity = V1)
testsubject <- rename(testsubject, ProbandID = V1)
testcases <- (cbind(xtest,ytest, testsubject))

ytrain <- rename(ytrain, Activity = V1)
trainsubject <- rename(trainsubject, ProbandID = V1)
traincases <- (cbind(xtrain,ytrain, trainsubject))

# combinging rows from test and train data sets
allcases <- rbind(testcases,traincases)

# reading the names of the measurements and marking those with 'mean' or 'std' in the name
features <- read.table("./rawdata/features.txt")
features$meanT <- ifelse(grepl("mean", features$V2), T, F)
features$stdT <- ifelse(grepl("std", features$V2), T, F)

# extracing relevant columns from joint table
allMeanStd <- select(allcases, as.integer(rownames(filter(features, meanT == T | stdT == T))))
allMeanStd <- cbind(allMeanStd, allcases$Activity, allcases$ProbandID)

# Renaming columns with measurement names
features <- filter(features, meanT == T | stdT == T)
labels <- features$V2
labels2 <- as.factor(c("Activity","ProbandID"))
labels <- c(as.character(labels), as.character(labels2))
names(allMeanStd) <- labels

# Renaming activity names
activityLabels <- read.table("./rawdata/activity_labels.txt")
allMeanStd <- mutate(allMeanStd, Activity = activityLabels$V2[Activity])

# write tidy data set to file 
write.table(allMeanStd, file="tidydata/step4_allMeanStd.txt", row.names = FALSE)

# copy tidy dataset 
allAv <- allMeanStd

# reorginize data set, so there is a row for each subject and activitiy per measure
allAvMelt <- melt(allAv, id=c("ProbandID","Activity"))
allAvMelt <- mutate(allAvMelt, value = as.numeric(value))

# seperating values per activity
walking_all <- filter(allAvMelt, Activity == "WALKING")
walking_upstairs_all <- filter(allAvMelt, Activity == "WALKING_UPSTAIRS")
walking_downstairs_all <- filter(allAvMelt, Activity == "WALKING_DOWNSTAIRS")
sitting_all <- filter(allAvMelt, Activity == "SITTING")
standing_all <- filter(allAvMelt, Activity == "STANDING")
laying_all <- filter(allAvMelt, Activity == "LAYING")

# calculating means per subject and measure
walking_all <- dcast(walking_all, ProbandID ~ variable, mean)
walking_all$Activity = "WALKING_ALL"

walking_upstairs_all <- dcast(walking_upstairs_all, ProbandID ~ variable, mean)
walking_upstairs_all$Activity = "WALKING_UPSTAIRS"

walking_downstairs_all <- dcast(walking_downstairs_all, ProbandID ~ variable, mean)
walking_downstairs_all$Activity = "WALKING_DOWNSTAIRS"

sitting_all <- dcast(sitting_all, ProbandID ~ variable, mean)
sitting_all$Activity = "SITTING"

standing_all <- dcast(standing_all, ProbandID ~ variable, mean)
standing_all$Activity = "STANDING"

laying_all <- dcast(laying_all, ProbandID ~ variable, mean)
laying_all$Activity = "LAYING"

# combinging results
average_all <- rbind(walking_all, walking_downstairs_all, walking_upstairs_all, sitting_all, standing_all, laying_all)

# write table to file
write.table(average_all, file="tidydata/step5_average_all.txt", row.names = FALSE)

# adding some additional metadata to facilitate CodeBook creation
attr(average_all, "source") <- "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"
attr(average_all, "license") <- "Use of this dataset in publications must be acknowledged by referencing the following publication: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012"
attr(average_all, "disclaimer") <- "This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited."

# creating a stub for the codebook 
codeBookStub <-  capture.output(str(average_all))
write(codeBookStub, file = "CodeBook.md")




