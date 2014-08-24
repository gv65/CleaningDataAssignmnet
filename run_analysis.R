## ------------------------Beginning of the project assignment--------------------
## Downloading of the data from the course website and storing in a local directory

	fileName = "D:\\Courses\\GettingAndCleaningData\\Data\\getdata_projectfiles_UCI HAR Dataset.zip"
	if (!file.exists(fileName)) {
		downloadedFile = download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
		destfile = fileName)
		unzip(fileName,files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE)
	}
	

## Assumed "plyr" package is installed before loading of the package
## Loading of "plyr" package. plyr package has tools for splitting, applying and combining data	

library(plyr)	
require(plyr)

# Information of data processing directories and files
UCILocalDir <- "UCI HAR Dataset"
featuresFile <- paste(UCILocalDir, "//features.txt", sep = "")
activityLabelsFile <- paste(UCILocalDir, "//activity_labels.txt", sep = "")
xTrainFile <- paste(UCILocalDir, "//train//X_train.txt", sep = "")
yTrainFile <- paste(UCILocalDir, "//train//y_train.txt", sep = "")
subjectTrainFile <- paste(UCILocalDir, "//train//subject_train.txt", sep = "")
xTestFile  <- paste(UCILocalDir, "//test//X_test.txt", sep = "")
yTestFile  <- paste(UCILocalDir, "//test//y_test.txt", sep = "")
subjectTestFile <- paste(UCILocalDir, "//test//subject_test.txt", sep = "")

# Loading of raw data from local directory
features <- read.table(featuresFile, colClasses = c("character"))
activityLabels <- read.table(activityLabelsFile, col.names = c("ActivityId", "Activity"))
xTrain <- read.table(xTrainFile)
yTrain <- read.table(yTrainFile)
subjectTrain <- read.table(subjectTrainFile)
xTest <- read.table(xTestFile)
yTest <- read.table(yTestFile)
subjectTest <- read.table(subjectTestFile)

##################################################################
# 1. Merging of the training and the test sets into one data set.
##################################################################

# Binding of sensor data
trainingSensorData <- cbind(cbind(xTrain, subjectTrain), yTrain)
testSensorData <- cbind(cbind(xTest, subjectTest), yTest)
sensorData <- rbind(trainingSensorData, testSensorData)

# Preparing label columns

sensorLabels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensorData) <- sensorLabels

############################################################################################
# 2. Extracting of only the measurements on the mean and standard deviation for each measurement.
############################################################################################

sensorDataMeanStd <- sensorData[,grepl("mean|std|Subject|ActivityId", names(sensorData))]

###########################################################################
# 3. Preparing descriptive activity names to the activities in the data set
###########################################################################

sensorDataMeanStd <- join(sensorDataMeanStd, activityLabels, by = "ActivityId", match = "first")
sensorDataMeanStd <- sensorDataMeanStd[,-1]

##############################################################
# 4. Labelling of data set with appropriate descriptive names.
##############################################################

names(sensorDataMeanStd) <- gsub('\\(|\\)',"",names(sensorDataMeanStd), perl = TRUE)
names(sensorDataMeanStd) <- make.names(names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('Acc',"Acceleration",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('GyroJerk',"AngularAcceleration",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('Gyro',"AngularSpeed",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('Mag',"Magnitude",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('^t',"TimeDomain.",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('^f',"FrequencyDomain.",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('\\.mean',".Mean",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('\\.std',".StandardDeviation",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('Freq\\.',"Frequency.",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- gsub('Freq$',"Frequency",names(sensorDataMeanStd))

######################################################################################################################
# 5. Create of tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################

sensorAvgByActSub = ddply(sensorDataMeanStd, c("Subject","Activity"), numcolwise(mean))
write.table(sensorAvgByActSub, file = "avg_by_act_sub.txt",row.name = FALSE)

##----------------------------End of Project Assignment---------------------------------------------------------------