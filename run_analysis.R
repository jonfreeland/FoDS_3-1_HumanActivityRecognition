if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  require(tidyr)
}

activities <- read.table("activity_labels.txt", quote="\"", comment.char="")
features <- read.table("features.txt", quote="\"", comment.char="")

ingest_data <- function(setType) {
  # read in data
  subject <- read.table(paste(setType,"/subject_",setType,".txt",sep=""), quote="\"", comment.char="")
  x <- read.table(paste(setType,"/X_",setType,".txt",sep=""), quote="\"", comment.char="")
  y <- read.table(paste(setType,"/y_",setType,".txt",sep=""), quote="\"", comment.char="")
  # transpose features and assign as column names
  colnames(x) <- t(features[c(2)])
  # select mean|std columns from x
  x_select <- x %>% subset(select = grep("mean\\(\\)|std\\(\\)", names(x)))
  # add subject column
  x_select$SubjectId <- subject$V1
  # assign source type column
  x_select$SourceSet <- setType
  # add activity ID column
  x_select$ActivityLabel <- y$V1
  return(x_select)
}

testData <- ingest_data("test")
trainData <- ingest_data("train")

# combine datasets here
comboData <- rbind(trainData, testData)

# join in activiy labels
colnames(activities) <- c("ActivityLabel", "ActivityName")
comboDataJoined <- merge(comboData, activities, by = "ActivityLabel")

# tidy it up
comboDataTidy <- gather(comboDataJoined, "Feature", "Value", 2:67)

# group by activity and subject to mean value
meany <- comboDataTidy %>% group_by(ActivityLabel, SubjectId, ActivityName, Feature) %>% summarise(mean = mean(Value))
