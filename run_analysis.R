#part 1
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_lab <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_sub <- read.table("./UCI HAR Dataset/train/subject_train.txt")
test <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_lab <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_sub <- read.table("./UCI HAR Dataset/test/subject_test.txt")
data <- rbind(train, test)
data <- cbind(data, rbind(train_lab, test_lab))
data <- cbind(data, rbind(train_sub, test_sub))
col.names <- readLines("./UCI HAR Dataset/features.txt")
col.names <- append(col.names, "activity_label")
col.names <- append(col.names, "subject_id")
colnames(data) <- col.names
write.table(data, "./data.txt", sep="\t")

#part 2
columns_to_extract <- c()
for (name in names(data)) {
  if ((grepl("mean()", name) | grepl("std()", name)) & !grepl("meanFreq()", name)) {
    columns_to_extract <- append(columns_to_extract, name)
  }
}
extract_data<-data[, columns_to_extract]

#part 3
label_count <- levels(as.factor(data$activity_label))
activity_data <- data.frame(label_count)
activity_data <- cbind(activity_data, c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
colnames(activity_data) <- c("Label", "Description")

#part 4
activity_name <- function(label) {
  names <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
  names[as.numeric(label)]
}
activity_description <- sapply(data$activity_label, activity_name)
data <- cbind(data, activity_description)

#part 5
col1 <- aggregate(data[,1], by=list(data$subject_id, data$activity_description),mean)
new_data <- data.frame(col1)
for (i in 2:561) {
  col <- aggregate(data[,i], by=list(data$subject_id, data$activity_description),mean)
  new_data <- cbind(new_data,col$x)
}
names(new_data) <- c("subject_id","activity_description",names(data)[1:561])
