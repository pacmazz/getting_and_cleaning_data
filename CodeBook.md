Getting and Cleaning Data Course Project
========================================================

We load the data and we merge them to build a single data set.

```{r}
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
```

Now we extract the mean and the standard deviation for every measurement we have in the data.

```{r}
columns_to_extract <- c()
for (name in names(data)) {
  if ((grepl("mean()", name) | grepl("std()", name)) & !grepl("meanFreq()", name)) {
    columns_to_extract <- append(columns_to_extract, name)
  }
}
extract_data<-data[, columns_to_extract]
```

The label for each activity isn't so simple to understand, so we connect descriptive activity names to the activities labels in the data set. Let's create a function which associates the labels with their description.

```{r}
activity_name <- function(label) {
  names <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
  names[as.numeric(label)]
}
```

We can view the associations also in a dataframe:

```{r}
label_count <- levels(as.factor(data$activity_label))
activity_data <- data.frame(label_count)
activity_data <- cbind(activity_data, c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
colnames(activity_data) <- c("Label", "Description")
```

Let's associate these descriptive names to every row of the original data, using the function we have just created:

```{r}
activity_description <- sapply(data$activity_label, activity_name)
data <- cbind(data, activity_description)
```

We finish this project creating a second tidy data set with the average of each variable for each activity and each subject.

```{r}
col1 <- aggregate(data[,1], by=list(data$subject_id, data$activity_description),mean)
new_data <- data.frame(col1)
for (i in 2:561) {
  col <- aggregate(data[,i], by=list(data$subject_id, data$activity_description),mean)
  new_data <- cbind(new_data,col$x)
}
names(new_data) <- c("subject_id","activity_description",names(data)[1:561])
```