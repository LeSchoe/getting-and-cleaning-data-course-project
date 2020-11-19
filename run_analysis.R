library(reshape2)

#1. Merges the training and the test sets to create one data set.

#general data
activity_labels <- read.table('./activity_labels.txt')
features <- read.table('./features.txt')

#test data
subject_test <- read.table('./test/subject_test.txt')
x_test <- read.table('./test/x_test.txt')
y_test <- read.table('./test/y_test.txt')

#Assignment of colnames for merging
colnames(subject_test) <- "subjectId"
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"

#Merge the data with cbind (there is no common column, same dimensions)
test_data <- cbind(y_test,subject_test,x_test);

#training data
subject_train <- read.table('./train/subject_train.txt')
x_train <- read.table('./train/x_train.txt')
y_train <- read.table('./train/y_train.txt')

#Assignment of colnames for merging
colnames(activity_labels) <- c('activityId','activityType')
colnames(subject_train) <- "subjectId"
colnames(x_train) <- features[,2] 
colnames(y_train) <- "activityId"

#Merge the data with cbind (there is no common column, same dimensions)
train_data <- cbind(y_train,subject_train,x_train)
all_data <- rbind(train_data, test_data)

#2. Extract mean and standard deviation columns
all_data_names <- names(all_data)
features_subset <- grep("activityId|subjectId|.*mean.*|.*std.*", all_data_names)
features_subset.names <- all_data_names[features_subset]

all_data_subset <- all_data[features_subset.names]

#3. Uses descriptive activity names to name the activities in the data set
all_data_subset$activityId <- factor(all_data_subset$activityId, levels = activity_labels[,1], labels = activity_labels[,2])
all_data_subset$subjectId <- as.factor(all_data_subset$subjectId)

#4. Appropriately labels the data set with descriptive variable names. 
features_subset.names <- gsub('-std', 'Std', features_subset.names)
features_subset.names <- gsub('-mean', 'Mean', features_subset.names)
features_subset.names <- gsub('[-()]', '', features_subset.names)

colnames(all_data_subset) <- features_subset.names

#5. New independent tidy data set with the average of each variable for each activity and each subject

#Calculating the mean
pre_tidy <- melt(all_data_subset, id = c("subjectId", "activityId"))
tidy_data <- dcast(pre_tidy, subjectId + activityId ~ variable, mean)

#Writing the file
write.table(tidy_data, "tidy.txt", row.names = FALSE)
