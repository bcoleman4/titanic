# Load libraries
library(rpart)
library(randomForest)

# Loading data from local files
train <- read.csv("~/Desktop/Kaggle/Titanic/train.csv")
test <- read.csv("~/Desktop/Kaggle/Titanic/test.csv")
train <- read.csv("~/Documents/Programming/Kaggle/Titanic/train.csv")
test <- read.csv("~/Documents/Programming/Kaggle/Titanic/test.csv")

# Your train and test set are still loaded in
str(train)
str(test)

# Preparing data
train$Embarked[c(62,830)] = "S"
train$Embarked <- factor(train$Embarked)

# create a new train set with the new variable
train$family_size <- (train$SibSp + train$Parch + 1)

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data=train[!is.na(train$Age),], method="anova")
train$Age[is.na(train$Age)] <- predict(predicted_age, train[is.na(train$Age),])
test$Age[is.na(test$Age)] <- predict(predicted_age, test[is.na(test$Age),])

test$Fare[153] <- median(train$Fare, na.rm=TRUE)

# create Title column
i <- 1
while (i <= nrow(train)){
ifelse(grepl("Mr.", train$Name[i]), train$Title[i] <- "mr", 
ifelse(grepl("Mrs.", train$Name[i]), train$Title[i] <- "mrs",
ifelse(grepl("Master.", train$Name[i]), train$Title[i] <- "master",
ifelse(grepl("Miss.", train$Name[i]), train$Title[i] <- "miss", "NA"
#train$Title[i] <- 
#if (train$Age[i] >= 16) {
#	train$Title[i][train$Sex[i]=="male"] <- "mr"
#} 
#else
#	{train$Title[i][train$Sex[i]=="female"] <- "mrs"
#	}	
#if  (train$Age[i] < 16) {
#	train$Title[i][train$Sex[i]=="male"] <- "master"
#}
#else
#	{train$Title[i][train$Sex[i]=="female"] <- "miss"
#	}
))))
i = i+1
}

#i <- 1
#while (i <= nrow(train)){
#if (train$Age[i] >= 16) {
#	train$Title[i][train$Sex[i]=="male"] <- "mr"
#} 
#else
#	{train$Title[i][train$Sex[i]=="female"] <- "mrs"
#	}	
#if  (train$Age[i] < 16) {
#	train$Title[i][train$Sex[i]=="male"] <- "master"
#}
#else
#	{train$Title[i][train$Sex[i]=="female"] <- "miss"
#	}
#i = i+1
#}


# Set seed for reproducibility
set.seed(104)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data=train, importance=TRUE, ntree=100000 )

# Make your prediction using the test set
my_prediction <- predict(my_forest, test, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "/Users/Brian/Documents/Programming/Kaggle/Titanic/random_forest_solution_II.csv", row.names = FALSE)