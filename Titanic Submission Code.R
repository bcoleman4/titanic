# Load libraries
library(rpart)
library(rattle)
library(RGtk2)

# Loading data from local files
train <- read.csv("/Users/Brian/Desktop/Kaggle/Titanic/train.csv")
test <- read.csv("/Users/Brian/Desktop/Kaggle/Titanic/test.csv")

# Your train and test set are still loaded in
str(train)
str(test)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

# Make your prediction using the test set
my_prediction <- predict(my_tree_two, test, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Check that your data frame has 418 entries
nrow(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file = "/Users/Brian/Desktop/Kaggle/Titanic/my_solution.csv", row.names = FALSE)

# Create a new decision tree my_tree_three
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class", control = rpart.control(cp = 0, minsplit = 50))
  
# Visualize your new decision tree
fancyRpartPlot(my_tree_three)

# Make your prediction using the test set
my_prediction2 <- predict(my_tree_three, test, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution2 <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction2)

# Check that your data frame has 418 entries
nrow(my_solution2)

# Write your solution to a csv file with the name my_solution2.csv
write.csv(my_solution2, file = "/Users/Brian/Desktop/Kaggle/Titanic/my_solution2.csv", row.names = FALSE)

# create a new train set with the new variable
train_two <- train
train_two$family_size <- (train$SibSp + train$Parch + 1)

# Create a new decision tree my_tree_three
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data=train_two, method="class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)