# Libraries to be used when creating the decision tree
library(rpart)
library(rpart.plot)
library(caret)

# Get data
data(iris)
head(iris)

# Split the data into testing and trianing
set.seed(123)
split <- sample(1:nrow(iris), 0.7 * nrow(iris))
training <- iris[split, ]
testing <- iris[-split, ]

# Create the model and view the decision tree using rpart.plot
model <- rpart(Species ~ ., data = training)
rpart.plot(model)

# Create predictions and view in a table
predictions <- predict(model, newdata = testing, type = "class")
table(predictions, testing$Species)

# Calculate accuracy of the model
accuracy <- sum(predictions == testing$Species) / nrow(testing)
print(paste("Accuracy:", accuracy))

# Create confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(predictions, testing$Species)
conf_matrix
