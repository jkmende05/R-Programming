library(dplyr)
library(ggplot2)
library(randomForest)

# Load wine quality data frame from url
wine_df <- read.csv(url(paste0("https://archive.ics.uci.edu/",
                               "ml/machine-learning-databases/wine-quality",
                               "/winequality-red.csv")),
                    header = TRUE, sep = ";")
head(wine_df)

dim(wine_df)

# Create simple barplot based on wine quality
barplot(table(wine_df$quality))

# Add new column called taste that is based on wine quality
wine_df$taste <- ifelse(wine_df$quality < 5, "bad", "good")
wine_df$taste[wine_df$quality == 5 | wine_df$quality == 6] <- "normal"
wine_df$taste <- as.factor(wine_df$taste)

str(wine_df)

# Create table and barplot of taste column
barplot(table(wine_df$taste))
table(wine_df$taste)

set.seed(123)

# Split, train and test the data
split <- sample(nrow(wine_df), 0.8 * nrow(wine_df))
training <- wine_df[split, ]
testing <- wine_df[-split, ]

# Create plot based on acidity levels
ggplot(wine_df, aes(fixed.acidity, volatile.acidity)) +
  geom_point(aes(color = taste)) +
  theme_minimal(base_size = 18)

# Create plot based on alcohol and taste
ggplot(wine_df, aes(alcohol)) +
  geom_histogram(aes(fill = taste), color = "black", bins = 50) +
  theme_minimal(base_size = 18)

dim(training)

# Train the model using the randomForest function
model <- randomForest(taste ~ . - quality, data = training, ntree = 1000,
                      mtry = 5)
model

# View confusion matrix of the model
model$confusion

# Create and view predictions
prediction <- predict(model, newdata <- testing)
table(prediction, testing$taste)

prediction

# Compare predictions to actual values and view table
results <- cbind(prediction, testing$taste)
results

colnames(results) <- c("prediction", "actual")
results <- as.data.frame(results)
View(results)

# Calculate accuracy of the model
accuracy <- sum(prediction == testing$taste) / nrow(testing)
accuracy