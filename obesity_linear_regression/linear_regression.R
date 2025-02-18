library(caTools)
library(dplyr)
library(tidyr)
library(ggplot2)

# Get csv data
csv_path <- paste0("obesity_linear_regression\\Nutrition__Physical_Activity",
                   "__and_Obesity_-_Behavioral_Risk_Factor",
                   "_Surveillance_System.csv")

obesity_data <- read.csv(csv_path)
head(obesity_data)

summary(obesity_data)

str(obesity_data)

colSums(is.na(obesity_data))

# Filter data to remove rows with NA values in Data Value, Education, or Income
complete_data <- obesity_data %>%
  filter(!is.na(Data_Value), !is.na(Education), !is.na(Income))
head(complete_data)

# Get unique education and income values
unique_educ_values <- unique(complete_data$Education)
unique_educ_values

unique_income_values <- unique(complete_data$Income)
unique_income_values

# Get count of number of non-null and unique values in columns
non_null_counts <- colSums(!is.na(obesity_data))
unique_counts <- sapply(obesity_data,
                        function(col) length(unique(na.omit(col))))
unique_counts

var_counts <- data.frame(
  non_null_counts = non_null_counts,
  unique_counts = unique_counts
)
var_counts

predictor_threshold <- 0.7 * nrow(obesity_data)
pot_predictors <- var_counts %>%
  filter(non_null_counts > predictor_threshold, unique_counts > 1) %>%
  arrange(desc(non_null_counts))
pot_predictors

# Scatter plot for sample size and data value
ggplot(obesity_data, aes(x = Sample_Size, y = Data_Value)) +
  geom_point() +
  labs(
    title = "Correlation between Sample Size and Data Value (Obesity Index)",
    x = "Sample Size",
    y = "Data Value (Obesity Index)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(labels = scales::comma)

# Scatter plot for correlation between high confidence limit and obesity index
ggplot(obesity_data, aes(x = High_Confidence_Limit, y = Data_Value)) +
  geom_point() +
  labs(
    title = paste0("Correlation between High Confidence Limit",
                   "and Data Value (Obesity Index)"),
    x = "High Confidence Limit",
    y = "Data Value (Obesity Index)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(labels = scales::comma)

# Split, train and test model
split <- sample.split(complete_data$Data_Value, SplitRatio = 0.7)
training_set <- subset(complete_data, split == TRUE)
testset <- subset(complete_data, split == FALSE)

# Get linear model
lm_model <- lm(Data_Value ~ High_Confidence_Limit, data = training_set)
summary(lm_model)

# Create predictions and compare with actual results
predictions <- predict(lm_model, newdata = complete_data)
results <- data.frame(
  Actual = complete_data$Data_Value,
  Predicted = predictions
)
head(results)

# Get calculated r2 value
r2 <- summary(lm_model)$r.squared
r2

# Plot data and linear model on ggplot2 scatter plot
ggplot(complete_data, aes(x = High_Confidence_Limit, y = Data_Value)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "Linear Regression: High Confidence Limit vs Data Value",
    x = "High_Confidence_Limit",
    y = "Data Value (Obesity Index)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
