# Get necessary packages and libraries
library(dplyr)
library(tidyr)

# Read in csv file
book_data <- read.csv("analyzing_sales_data//sales_data.csv")
head(book_data)

# Get dimensions of data frame
dim(book_data)

# Get column names and print unique values of each column
colnames(book_data)

for (c in colnames(book_data)) {
  print(paste("Unique values in", c))
  print(unique(book_data[[c]]))
}

# Filter out NA values in total_purchased column and calculate mean
total_purchased_mean <- book_data %>%
  filter(!is.na(total_purchased)) %>%
  pull(total_purchased) %>%
  mean

# Replace NA values with calculated mean
book_data <- book_data %>%
  mutate(
    replace_na_purchases = if_else(is.na(total_purchased),
                                   as.integer(total_purchased_mean),
                                   total_purchased)
  )

# Determine most popular book by total sales
most_popular_book <- book_data %>%
  count(book_data$title, name = "Sales") %>%
  arrange(desc(Sales))

head(most_popular_book)

# Filter out sales with no reviews
data_with_reviews <- book_data %>%
  filter(!is.na(book_data$user_submitted_review) &
           book_data$user_submitted_review != "")

head(data_with_reviews)

for (c in colnames(data_with_reviews)) {
  print(paste("Unique values in", c))
  print(unique(data_with_reviews[[c]]))
}

# Create new data frame by number of each type of review per book
reviews_by_title <- data_with_reviews %>%
  count(user_submitted_review, title) %>%
  pivot_wider(names_from = title, values_from = n,
              values_fill = 0)

head(reviews_by_title)