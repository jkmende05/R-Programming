library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Create table of all combinations of dice and their sums
dice_values_df <- data.frame(
  Dice_One = rep(c(1:6), each = 6),
  Dice_Two = rep(c(1:6), times = 6)
)
dice_values_df$sum_dice <- dice_values_df$Dice_One + dice_values_df$Dice_Two

dice_values_df

# Use pivot_wider to make table easier to view and use
dice_sums <- pivot_wider(dice_values_df, names_from = Dice_Two,
                         values_from = sum_dice)
drop_dice_one_col <- c("Dice_One")
dice_sums <- dice_sums[, !(names(dice_sums) %in% drop_dice_one_col)]
dice_sums

# Calculate number of occurences of each sum
sum_occurences <- dice_sums %>%
  pivot_longer(cols = everything(), values_to = "Sum") %>%
  count(Sum, name = "Occurences")
sum_occurences

# Display distributions of the sum in ggplot bar graph
ggplot(sum_occurences, aes(x = Sum, y = Occurences)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Probability Distribution of the Sum of Two Dice",
       x = "Dice Sum",
       y = "Possible Combinations") +
  theme_gray(base_size = 24) +
  scale_x_continuous(breaks = 2:12) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Calculate probabilities as a decimal number
sum_occurences$Probability <- sum_occurences$Occurences / 36
sum_occurences

# Create random sample of 1000 dice rolls
die1 <- sample(1:6, 1000, replace = TRUE)
die2 <- sample(1:6, 1000, replace = TRUE)

# Calculate sums
sums <- die1 + die2

# Store sums and dice rolls in a data table
dice_df <- data.frame(Roll = rep(1:1000, 2),
                      Dice = rep(c("Die 1", "Die 2"), each = 1000),
                      Value = c(die1, die2))
head(dice_df)

# Create facet plot of the number of occurences of each number for both die
ggplot(dice_df, aes(x = Value)) +
  geom_bar(fill = "red", alpha = 0.7) +
  facet_wrap(~ Dice) +
  labs(title = "Distribution of Dice Rolls",
       x = "Dice Value", y = "Count") +
  scale_x_continuous(breaks = 1:6) +
  theme_minimal(base_size = 32) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Create table and graph of the sum of the 1000 dice rolls
sum_table <- data.frame(Dice1 = die1, Dice2 = die2, Sum = sums)
head(sum_table)
ggplot(sum_table, aes(x = Sum)) +
  geom_bar(fill = "red", alpha = 0.7) +
  labs(title = "Distribution of Sum",
       x = "Sum Value", y = "Count") +
  scale_x_continuous(breaks = 2:12) +
  theme_minimal(base_size = 32) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Determine percentages of each sum for sample
observed_counts <- sum_table %>%
  count(Sum, name = "Observed") %>%
  mutate(Observed_Percentage = (Observed / 1000) * 100)
head(observation_counts)

# Compare theoretical and sample data in table using left_join
# Convert theoretical probabilities to percentages
comparison_data <- left_join(sum_occurences, observed_counts, by = "Sum") %>%
  mutate(Expected_Percentage = Probability * 100)
head(comparison_data)

# Modify data to be used to create comparison graph
comparison_data <- comparison_data %>%
  select(Sum, Observed_Percentage, Expected_Percentage) %>%
  pivot_longer(cols = c(Observed_Percentage, Expected_Percentage),
               names_to = "Type", values_to = "Percentage")
head(comparison_data)

# Create line graph comparing sample and theoretical sum data
ggplot(comparison_long, aes(x = Sum, y = Percentage, color = Type,
                            group = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = str_wrap("Dice Sum Distribution (Percentage)",
                        width = 100),
       x = "Sum of Two Dice", y = "Percentage (%)",
       color = "Distribution") +
  scale_x_continuous(breaks = 2:12) +
  theme_minimal(base_size = 24) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.85, 0.85)) +
  scale_color_manual(values = c("Observed_Percentage" = "blue",
                                "Expected_Percentage" = "red"))

# Function to calculate probability of rolling a number
# Function is based on number of dice, desired sum, and number of sides of the die
prob_sums <- function(num_dice, target_sum, sides = 6) {
  outcomes <- expand.grid(rep(list(1:sides), num_dice))
  sums <- rowSums(outcomes)
  prob <- sum(sums == target_sum) / length(sums)
  return(prob)
}

print(paste("Probability of rolling a sum of", 7, "with", 2, "dice:",
            prob_sums(2, 7)))

# Function to calculate probability of rolling greater than a certian sum
prob_greater <- function(num_dice, threshold, sides = 6) {
  outcomes <- expand.grid(rep(list(1:sides), num_dice))
  sums <- rowSums(outcomes)
  prob <- sum(sums > threshold) / length(sums)
  return(prob)
}

print(paste("Probability of rolling greater than a", 6, "with", 2, "dice:",
            prob_greater(2, 6)))

# Probability of rolling an even number or odd number based on number of dice 
# Also based on number of sides on each
prob_even_odd <- function(num_dice, sides = 6) {
  outcomes <- expand.grid(rep(list(1:sides), num_dice))
  sums <- rowSums(outcomes)
  prob_even <- sum(sums %% 2 == 0) / length(sums)
  prob_odd <- sum(sums %% 2 == 1) / length(sums)
  return(list(even = prob_even, odd = prob_odd))
}

even_odd <- prob_even_odd(2)
print(paste("Probability of an even sum:", even_odd$even))
print(paste("Probability of an odd sum:", even_odd$odd))