library(dplyr)
library(ggplot2)

# Dataset involves forest fire data from 2008 in the Norteast Region of Portugal
# Link: https://archive.ics.uci.edu/dataset/162/forest+fires
forest_fires_data <- read.csv("analyzing_forest_fire_data//forestfires.csv")
head(forest_fires_data)

colnames(forest_fires_data)

# Get unique values for month and day column
forest_fires_data %>% pull(month) %>% unique
forest_fires_data %>% pull(day) %>% unique

# Create vectors to help order month column
months <- c("jan", "feb", "mar", "apr",
            "may", "jun", "jul", "aug",
            "sep", "oct", "nov", "dec")

days <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

forest_fires_data <- forest_fires_data %>%
  mutate(
    month = factor(month, levels = months),
    day = factor(day, levels = days)
  )

head(forest_fires_data)

# Create new data frame for total fires by month, arrange in order of the months
month_fires <- forest_fires_data %>%
  group_by(month) %>%
  summarize(total_fires = n())
arrange(months)

head(month_fires)

# Create bar graph using ggplot2 of total fires by month
month_fires %>%
  ggplot(aes(x = month, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of Fires by Month",
    x = "Month of the Year",
    y = "Total Fires"
  ) +
  theme_gray(base_size = 24) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Create new data frame for total fires by day of the week
day_fires <- forest_fires_data %>%
  group_by(day) %>%
  summarize(total_fires = n())
arrange(day_fires)

# Create bar graph for total fires by day of the week
day_fires %>%
  ggplot(aes(x = day, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of Fires by Day of the Week",
    x = "Days of the Week",
    y = "Total Fires"
  ) +
  theme_gray(base_size = 24) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Create more detailed data frame for other variables included in original csv file
detailed_forest_fires <- forest_fires_data %>%
  pivot_longer(
    cols = c("temp", "RH", "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

drop_columns <- c("X", "Y", "FFMC", "DMC", "DC", "ISI")
detailed_forest_fires <- detailed_forest_fires[,!(names(detailed_forest_fires)
                                                  %in% drop_columns)]

head(detailed_forest_fires)

# Create multiple graphs for each variable by the months of the year
detailed_forest_fires %>%
  ggplot(aes(x = month, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable Changes by Month",
    x = "Month",
    y = "Variable Value"
  ) +
  theme_gray(base_size = 24) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Create multiple graphs for each variable by area of the fire
# Outlier data was also filtered out
detailed_forest_fires %>%
  filter(area < 300) %>%
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scale = "free_x") +
  labs(
    title = "Variable Changes by Area",
    x = "Variable Value",
    y = "Area"
  ) +
  theme_gray(base_size = 24) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )