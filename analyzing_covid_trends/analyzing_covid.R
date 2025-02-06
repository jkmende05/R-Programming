# Libraries and packages that will be used
library(dplyr)
library(tibble)

library(ggplot2)
library(plotly)

# Get csv data
csv_name <- "analyzing_covid_trends\\tested_worldwide.csv"

covid_data <- read.csv(csv_name)

# Display the head and tail of the data
head(covid_data)
tail(covid_data)

# Show summary table of data
summary(covid_data)

# List the column names
colnames(covid_data)

# Use glimpse to display data
glimpse(covid_data)

# Show list of unique countries listed in the Data
unique(covid_data$Country_Region)

# List the countries in alphabetically order
sort(unique(covid_data$Country_Region))

# Get dimensions of the data frame
dim(covid_data)
nrow(covid_data)
ncol(covid_data)
str(covid_data)

# Filter data to only get rows where it involves the entire country
country_covid_data <- covid_data %>% filter(Province_State == "All States")

# Remove province column
country_covid_data <- country_covid_data %>% select(-Province_State)
head(country_covid_data)

# Change column names
colnames(country_covid_data) <- c("Date", "Country", "Positve", "Active",
                                  "Hospitalized", "Hospitalized_Currently",
                                  "Recovered", "Death", "Total_Tested",
                                  "Daily_Tested", "Daily_Positive")


# Replace NA values with 0
country_covid_data <- country_covid_data %>% replace(is.na(.), 0)
head(country_covid_data)

# Filter data to only get the data for Canada
canada_data <- country_covid_data %>% filter(Country == "Canada")
canada_data <- canada_data %>% select(-Country)
canada_data$Date <- as.Date(canada_data$Date)

glimpse(canada_data)

# Create line graph of daily cases by date
plot(canada_data$Date, canada_data$Daily_Positive, type = "l", lwd = 2,
     main = "Graph of Canada Daily Positive COVID Cases", xlab = "Date",
     ylab = "Number of Cases")

# Use ggplot2 to create line graph of active cases by day
ggplot(canada_data, aes(x = Date, y = Active)) + geom_point(color = "blue") +
  labs(title = "Canada: Active COVID-19 Cases")

# Create bar graph of deaths by day using plotly
canada_bar_graph <- plot_ly(canada_data, x = ~Date, y = ~Death, type = "bar",
                            marker = list(color = "red"))

canada_bar_graph <- canada_bar_graph %>% layout(title = "Canada COVID Deaths",
                                                bargrap = 0.2)

canada_bar_graph

# Summarizee data by total tests, cases, hospitalizations by country
country_daily_summary <- country_covid_data %>%
  group_by(Country) %>%
  summarise(Tested = sum(Daily_Tested), Positive = sum(Daily_Positive),
            Hospitalized = sum(Hospitalized_Currently))

country_daily_summary <- country_daily_summary %>% arrange((desc(Positive)))
country_daily_summary

# Get top three countries by positive cases
most_positive <- country_daily_summary
most_positive <- head(most_positive, 3)
top_positive_countries <- most_positive$Country
top_positive_countries

# Get top three countries by total tests
most_tested <- country_daily_summary %>% arrange((desc(Tested)))
most_tested <- head(most_tested, 3)
top_tested_countries <- most_tested$Country
top_tested_countries

# Get top three countries by hospitalizations
most_hospitalized <- country_daily_summary %>% arrange((desc(Hospitalized)))
most_hospitalized <- head(most_hospitalized, 3)
top_hospitalized_countries <- most_hospitalized$Country
top_hospitalized_countries

# Calculate testing rate
country_daily_summary$Rate <- country_daily_summary$Positive /
  country_daily_summary$Tested
head(country_daily_summary)

# Get top three countries with highest positive test rate
high_rate <- country_daily_summary %>% arrange((desc(Rate)))
high_rate <- head(high_rate, 3)
highest_rate_countries <- high_rate$Country
highest_rate_countries

# Create matrix with the leaders in each category
covid_leaders <- rbind(top_positive_countries, top_tested_countries,
                       top_hospitalized_countries, highest_rate_countries)
covid_leaders