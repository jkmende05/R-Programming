# Libraries and packages to be used
library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)

# Data was retrieved from:
# https://data.cityofnewyork.us/Education/2011-NYC-School-Survey/mnz3-dyi8/about_data

# Get first data frame
survey_gened <- read_excel("school_survey_data//masterfile11_gened_final.xlsx",
                           col_names = FALSE)
head(survey_gened)

# Re-arrange to set column names and remove unnecessary rows
colnames(survey_gened) <- survey_gened[3, ]
survey_gened <- survey_gened[-c(1, 2, 3), ]
head(survey_gened)

# Only take necessary columns
survey_gened <- survey_gened %>%
  select(dbn:aca_tot_11)

survey_gened$type <- "gened"
head(survey_gened)

# Read in second data frame, which contains District 75 Data
survey_d75 <- read_excel("school_survey_data//masterfile11_d75_final.xlsx")
head(survey_d75)

colnames(survey_d75) <- survey_d75[2, ]
survey_d75 <- survey_d75[-c(1, 2), ]
head(survey_d75)

survey_d75 <- survey_d75 %>%
  select(dbn:aca_tot_11)

survey_d75$type <- "d75"

# Use full_join to combine the data frames
combined_data <- full_join(survey_gened, survey_d75)
head(combined_data)

dim(combined_data)

# Replace all NA values with 0
combined_data <- combined_data %>% mutate_all(~ replace(., is.na(.), 0))

# Change data types in columns to numeric if it is a numeric value
# Keep first three columns as strings
for (col in colnames(combined_data)){
  if (col == "type" || col == "dbn" || col == "schoolname") {
    next
  }
  combined_data[[col]] <- as.numeric(combined_data[[col]])
}

# Create correlation matrix for variables and aca_tot_11
correlation_mat <- combined_data %>%
  select(aca_tot_11, rr_s : eng_tot_11) %>%
  cor(use = "pairwise.complete.obs")

correlation_mat

# Create tibble to be easier to use and view
correlation_tib <- correlation_mat %>%
  as_tibble(rownames = "variable")

correlation_tib

# Filter to include only strong correlations
strong_correlations <- correlation_tib %>%
  select(variable, aca_tot_11) %>%
  filter(aca_tot_11 > 0.25 | aca_tot_11 < -0.25)

strong_correlations

# Create a function to create a scatter plot using ggplot2
create_sp <- function(x, y) {
  ggplot(data = combined_data) +
    aes_string(x = x, y = y) +
    geom_point(alpha = 0.25) +
    theme(panel.background = element_rect(fill = "white"))
}

# Create scatter plot for data between aca_tot_11 and rr_t
create_sp(strong_correlations$variable[2], "aca_tot_11")
