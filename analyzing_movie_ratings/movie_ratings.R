library(rvest)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)

ratings_url <- "http://dataquestio.github.io/web-scraping-pages/IMDb-DQgp.html"

webpage <- read_html(ratings_url)
webpage

# Extract header elements
select_title <- ".lister-item-header a"

movie_titles <- webpage %>%
  html_nodes(select_title) %>%
  html_text()

movie_titles

# Extract movie years
select_year <- ".lister-item-year"

movie_years <- webpage %>%
  html_nodes(select_year) %>%
  html_text()

movie_years <- parse_number(movie_years)
movie_years

# Extract movie runtimes
select_runtime <- ".runtime"

runtimes <- webpage %>%
  html_nodes(select_runtime) %>%
  html_text()

runtimes <- parse_number(runtimes)
runtimes

# Extract movie genres
select_genre <- ".genre"

genres <- webpage %>%
  html_nodes(select_genre) %>%
  html_text()

genres <- str_trim(genres)
genres

# Extract movie ratings
select_user_rating <- ".ratings-imdb-rating"

user_ratings <- webpage %>%
  html_nodes(select_user_rating) %>%
  html_attr("data-value")

user_ratings <- as.numeric(runtimes)
user_ratings

# Extract metascores
select_metascore <- ".metascore"

metascores <- webpage %>%
  html_nodes(select_metascore) %>%
  html_text()

metascores <- str_trim(metascores)
metascores <- as.numeric(metascores)
metascores

# Extract user votes
select_votes <- ".sort-num_votes-visible :nth-child(2)"

total_votes <- webpage %>%
  html_nodes(select_votes) %>%
  html_text()

total_votes <- parse_number(total_votes)
total_votes

# Filling in Any Missing Data
add_vector <- function(vector, insert_indices, values) {
  vector_current_indices <- 1:length(vector)
  new_insert <- insert_indices + seq(0, 0.9,
                                     length.out = length(insert_indices))
  indices <- c(vector_current_indices, new_insert)
  ord_indices <- order(indices)
  new_vec <- c(vector, values)
  new_vec[ord_indices]
}

metascores <- add_vector(metascores, c(1, 1, 1, 13, 24), NA)
metascores

# Remove unnecessary element in the 17th position
metascores <- metascores[-17]
movie_titles <- movie_titles[-17]
movie_years <- movie_years[-17]
runtimes <- runtimes[-17]
genres <- genres[-17]
user_ratings <- user_ratings[-17]

# Combine into tibble
movie_data <- tibble("movie_title" = movie_titles,
                     "release_year" = movie_years,
                     "runtime" = runtimes,
                     "genre" = genres,
                     "rating" = user_ratings,
                     "votes" = total_votes,
                     "metascore", metascores)

movie_data

# Create box plot of runtime and total votes
ggplot(data = movie_data,
       aes(x = runtimes, y = total_votes, group = runtimes)) +
  geom_boxplot()
