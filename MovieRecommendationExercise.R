# Basic packages - For the 1st time install the below packages by uncommenting the below.
#install.packages("tidyverse")
# Obtain Movie Lens data
library("dslabs")
data("movielens")
head(movielens)

# You can see from the above, each row contains the rating provided by a user for a movie. Now
# lets find out the uniques users and unique movies that the ratings have been provided.

library(tidyverse)
movielens %>% summarize(n_user = n_distinct(userId), n_movies = n_distinct(movieId))