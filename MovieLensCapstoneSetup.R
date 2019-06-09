###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
     semi_join(edx, by = "movieId") %>%
     semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# determining the dimensions of edx
class(edx)
nrow(edx)
ncol(edx)

head(edx)

# Number of movies that received the rating 0
edx %>% filter(rating==0) %>% nrow()

# Number of movies that received the rating 3
edx %>% filter(rating==3) %>% nrow()

# How many different movies are in the edx dataset
edx %>% summarize(n_movies = n_distinct(movieId))

# How many different users are in the edx dataset
edx %>% summarize(n_users = n_distinct(userId))

# How many movie ratings grouped by genre in edx dataset
head(edx)

# Number of movie ratins for genre: Drama
edx %>% group_by("genres") %>% filter(genres=="Drama") %>% nrow()
#733296

# Number of movie ratins for genre: Comedy
edx %>% group_by("genres") %>% filter(genres=="Comedy") %>% nrow()
#700889

# Number of movie ratins for genre: Thriller
edx %>% group_by("genres") %>% filter(genres=="Thriller") %>% nrow()
#94662

# Number of movie ratins for genre: Romance
edx %>% group_by("genres") %>% filter(genres=="Romance") %>% nrow()
#8410
# Ok the values did not match
# Lets load the rds file and see what happens

# Lets find the current working directory
getwd()

# Moved the edx and validation datasets under the dreamer project, created
# a separate folder called dataset
# Resetting edx
rm(edx)
# Loading edx
edx <- readRDS("./dataset/edx.rds")
dim(edx)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Now lets the above code again with this dataset
# Number of movie ratins for genre: Drama
edx %>% group_by("genres") %>% filter(genres %in% "Drama") %>% nrow()
#733296

# Number of movie ratins for genre: Comedy
edx %>% group_by("genres") %>% filter(genres %in% "Comedy") %>% nrow()
#700889

# Number of movie ratins for genre: Thriller
edx %>% group_by("genres") %>% filter(genres=="Thriller") %>% nrow()
#94662

# Number of movie ratins for genre: Romance
edx %>% group_by("genres") %>% filter(genres=="Romance") %>% nrow()
#8410
#View(edx %>% group_by("genres") %>% filter(genres=="Romance"))

# Which movie has the greatest number of ratings
head(edx)
#View(edx %>% select(title) %>% table())
#Forrest Gump: 31079
#Jurassic park : 29360
#Pulp fiction: 31362
#Speed 2 : 2566
#The shawshant redemption: 28015

# Five most given ratings - order from high to low.
edx %>% select(rating) %>% table()

# Lets find all of the genres
genres_result <- edx %>% select(genres)
class(genres_result)
names(genres_result)[1] <- "genre"
genres_result %>% grep("Drama")

