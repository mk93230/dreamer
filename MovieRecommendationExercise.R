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

# User Movie plot, it tells not all movies are rated
movielens %>% ggplot(aes(movieId,userId,color="red")) + geom_point()

# Create a histogram to find which users have rated more
movielens %>% ggplot(aes(userId,color="red")) + geom_histogram(binwidth = 5, color="blue")

# we will do the same for the movies to determine which movies are rated more
# typically blockbuster movies would be
movielens %>% ggplot(aes(movieId,color="red")) + geom_histogram(binwidth = 5, color="green")

# Lets create the training and test set so that we can build the algorithm
library(caret)
set.seed(755)
test_index <- createDataPartition(y=movielens$rating,times=1,p=0.2,list=FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>% semi_join(train_set, by="movieId") %>% semi_join(train_set, by="userId")

# Lets create a function for room mean square error. This is calculated by
# finding the difference between predicted rating to the actual rating by a user
# for all the movies related to that user
RMSE <- function(actualrating, predictedrating)
{
  sqrt(mean((predictedrating-actualrating)^2))
}

# Building the simple model, not considering users of similar types or
# movies of similar types
# Y = Mu + epsilon - This is for all movies, Mu is for the true ratings
# Because we have the sample data we will find mu_hat. epsilon is the error because
# we have the sample centered in zero because we obviously don't have the whole
# population
mu_hat <- mean(train_set$rating)
mu_hat # This is the mean for all movies and for all users

# Now lets compute the Root mean equare error with the test data using the function
# that we created.
naive_rmse <- RMSE(test_set$rating,mu_hat)
naive_rmse

# average value mu_hat reduces the residual mean squar error, you can try with 
# different values but it does not help much
nrow(test_set)
prediction_naive <- rep(2.5,nrow(test_set))
RMSE(test_set$rating,prediction_naive)

# we will be calculating the RMSE for different approaches and see how 
# we are doing. We will call that as RMSE results

rmse_results <- data_frame(method="Just the average", RMSE=naive_rmse)
rmse_results

# We know that some movies are rated higher than others and some are rated lower
# than others. So if we take the y_hat rating of a specific movie and add the movie
# effect b to it. So now the equation will look the following
# y_hat = mu_hat + b 

# Curious to see what factor does to the user id.
as.factor(movielens$userId)

# coming back to original discussion how do you calculate b for a specific movie
# from the equation y_hat_i = mu_hat + b_i where i represents movie i.
# One of the approaches is to calculate the b_i by taking the average/mean of ratings
# of movie i and subtracting that from the mean/average of all movie ratings.
<<<<<<< HEAD
# i.e b_i = mu_hat - y_hat_i. I would like to think it as mu_hat_i - mu_hat. By saying 
=======
# i.e b_i = mu_hat_i - y_hat_i. I would like to think it as mu_hat_i - mu_hat. By saying 
>>>>>>> a96ad0b088ce2f71b64bd5770ec30d520f634322
# y_hat_i you are not ingnoring any other additional effects.

head(train_set)
mu_hat <- mean(train_set$rating)
b_hats<-train_set %>% group_by(movieId) %>% summarize(b_hat_i = (mean(rating)-mu_hat))

# I am going to keep the hat notation to the variables indicating that these are
# estimates. 
# Now that we calculated the b_hats lets build a hist
b_hats %>% ggplot(aes(b_hat_i, color="red")) + geom_histogram(bins=10,fill="red")
# the histogram does tell that some movies are good and other movies are bad.

# We will now calculate the predicted value.
# lets add the b_hats to the mu_hat.
predicted_ratings <- mu_hat + test_set %>% left_join(b_hats,by="movieId")%>%.$b_hat_i
predicted_ratings

# Now lets calculate the RMSE for this new model with movie effect
model_me_results <- RMSE(predicted_ratings,test_set$rating)

# lets bind this to the table we are collecting for the models

rmse_results <- bind_rows(rmse_results, data_frame(method="movie_effect", RMSE=model_me_results))
rmse_results %>% knitr::kable()

# lets find the users who have rated more than 100 movies
userId_occurence <- table(train_set$userId)
userId_occurence_df <- as.data.frame(userId_occurence)
#View(userId_occurence)
which(userId_occurence_df$Freq>=100)

userId_occurence_df_cond <- userId_occurence_df[which(userId_occurence_df$Freq>=100),]
colnames(userId_occurence_df_cond) <- c("userId","Freq")
class(userId_occurence_df_cond$userId)
userId_occurence_df_cond[,1] <- as.integer(userId_occurence_df_cond[,1])
train_set %>% semi_join(userId_occurence_df_cond,by="userId")%>%group_by(userId) %>% summarize(avg_rating=mean(rating)) %>% ggplot(aes(avg_rating, color="green")) + geom_histogram(bins = 5, fill="green")
# The histogram from the above shows the users who have rated more than or equal to
# 100 movies. You can see from the histogram that some users rate pretty much
# all the movies with lower ratings and others are too positive but at the same
# time you can see a nice distribution ratings from most of the users.

# Now that we understood the user effect, we can now introduce the user effect
# in our model. We could have gone with the lm function, the linear model
# to do the calculation. But this might take a long time and could crash the computer.
# We will use the same concept as we did for the movie effect.
# We have already calculated the average ratings for all movies in the trainset
# and we called them as mu_hat. We then calculated the b_i_hat by subtracting the 
# average rating for a movie. We did that by subtracting mu_hat from mu_i_hat.
<<<<<<< HEAD
# To calculate the b_u_hat (the user effect) we will subtract the rating from that user from 
=======
# To calculate the b_u_hat (the user effect) we will subtract the rating from thet user from 
>>>>>>> a96ad0b088ce2f71b64bd5770ec30d520f634322
# mu_hat and b_i_hat and take the average of it. I.e rating_u-mu_hat-b_u_hat

#View(b_hats) # contains movie id and b_i_hat
b_u_hats<-train_set %>% left_join(b_hats,by="movieId") %>% group_by(userId) %>% summarize(b_hat_u = mean(rating-mu_hat-b_hat_i))

# We will now calculate the predicted value with user effect
# lets add the b_hats to the mu_hat.
predicted_ratings_wu <- predicted_ratings + test_set %>% left_join(b_u_hats,by="userId")%>%.$b_hat_u
predicted_ratings_wu

# Now lets calculate the RMSE for this new model with movie and user effect
model_mue_results <- RMSE(predicted_ratings_wu,test_set$rating)

# lets bind this to the table we are collecting for the models

rmse_results <- bind_rows(rmse_results, data_frame(method="movie_user_effect", RMSE=model_mue_results))
rmse_results %>% knitr::kable()

# the results are also skewed when you have movies that are not rated
# as much. In this case what we can do is introduce the skew effect lambda
# and square it and make it inverseley proportional. Lambda would be high when
# there are lower number of ratings, the inverse number makes it too small add as
# an effect to the original. I do not have the plans to build this model right now.
# But I will keep this space if I wanted to build it.


# The next thing we need to do that there is a pattern noticed on how similar
# movies are rated and same user rating many movies. We will build that next
# model, to do that we need to first need to get the prediction for the movie i
# by the user u. Introduce the movie effect bi and user effect bu. Subtract 
# both from the actual rating to get the residual. So the equation looks like
# the following
# rui = yui-b_i_hat-b_u_hat

head(movielens)
class(movielens)

# we will write the below code to filter any movies that are rated equal to or more than
# 50 times and within that we will filter users who have rated dreams more than
# or equal to 50 times.
train_small <- movielens %>% group_by(movieId) %>% filter(n()>=50 | movieId==3252) %>% ungroup() %>% group_by(userId) %>% filter(n()>=50) %>% ungroup()
#View(train_small)

# lets select user id, movie id and rating and create a matrix

y_small <- train_small %>% select(userId,movieId,rating) %>% spread(movieId,rating) %>% as.matrix()
movie_titles <- train_small %>% select(movieId,title)
head(train_small)
#View(y_small)
y_small[,1]
rownames(y_small) <- y_small[,1]
rownames(y_small)
# lets remove the 1st row that has the row names which we already moved to row names
y_small <- y_small[,-1]
colnames(y_small) <- with(movie_titles, title[match(colnames(y_small),movieId)])
#View(y_small)

# we will use the sweep function to apply the function to all cells.
# the default is subtract function

y_small <- sweep(y_small,1,rowMeans(y_small,na.rm=TRUE))
y_small <- sweep(y_small,2,colMeans(y_small,na.rm=TRUE))
#View(y_small)

# Now that we have residuals, to see if these are not epsilon which are
# just noise - independed of each other we can see if there is a 
# correlation. If there is a correlation then its not a noise.
colnames(y_small)
# lets see if there is a correlation between Star Trek II and III. Col no: 219, 220
correl_mov <- y_small[,c(219,220)]
class(correl_mov)
correl_mov_df <- as.data.frame(correl_mov)
class(correl_mov_df)
correl_mov_df %>% plot()
# You can see from the above plot that they are very much correlated which
# says that they are correlated.

# we can do pairwise correlation with certain movies Kill Bill - 412, 418
# spider man 2 - 421, batman begins 430

correl_mov_mul <- y_small[,c(219,220,412,418, 421,430)]
correl_mov_mul_df <- as.data.frame(correl_mov_mul)

# Creating a correlation graph
install.packages("corrgram")
library(corrgram)
corrgram(correl_mov_mul_df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="movies rated by user")


# so what we calculated as residuals can be done using Principal component analysis
# Before calculating that, we need to convert all nas to zero in the data
y_small_pca <- train_small %>% select(userId,movieId,rating) %>% spread(movieId,rating) %>% as.matrix()
y_small_pca[,1]
rownames(y_small_pca) <- y_small_pca[,1]
rownames(y_small_pca)
# lets remove the 1st row that has the row names which we already moved to row names
y_small_pca <- y_small_pca[,-1]
colnames(y_small_pca) <- with(movie_titles, title[match(colnames(y_small_pca),movieId)])

# Before runing the PCA lets change all the NAs to 0
y_small_pca[is.na(y_small_pca)] <- 0
# Lets do the row means, which is the p vector for user effects
y_small_pca_row <- sweep(y_small_pca,1,rowMeans(y_small_pca))
pca <- prcomp(y_small_pca_row)
dim(pca$rotation)
#View(pca$rotation)
dim(pca$x)
#View(pca$x)
plot(pca$sdev)

# There are few variables that have the most impact. Lets calculate the
# cumulative sum
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
<<<<<<< HEAD
plot(var_explained)
#View(var_explained)

pcs <- pca$x
View(pcs)
pcs %>% select()
=======
View(var_explained)
plot(var_explained)
>>>>>>> a96ad0b088ce2f71b64bd5770ec30d520f634322
