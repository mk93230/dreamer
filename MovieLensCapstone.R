# R Version 3.6

# @author Mahalingam Kamalahasan

###################################################################
################# LOADING THE REQUIRED PACKAGES ###################
###################################################################
# If tidyverse package does not exist, load from the following repo and install it
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# If caret package does not exist, load from the following repo and install it
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")



###################################################################
########## LOADING THE TEST and VALIDATION DATA SETS ##############
###################################################################
# Loading edx, the rds file was downloaded from google drive and stored locally
# The file was retreived from https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D
edx <- readRDS("../dataset/edx.rds")

# running the dimension on edx to make sure I have correct number of rows and cols
dim(edx) # rows: 9000055      cols: 6

# Loading Validation dataset, the rds file was downloaded from google drive and stored locally
# The file was retreived from https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D
validation <- readRDS("../dataset/validation.rds")

# running the dimension on validation to make sure I have correct number of rows and cols
dim(validation) # rows: 999999     cols: 6


###################################################################
################# ROOT MEAN SQUARE ERROR Function #################
###################################################################
# Lets create a function for room mean square error. This is calculated by
# finding the difference between predicted rating to the actual rating
RMSE <- function(predictedrating,actualrating)
{
  sqrt(mean((predictedrating-actualrating)^2))
}



###################################################################
####################### BUILDING THE MODEL ########################
###################################################################

# Through analysis we can capture additional effects in addition to
# calculating the average ratings of all movies

# You can see some movies are rated more that the others, because some are popular,
# blockbusters, critically acclaimed and etc. You can see them visually through the 
# below plot
edx %>% ggplot(aes(movieId,color="movie ratings")) + geom_histogram(binwidth = 5, color="green")

# You can notice from the below histogram that not all users rate the 
# same amount and some rate higher than the others
edx %>% ggplot(aes(userId,color="user ratings")) + geom_histogram(binwidth = 5, color="blue")

############################BUILDING AN EQUATION ####################################
# The above plots tell me that at the minimum to build a model that is effective to 
# predict I need average of all movie ratins and add movie and user effect.
# So the equation is y_hat_iu = mu + bi + bu + epsilon
# Where mu is the average rating of all the movies
# Where y_hat_iu is the predicted rating of a movie i by user u.
# Where bi is the average of movie effect of a movie i rated by all users
# Where bu is the average of user effect on all movies rated by the user
# epsilon is the noise introduced, that is the residual obtained after determining
# all kinds of relationship we saw in the data. For simplicity we will assume that the
# epsilon is 0.
# Therefore the derived equation for the model is:y_hat_iu = mu + bi + bu

############################CALCULATING mu ###################################
mu <- mean(edx$rating) #3.512465

############################CALCULATING bi ###################################
# One of the approaches to calculate bi for a specific movie i is by assuming
# the equation y_hat_i = mu + bi.
# Rearranging this equation we get bi = y_hat_i - mu, where y_hat_i is the average ratings
# of movie i, where bi is the vector of movie effects and b_hat_i is the movie effect residual
# for movie i. Where mu is the average rating of all movies.

bi<-edx %>% group_by(movieId) %>% summarize(b_hat_i = (mean(rating)-mu))

# From the below histogram plot it is helpful to see the movie effect where we can see
# movie effect helping the good movies and discrediting the not so good movies.
bi %>% ggplot(aes(b_hat_i, color="movie effect")) + geom_histogram(bins=10,fill="brown")

############################CALCULATING bu ###################################
# One of the approaches to calculate bu for a specific user u is by assuming
# the equation y_hat_iu = mu + bi + bu.
# Rearranging this equation we get bu = y_hat_iu - mu - bi, where y_hat_iu is the average ratings
# of movie i by user u, where bi is the vector of movie effects, bu is the vector of user effects
# bu_hat_i is the user effect on a specific movie i. Where mu is the average rating of all movies.

# Add b_hat_i to the movies before grouping by userid.
# Apply the equation bu = y_hat_iu - mu - bi to the grouped data
bu<-edx %>% left_join(bi,by="movieId") %>% group_by(userId) %>% summarize(bu_hat_u = mean(rating-mu-b_hat_i))

# From the below histogram plot it is helpful to see the user effect, i.e some users
# in general rate the movies they see optimistically than others. Some users more
# pessimistic than others.
bu %>% ggplot(aes(bu_hat_u, color="user effect")) + geom_histogram(bins=10,fill="brown")


############### Building the equation ################################
############## y_hat_iu = mu + bi + bu ###############################
############# we will do this in 2 steps for brevity #################
# Step 1 building mu + bi, we will call it as mu_plus_bi
mu_plus_bi <- mu + validation %>% left_join(bi,by="movieId")%>%.$b_hat_i

# Step 2 building y_hat_iu = mu_plus_bi + bu
y_hat_iu <- mu_plus_bi + validation %>% left_join(bu,by="userId")%>%.$bu_hat_u

#####################################################################
############## CALCULATING RMSE FOR THE MODEL #######################
#####################################################################
# Now lets calculate the RMSE for this new model with movie and user effect
rmse_results <- RMSE(y_hat_iu,validation$rating) #0.8653488
rmse_results

########################################################################
###########################RMSE RESULT: 0.8653488 ######################
########################################################################
