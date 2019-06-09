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
edx <- readRDS("./dataset/edx.rds")

# running the dimension on edx to make sure I have correct number of rows and cols
dim(edx) # rows: 9000055      cols: 6

# Loading Validation dataset, the rds file was downloaded from google drive and stored locally
# The file was retreived from https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D
validation <- readRDS("./dataset/validation.rds")

# running the dimension on validation to make sure I have correct number of rows and cols
dim(validation) # rows: 999999     cols: 6


###################################################################
################# ROOT MEAN SQUARE ERROR Function #################
###################################################################
# Lets create a function for room mean square error. This is calculated by
# finding the difference between predicted rating to the actual rating
RMSE <- function(actualrating, predictedrating)
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
edx %>% ggplot(aes(movieId,color="red")) + geom_histogram(binwidth = 5, color="green")

# You can notice from the below histogram that not all users rate the 
# same amount and some rate higher than the others
edx %>% ggplot(aes(userId,color="red")) + geom_histogram(binwidth = 5, color="blue")

############################BUILDING AN EQUATION ####################################
# The above plots tell me that at the minimum to build a model that is effective to 
# predict I need average of all movie ratins and add movie and user effect.

# my 


