# Capstone Project - R version 3.6

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
