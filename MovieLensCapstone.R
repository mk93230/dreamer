# Capstone Project - R version 3.6

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Loading edx
edx <- readRDS("./dataset/edx.rds")

# running the dimension on edx to make sure I have correct number of rows and cols
dim(edx) # rows: 9000055      cols: 6

# Loading Validation dataset
validation <- readRDS("./dataset/validation.rds")

# running the dimension on validation to make sure I have correct number of rows and cols
dim(validation) # rows: 999999     cols: 6

