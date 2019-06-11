# R Version 3.6

# @author Mahalingam Kamalahasan

###################################################################
################# LOADING THE REQUIRED PACKAGES ###################
###################################################################
# If tidyverse package does not exist, load from the following repo and install it
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# If caret package does not exist, load from the following repo and install it
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")


# The rattle package contains the data for wines grown in specific area of Italy
# The wine data was obtained from UCI Machine Learning Repository.
if(!require(rattle)) install.packages("rattle")

# The following command will list all the datasets in a package, we are interested
# in the wine dataset
data(package="rattle")

#Loading the wine dataset
data("wine")

##################################################################################
########################### Understanding the wine dataset #######################
##################################################################################

head(wine)

# Each record is a sample that contains 13 chemical analysis, based on this value
# we can categorize the type of wine as 1, 2 or 3. The following command gives you
# all the column names  1.   "Alcohol"        2. "Malic" 3."Ash"            
# 4. "Alcalinity" 5. "Magnesium"  6. "Phenols"  7. "Flavanoids" 8. "Nonflavanoids"   
# 9. "Proanthocyanins" 10. "Color"  11. "Hue"   12. "Dilution"  13. "Proline"
names(wine)

# The output will be type of wine, the column is named"Type" 
# which has the values 1, 2 or 3. You can get this by running the below command
levels(wine$Type) # "1" "2" "3"

###############################################################################
#########################Creating Trainining and Test Dataset##################
###############################################################################
dim(wine) #178  14

set.seed(66)
wine_test_index <- createDataPartition(y = wine$Type, times = 1, p = 0.1, list = FALSE)

############################ Training set #####################################
wine_train_set <- wine[-wine_test_index,]
# Store train input features in a separate variable
train_input_features <- wine_train_set %>% select(-Type)

# Store train output in a separate variable
train_output <- wine_train_set %>% select(Type) %>% .$Type

############################ Test set #####################################
wine_test_set <- wine[wine_test_index,]
# Store test input features in a separate variable
test_input_features <- wine_test_set %>% select(-Type)

# Store test output in a separate variable
test_output <- wine_test_set %>% select(Type) %>% .$Type

#################################################################################
################################### GOAL ########################################
#################### About 68% accuracy for Classification rate##################
#################################################################################

# Use the 13 features (Chemical analysis), we would like to determine the type of
# wine, which is categorized as 1, 2 or 3. We will use the 90% of the data for
# training and build the model with that. We will then predict the output using the
# test dataset. Based on the sources on the internet, it seems like getting a 
# classification rate of about 68% seems to be good accuracy
# ref: https://www.datacamp.com/community/tutorials/k-nearest-neighbor-classification-scikit-learn


###############################Building the Model #################################
# We will use the knn model, to determine the no. of neighbors(k) we will use cross validation
# to determine the ideal k value
# lets use kfold cross validation. We will take 10 resampling and we will take 90% of the data for 
# sample
cv_no <- 5
control <- trainControl(method="cv", number=cv_no, p=0.9)

train_knn <- train(wine_train_set,train_output,method="knn", tuneGrid = data.frame(k=c(1,3,4,5,7)),trControl = control )
ggplot(train_knn)

# From the above ggplot,