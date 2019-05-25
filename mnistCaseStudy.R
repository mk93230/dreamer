# Reading mnist data
library(tidyverse)
library(dslabs)
mnist <- read_mnist()
names(mnist)
class(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)
#View(mnist$train$labels)
totalRows <- nrow(mnist$train$images)
set.seed(123)
# pick 10,000 samples from 60,000 rows
index <- sample(totalRows,10000)
x<- mnist$train$images[index,]
class(x)
dim(x)
y <- factor(mnist$train$labels[index])
class(y)

# Picking 1000 samples for test data set with the same seed, so that we can test it quickly
index_test <- sample(totalRows, 1000)
x_test <- mnist$train$images[index_test,]
y_test <- factor(mnist$train$labels[index_test])

# you can do the above with 1000 samples to test the code because the processing might
# take longer for 10000 samples. To test if the code works, first use a smaller sample 
# before trying with the actual sample size

# lets remove the predictors that do not contribute to the prediction.
# we can test how many of these predictor variables varies for the output y.
# we can test this using standard deviation
# include matrix stat library
library(matrixStats)
# calculated sds for each columns (which are the predictors)
sds <- colSds(x)
# lets plot histogram with (q)uick plot
qplot(sds,bins=256)

# we can use the near zero variance command from the caret package to determine the col index for
# the near zero variance
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28,28))

# column index, the below attributes have predicting power
col_index <- setdiff(1:784,nzv)
length(col_index)

# lets work on the model fitting now
# lets assign the number of columns as the column names
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x)

# lets use kfold cross validation. We will take 10 resampling and we will take 90% of the data for 
# sample
# even for 10000 rows it will take a long time so we may need to determine how long it would
# take by changing number of rows and cross validations so that we can understand 
# this model can run for a long time, to determine how long it could take lets create
# a variable for number of rows and the number of cross validation. We can then determine
# approximately how long it will run
nrow(x)
number_of_rows <- 10000
index <- sample(nrow(x),number_of_rows)
cv_no <- 10
control <- trainControl(method="cv", number=cv_no, p=0.9)
train_knn <- train(x[index,col_index],y[index],method="knn", tuneGrid = data.frame(k=c(1,3,5,7)),trControl = control )
ggplot(train_knn)
# Now lets create a model after we found the ideal K 
knn_fit <- knn3(x[,col_index],y,k=3)
# now lets run the knn_fit model against the test data to get the predicted Y
y_hat_knn <- predict(knn_fit,x_test[,col_index],type="class")

# lets see how accurate it is by executing the confusion matrix
cm <- confusionMatrix(y_hat_knn,factor(y_test)) 
cm$overall["Accuracy"]
# lets display all the output classes's specificity and sensitivity
cm$byClass[,1:2]

# we can also build the model with random forest using rborist package. Because
# it would take lot of time we will use 5 fold cross validation. Which means Randomly sample
# will be picked which will contain 80% of the data and apply all the formula and do the same
# 4 more times - i.e randomly selecting the data for train and test.
control_rf <- trainControl(method = "cv", number = 5, p=0.8)
# in the below command I believe the predFixed determines the number of predictors to be used
grid <- expand.grid(minNode=c(1,5), predFixed=c(10,15,25,35,50))
grid
# lets create the output by training
train_rf <- train(x[,col_index],y,method="Rborist",nTree=50,trControl = control_rf,tuneGrid = grid,
                  nSamp=5000)

# lets build a random forest
library(randomForest)
rf <- randomForest(x,y,ntree=50)
imp <- importance(rf)
imp
# we can also create an image based on the importance
image(matrix(imp,28,28))