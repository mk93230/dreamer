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
control <- trainControl(method="cv", number=10, p=0.9)
train_knn <- train(x[,col_index],y,method="knn", tuneGrid = data.frame(k=c(1,3,5,7)),trControl = control )

# lets build a random forest
library(randomForest)
rf <- randomForest(x,y,ntree=50)
imp <- importance(rf)
imp
# we can also create an image based on the importance
image(matrix(imp,28,28))