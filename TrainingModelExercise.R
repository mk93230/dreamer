# capture all models
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

library(caret)
library(dslabs)
#install.packages("mboost")
#library(mboost)
#install.packages("mvtnorm")
#library(mboost)
set.seed(1)
data("mnist_27")
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

fits_predicts <- sapply(fits, function(fits){ predict(fits,mnist_27$test) })

class(mnist_27$test$y)
mnist_27$test$y
factor(fits_predicts[,1])

model_accuracy_1 <- confusionMatrix(factor(fits_predicts[,1]),mnist_27$test$y)$overall["Accuracy"]
no_of_models <- 1:23
no_of_models
model_accuracy <- sapply(no_of_models,function(model_no)
  {
    confusionMatrix(factor(fits_predicts[,model_no]),mnist_27$test$y)$overall["Accuracy"]
  })
model_accuracy
class(model_accuracy)
mean(model_accuracy)

#The below object has 200 rows of predicted values for 23 models which are organized columnwise.
# we will find for each input of 200, we will determine if the majority is 2 or 7. Then we will pick
# the majority and then create a ensembled y_hat.

# use table to get the frequency
table(fits_predicts[1,])
# use max function on the above to find the number of occurences
max(table(fits_predicts[1,]))

# use the below to find which one/s has the maximum occurence/s
which.max(table(fits_predicts[1,]))

# then pull the name column to find which one has the maximum occurence - hoping that there is only
# one, if two numbers have same number of maximum occurrences I believe this may not work

names(which.max(table(fits_predicts[1,])))

no_of_rows <- 1:200
y_ensemble <- sapply(no_of_rows,function(index)
  {
    names(which.max(table(fits_predicts[index,])))
  })
y_ensemble
cm_ensemble <- confusionMatrix(factor(y_ensemble),mnist_27$test$y)$overall["Accuracy"]
cm_ensemble

