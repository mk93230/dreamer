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

# Q5
# Changes names for model_accuracy object
names(model_accuracy) <- models
names(model_accuracy)
model_accuracy

# eventhough the below display 0.84, the percentage expected from the course is 0.845,
# some error in the course material, however use this to determine the model which got
# accuracy more than 0.845 instead of 0.84. The answer is 1. 
cm_ensemble

#Lets find the accuracies for a model
class(fits)
# list all accuracies for 1st model
fits[[1]]$results["Accuracy"]
no_of_models
cv_accuracy_results <- sapply(no_of_models,function(model_no)
  {
   fits[[model_no]]$results["Accuracy"]
  
  })
class(cv_accuracy_results)
min(cv_accuracy_results[[3]])
# By default the train does cross validation and therefore some of the results have more than
# one accuracy. We will take the min of the accuracy of there is more than one accuracy value.
# we cannot take average of all the accuracy values for that model because the model does 
# not provide that accuracy when ran with the data. Either min or max is appropriate because it
# ran with those, To be conservative we will take the min, which says that it can atleast to this
# accuracy or better. Lets find the min value for all model and average them.
cv_accuracy_results_single <- sapply(no_of_models,function(model_no)
  {
    min(cv_accuracy_results[[model_no]])
  })

cv_accuracy_results_single
mean(cv_accuracy_results_single)
class(cv_accuracy_results_single)

# to answer q7, lets remove alls the models which has value less than 0.80
models_filtered_index <- which(cv_accuracy_results_single<0.80)
models_filtered_index

# lets remove colums from fits_predicts matrix
fits_predicts_filtered <- fits_predicts[,-models_filtered_index]
dim(fits_predicts_filtered)

# now lets run the ensemble similar to the above

no_of_rows <- 1:200
y_ensemble_filtered <- sapply(no_of_rows,function(index)
{
  names(which.max(table(fits_predicts_filtered[index,])))
})
y_ensemble_filtered
cm_ensemble_filtered <- confusionMatrix(factor(y_ensemble_filtered),mnist_27$test$y)$overall["Accuracy"]
cm_ensemble_filtered
