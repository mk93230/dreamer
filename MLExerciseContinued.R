# lets do the prediction for categorical outcome - Male or Female
library(dslabs)
library(dplyr)
library(caret)
library(tidyverse)
# There are other ways through which we can avoid this situation.
# Package for F1 score
#install.packages("MLmetrics")
library(MLmetrics)

data(heights)
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)
# when we have categorical outcome, that y=1 for female and 0 for not a female (male)
# in this case we are looking for conditional probability which is probability of female
# given the height
# lets find the proportion of female who are 66 inches tall
res<- train_set %>% filter(round(height)==66) %>% summarize(result=mean(sex=="Female"))
res$result

prob_female <- function(x){
  res <- train_set %>% filter(round(height)==x) %>% summarize(result=mean(sex=="Female"))
  res$result
}
inches <- seq(63,75,1)
result_p <- sapply(inches, prob_female)
g_points <- data.frame(inches,result_p)
g_points %>% ggplot(aes(inches,result_p)) + geom_point()
# the above looks similar to regression line, we will make the categorical
# conditional expectation is nothing but the normal distribution (random) within each input
# in case of categorical outcome we can turn them to 1s and 0s. The proportion is nothing but
# conditional expectation. We can then calculate least square loss.
lm_fit <- train_set %>% mutate(y=as.numeric(sex=="Female")) %>% lm(y~height,data=.)
# in case of categorical output, the predict method gives the value which is the probability (conditional probability)
p_hat <- predict(lm_fit,test_set) # you don't need to set output to binary in test set because we are using only predictors i.e x value

# lets calculate the y_hat, we can say its females if probability is greater than 0.5
y_hat <- ifelse(p_hat>0.5, "Female", "Male") %>% factor()

confusionMatrix(y_hat,reference = test_set$sex)

# lets talk about the p_hat we calculated above and I mentioned its conditional probability but then if you look at the value
# you will negative values and values above 1. That does not make sense. You can see it
# yourself
min(p_hat)
max(p_hat)

# the logistics regression will resolve this issue, by doing log of odds, 
# odds indicate what are the chances of something happening versus not happening
# then you take a log of that, this will put the number between 0 to 1. Because
# there is no conditional probability any more, the least square error will not work
# therefore we should rely on maximum likelihood estimate. GLM function should take care of that
# lets do the same as above
glm_fit <- train_set %>% mutate(y=as.numeric(sex=="Female")) %>% glm(y~height,data=.,family="binomial")
p_hat_logit <- predict(glm_fit,newdata=test_set,type="response") # you don't need to set output to binary in test set because we are using only predictors i.e x value
# In the above notice that we set type=response, otherwise we won't get conditional probabilities
# value, you'll end up getting log p/(1-p) values and etc
# lets calculate the y_hat, we can say its females if probability is greater than 0.5
y_hat_logit <- ifelse(p_hat_logit>0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat_logit,reference = test_set$sex)
# letscalculate the F1 score for logistics regression
F1_Score(test_set$sex,y_hat_logit,positive = NULL)
# letscalculate the F1 score for linear regression
F1_Score(test_set$sex,y_hat,positive = NULL)
# as you can see logistics regression does better with F1 score, closer to 1 is better, 0 is
# not good. F1 takes in account the right balance for precision and recall

# Lets do the KNN for determining gender based on height. I don't think you need to change it
# to binary, because this technique for category does not need conditional probability
set.seed(1)
knn_fit <-  knn3(sex~height,data=train_set,k=5)
# For KNN you don't need p_hat
y_hat_knn <- predict(knn_fit,test_set,type="class")
confusionMatrix(y_hat_knn,reference = test_set$sex)
F1_Score(test_set$sex,y_hat_knn,positive = NULL)

# lets create a function to determine F1 score value for different Ks
calculate_F1 <- function(x){
  set.seed(1)
  test_index <- createDataPartition(heights$height,times=1,p=0.5,list=FALSE)
  train_set <- heights %>% slice(-test_index)
  test_set <- heights %>% slice(test_index)
  
  knn_fit <-  knn3(sex~height,data=train_set,k=x)
  # For KNN you don't need p_hat
  y_hat_knn <- predict(knn_fit,test_set,type="class")
  confusionMatrix(y_hat_knn,reference = test_set$sex)
  F1_Score(test_set$sex,y_hat_knn,positive = NULL)
  
}

k_value <- seq(1,100,2)
result_F1 <- sapply(k_value,calculate_F1)
k_f1_df <- data.frame(k_value,result_F1)
k_f1_df %>% ggplot(aes(k_value,result_F1)) + geom_point()

# assessment for KNN
library(dslabs)
library(caret)
library(purrr)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
test_index <- createDataPartition(y,p=0.5,times=1,list=FALSE)
train_set_x <- tissue_gene_expression$x[-test_index,]
train_set_y <- tissue_gene_expression$y[-test_index]
#train_set <- list(train_set_y,train_set_x)
test_set_x <- tissue_gene_expression$x[test_index,]
test_set_y <- tissue_gene_expression$y[test_index]
ks <- seq(1,11,2)
#accuracy <- map_df(ks,function(x) {
    #test_set <- list(test_set_y,test_set_x)
    # we will use the matrix approach for knn model
    #print(x)
    set.seed(1)
    knn_fit <-  knn3(train_set_x,train_set_y,k=1)
    # when using predict with knn, we can get probability values or outcome with maximum
    # probability. By using type=class we will get outcomes
    # you can see in the below function I used the input test dataset, which is what required
    predict_knn <- predict(knn_fit,test_set_x,type="class")
    # lets run the confusion matrix for accuracy
    confusionMatrix(predict_knn,reference = test_set_y)
#})
    
