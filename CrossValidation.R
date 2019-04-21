# assessment in cross-validation concept
library(caret)
library(dslabs)
library(tidyverse)
# installing machine learning packahes
#install.packages("mlr")
#library(mlr)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]
# train method is present both in caret and mlr. We will use the one with caret
fit <- train(x_subset,y, method = "glm")
fit$results

# assessment 2 cross validation
# using the command .libPaths() to determine if Rcpp and Rinside are installed in the same location
#install.packages("devtools")
library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
# the below t test should help us to determine how many predictors can siginificantly determine
# the y value of 0 or 1. To do that we need to run ttest on column and determine the pvalue cutoff
tt <- colttests(x, y)
colnames(tt)
# statistically significant cut off 0.01
a <- tt$p.value <=0.01
index <- which(tt$p.value<=0.01)
x_subset_gene <- x[,index]
# re-run the cross validation with this subset
library(caret)
fit_gene <- train(x_subset_gene,y,method="glm")
fit_gene$results
# after you run the above you will see that its like k=1, that is overtraining the model
# that is why we need to get to different K neighbors.
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(1, 301, 25)))
ggplot(fit)

# Mnist data Digits data
#install.packages("dslabs")
# To refresh our mind, the digits are determined based on the density(?) quadrant 1 and quadrant 4, which is represented as x_1 and x_2
library(dslabs)
library(caret)
data("mnist_27")
set.seed(1995)
# createResample function is in caret package
indexes <- createResample(mnist_27$train$y,10)
class(indexes)
# mnist_27$train$y[3]
sum(indexes$Resample10==3)

# Bootstrap assignments - q3 - Montecarlo simulation
set.seed(1)

B <- 10000
q75s <- replicate(B, {
  y <- rnorm(100,0,1)
  q75 <- quantile(y, 0.75)
})
mean(q75s)
sd(q75s)
hist(q75s)
qqnorm(q75s)
# putting a line on qqnorm. qqline does not if you don't execute qqnorm
qqline(q75s)

# q4 create 10 bootstrap samples - it looks similar to montecarlo simulation but we indicate
# replace = true, instead of doing this I can also use createSample and create 10 samples.
# let me try with that first.
# let me use createResample method and then I will use similar to monte carlo simulation
set.seed(1)
y <- rnorm(100,0,1)
set.seed(1)
indexes <- createResample(y,10)
q75_1 <- quantile(y[indexes$Resample01],0.75)
q75_2 <- quantile(y[indexes$Resample02],0.75)
q75_3 <- quantile(y[indexes$Resample03],0.75)
q75_4 <- quantile(y[indexes$Resample04],0.75)
q75_5 <- quantile(y[indexes$Resample05],0.75)
q75_6 <- quantile(y[indexes$Resample06],0.75)
q75_7 <- quantile(y[indexes$Resample07],0.75)
q75_8 <- quantile(y[indexes$Resample08],0.75)
q75_9 <- quantile(y[indexes$Resample09],0.75)
q75_10 <- quantile(y[indexes$Resample10],0.75)
q75_boot <- c(q75_1,q75_2,q75_3,q75_4,q75_5,q75_6,q75_7,q75_8,q75_9,q75_10)
mean(q75_boot)
sd(q75_boot)

# lets do the bootstrap similar to montecarlo but using replace=true
set.seed(1)
B <- 10000 # changing 10 to 10000 to answer q5
y <- rnorm(100,0,1)
set.seed(1)
q75s <- replicate(B, {
  x <- sample(y,100,replace=TRUE)
  q75 <- quantile(x, 0.75)
})
mean(q75s)
sd(q75s)
# The value matched with the above, change b=10 before running the above

# Quadratic Discrimant analysis
library(dslabs)
library(tidyverse)
data("mnist_27")
mnist_27$train %>% group_by(y)


# Generative models - LDA
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
library(caret)
train_lda <- train(x,y,method="lda",preProcess = "center")
train_lda
train_lda$finalModel$means
class(train_lda$finalModel$means)
dim(train_lda$finalModel$means)
colnames(train_lda$finalModel$means)
rownames(train_lda$finalModel$means)
# converting matrix to a dataframe
means_df <- as.data.frame(train_lda$finalModel$means)
class(means_df)
means_df

# Generative models - QDA
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
library(caret)
train_qda <- train(x,y,method="qda")
train_qda
train_qda$finalModel$means

# all tissue types - not just Cerebellum and hippocampus
set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
library(caret)
train_lda <- train(x,y,method="lda")
train_lda

# Decision tree and Forests
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat) # 4th choice

# q2
plot(fit,margin = 0.1)
text(fit,cex=0.75) # ignore the value obtained, just look at the tree structure
# as of 4/19/2019 it is the 4th choice

# q3
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2) # choice 1

# q4
library(randomForest)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- randomForest(y ~ x, data = dat) # choice 1
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

#q5
plot(fit)

#q6
# nodesize is the minimum number of datapoints in a node
# for max nodes - it is the maximum number of nodes in a tree
# syntax is nodesize and maxnodes
library(randomForest)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25) # choice #4
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

# tuning parameters
getModelInfo("knn")
modelLookup("knn")

# install rborist package so that we can tune the minnode parameter
#install.packages("Rborist")
# Q1
modelLookup("Rborist")
library(Rborist)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
#minNode <- seq(25, 100, 25)
# the below grid is also a data frame with 2 parameters - which we found from model lookup for Rborist package
gridRF <- expand.grid(minNode=seq(25,100,25),predFixed=0)
set.seed(1)
train_RF <- train(y ~ ., method = "Rborist", 
                   data = dat,
                   tuneGrid = gridRF)
ggplot(train_RF,highlight = TRUE)


# Q2
library(caret)
dat %>% 
  mutate(y_hat = predict(train_RF)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2) # choice 2 when I wrote this -4/20/2019

# q3 - tissue generation dataset to determine the best complexity paramter (cp) to make the 
# partition. For example 0.01 means if the residual sum of squares increases by 1% make a partition or something like this
# all tissue types - not just Cerebellum and hippocampus
library(caret)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1991)
train_rpt <- train(x,y,method="rpart",tuneGrid =data.frame(cp=seq(0,0.1,0.01)))
ggplot(train_rpt) # cp=0 is the answer
train_rpt$finalModel
# lets plot the tree
plot(train_rpt$finalModel,margin = 0.1)
text(train_rpt$finalModel,cex=0.75)
train_rpt$finalModel$cptable

#Q4
confusionMatrix(train_rpt)
# answer is 3rd option - Placenta samples are being classified somewhat evenly across tissues. correct
#Q5
library(caret)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1991)
train_rpt <- train(x,y,method="rpart",control = rpart.control(minsplit = 0),tuneGrid =data.frame(cp=seq(0,0.1,0.01)))
confusionMatrix(train_rpt)

#q6 plotting the tree
plot(train_rpt$finalModel,margin = 0.1)
text(train_rpt$finalModel,cex=0.75)
train_rpt$finalModel$cptable
train_rpt$bestTune

#q7 using the random forest and determine the predictors
library(caret)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1991)
# we will use mtry in this one, which is number of variable randomly sampled at each split
# nodesize is the minimum size of the node
fit<- train(x,y,method="rf",tuneGrid = data.frame(mtry=seq(50,200,25)),nodesize=1)
ggplot(train_rft)

#q8
#varImp which is variable importance, determines the importance of the predictor
# variable to the classification
imp <- varImp(fit)
imp
#q9 For the imp value above, the below information is obtained
# CFHR4 value is 35.03 and the rank is 7 as of April 20, 2019.
