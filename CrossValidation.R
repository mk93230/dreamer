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
