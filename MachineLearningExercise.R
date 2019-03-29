#install.packages("caret")
# install.packages('e1071', dependencies=TRUE) # needed in addition to caret package
#install.packages("dslabs")
# Galton Heights
#install.packages("HistData")
#install.packages("Purr") //this package is for map function but its not available
# the below library is for map function which is available in purrr (3rs) package
library(purrr)
# Machine learning library
library(caret)
# Heights dataset
library("dslabs")
library(dplyr)
data(heights)
y <- heights$sex
x<- heights$height

library(HistData)
galton_heights <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>% 
  rename(son = childHeight)


set.seed(1)
rmse <- replicate(n,
                     {
                       test_index <- createDataPartition(dat$y,times=1,p=0.5, list=FALSE)
                       train_set <- dat %>% slice(-test_index)
                       test_set <- dat %>% slice(test_index)
                       fit <- lm(y~x,data=train_set)
                       mse <- mean(((fit$coef[1]+test_set$x*fit$coef[2])-test_set$y)^2)
                       rmse_r = sqrt(mse)
                      }
                    )
mean(rmse)
sd(rmse)

rmse_fun <- function(n)
  {
    print(n)
    Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
    dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
      data.frame() %>% setNames(c("x", "y"))
    print(nrow(dat))
    result<- replicate(100,
              {
                test_index <- createDataPartition(dat$y,times=1,p=0.5, list=FALSE)
                train_set <- dat %>% slice(-test_index)
                test_set <- dat %>% slice(test_index)
                fit <- lm(y~x,data=train_set)
                mse <- mean(((fit$coef[1]+test_set$x*fit$coef[2])-test_set$y)^2)
                rmse_r = sqrt(mse)
              }
    )
    print(mean(result))
    print(sd(result))
  }
n_vector <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
sapply(n_vector, rmse_fun)


# For exercise q4
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
  set.seed(1)
  result<- replicate(100,
                   {
                     test_index <- createDataPartition(dat$y,times=1,p=0.5, list=FALSE)
                     train_set <- dat %>% slice(-test_index)
                     test_set <- dat %>% slice(test_index)
                     fit <- lm(y~x,data=train_set)
                     mse <- mean(((fit$coef[1]+test_set$x*fit$coef[2])-test_set$y)^2)
                     rmse_r = sqrt(mse)
                   }
  )
  print(mean(result))
  print(sd(result))
  
  
  
  set.seed(1)
  n <- 1000
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  set.seed(1)
  result<- replicate(100,
                     {
                       test_index <- createDataPartition(dat$y,times=1,p=0.5, list=FALSE)
                       train_set <- dat %>% slice(-test_index)
                       test_set <- dat %>% slice(test_index)
                       fit <- lm(y~x_1 + x_2,data=train_set)
                       mse <- mean(((fit$coef[1]+test_set$x_1*fit$coef[2]+test_set$x_2*fit$coef[3])-test_set$y)^2)
                       rmse_r = sqrt(mse)
                     }
  )
  print(mean(result))
  print(sd(result))
  
  
  set.seed(1)
  n <- 1000
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  set.seed(1)
  test_index <- createDataPartition(dat$y,times=1,p=0.5, list=FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x_1 + x_2,data=train_set)
  mse <- mean(((fit$coef[1]+test_set$x_1*fit$coef[2]+test_set$x_2*fit$coef[3])-test_set$y)^2)
  rmse_r = sqrt(mse)
  rmse_r
  
  
  # Heights exercise
  set.seed(2)
  test_index<- createDataPartition(y,p=0.5,times=1,list=FALSE)
  train_set <- heights %>% slice(-test_index)
  test_set <- heights %>% slice(test_index)
  train_set %>% filter(round(height)==height) %>% summarize(mean(sex=='Female'))
  
  
  # Digits data
  #install.packages("dslabs")
  library(dslabs)
  data("mnist_27")
  mnist_27$true_p %>% ggplot(aes(x_1,x_2,z=p, fill=p)) + geom_raster() + scale_fill_gradientn(colors=c("#F8766D", "white","#00BFC4" )) + stat_contour(breaks=c(0.5),color="black")
  
 
  # Another exercise 
  set.seed(2)
  make_data <- function(n = 1000, p = 0.5, 
                        mu_0 = 0, mu_1 = 2, 
                        sigma_0 = 1,  sigma_1 = 1){
    
    y <- rbinom(n, 1, p)
    f_0 <- rnorm(n, mu_0, sigma_0)
    f_1 <- rnorm(n, mu_1, sigma_1)
    x <- ifelse(y == 1, f_1, f_0)
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    
    list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
         test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
  }
  dat <- make_data()
  
  # smoothing exercise
  data("polls_2008")
  qplot(day,margin,data=polls_2008)
  
  ggplot(polls_2008,aes(day,margin)) + geom_point() + stat_smooth(method="lm", color="red")
  
  span <- 7
  fit <- with(polls_2008,ksmooth(day,margin,x.points = day,kernel="box",bandwidth=span))
  polls_2008 %>% mutate(smooth=fit$y) %>% ggplot(aes(day,margin)) + geom_point(size=3,alpha=0.5, color="grey") + geom_line(aes(day,smooth),color="red")
  
  total_days <- diff(range(polls_2008$day))
  span <- 21/total_days
  fit <- loess(margin~day,degree=1,span=span,data=polls_2008)
  polls_2008 %>% mutate(smooth=fit$fitted) %>% ggplot(aes(day,margin)) + geom_point(size=3,alpha=0.5, color="grey") + geom_line(aes(day,smooth),color="red")
  
  
  # mortality counts exercise - Puerto Rico 
  library(tidyverse)
  library(purrr)
  #install.packages("pdftools")
  library(pdftools)
  library(lubridate)
  
  fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
  dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
    s <- str_trim(s)
    header_index <- str_which(s, "2015")[1]
    tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
    month <- tmp[1]
    header <- tmp[-1]
    tail_index  <- str_which(s, "Total")
    n <- str_count(s, "\\d+")
    out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
    s[-out] %>%
      str_remove_all("[^\\d\\s]") %>%
      str_trim() %>%
      str_split_fixed("\\s+", n = 6) %>%
      .[,1:5] %>%
      as_data_frame() %>% 
      setNames(c("day", header)) %>%
      mutate(month = month,
             day = as.numeric(day)) %>%
      gather(year, deaths, -c(day, month)) %>%
      mutate(deaths = as.numeric(deaths))
  }) %>%
    mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                          "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
    mutate(date = make_date(year, month, day)) %>%
    filter(date <= "2018-05-01")
  # Omitting na from the data set
    dat <- na.omit(dat)
    total_days <- diff(range(dat$date))
    span <- 60/as.integer(total_days)
    
    fit <- loess(deaths~as.numeric(date),degree=1,span=span,data=dat)
    dat %>% mutate(smooth=fit$fitted) %>% ggplot(aes(date,deaths)) + geom_point(size=3,alpha=0.5, color="grey") + geom_line(aes(date,smooth),color="red")
    
# days of the year, color it by year
    dat %>% 
      mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
      ggplot(aes(day, smooth, col = year)) +
      geom_line(lwd = 2)

    
# 3rd exercise - predictive power of digits.
    library(broom)
    mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
    count(mnist_27$train)
    head(mnist_27$train)
    span=10
    fit <- loess(as.numeric(y)~x_2,degree=1,span=span,data=mnist_27$train)
    mnist_27$train %>% mutate(smooth=fit$fitted) %>% ggplot(aes(x_2,y)) + geom_point(size=3,alpha=0.5, color="grey") + geom_line(aes(x_2,smooth),color="red")
   
# Working on Matrices
    library(dslabs)
    mnist <- read_mnist()
    x<-mnist$train$images[1:1000,]
    y<- mnist$train$labels[1:1000]
    
    grid <- matrix(x[1,],28,28)
    image(1:28,1:28,grid[,28:1])
    
    x_1 <- 1:5
    x_2 <- 6:10
    cbind(x_1,x_2)
    dim(x_1)
    dim(as.matrix(x_1))
    my_vector <- 1:15
    mat <- matrix(my_vector,5,3)
    mat_t <- matrix(my_vector,3,5,byrow = TRUE)
    identical(t(mat),mat_t)
    
# lets sum the pixel values for each digit, to understand the pixel density for numbers
    sums <- rowSums(x)
    avg <- rowMeans(x)
    rowsds <- apply(x,1,sd)
    colSums <- colSums(x)
    colAvgs <- colMeans(x)
    colsds <- apply(x,2,sd)
    
# converting to factors
    y_factor <- as.factor(y)
    
# creating a dataframe
    train_mnist_dataframe <- data.frame(y_factor,avg)
    train_mnist_dataframe %>% ggplot(aes(y_factor,avg)) + geom_boxplot()
    
# second challenge with digits
#    install.packages("matrixStats")
    library(matrixStats)
    sds <- colSds(x)
    hist(sds)
    qplot(sds, bins = 256)
    
    library(caret)
    nzv <- nearZeroVar(x)
    grid_sd <- matrix(1:784 %in% nzv,28,28)
    image(1:28,1:28,grid_sd[,28:1])
    col_index <- setdiff(1:ncol(x), nzv)
    length(col_index)
    
# ommiting columns that are below certain standard deviation
    x_new <- x[,sds>60]
    dim(x_new)
    
# preserve matrix by using drop attribute
    class(x[,1,drop=FALSE])
    
# converting matrices to vectors
    mat_to_vect <- as.vector(mat)
    mat_to_vect
    
# histogram of all our predictors 
    qplot(as.vector(x),bins=30,color=I("black"))
    # shows a clear of dichotomy of part with ink and parts without ink
    new_x <- x
    new_x[new_x<50] <- 0
    
    mat_test <- matrix(1:15,5,3)
    mat_test
    mat_test[mat_test<3] <-0
    mat_test[mat_test>3 & mat_test <11] <-0
    
# lets work on binarizing the data
    bin_x <- x
    bin_x[bin_x<255/2] <-0
    bin_x[bin_x>255/2] <-1 # as you can see by dividing by 2 not only it splits it gives a decimal point number which our matrices contain int therefor we don't need to put equal to and greater than to catch all values- just a different approach
    # another technique
    bin_x <- (x>255/2)*1

# lets see the binarized image now
    grid_non_bin <- matrix(x[1,],28,28)
    grid_bin <- matrix(bin_x[1,],28,28)
    image(1:28,1:28,grid_non_bin[,28:1])
    image(1:28,1:28,grid_bin[,28:1])
    
# Scaling each row of the matrix
  (x-rowMeans(x))/rowSds(x)
# to do it for columns, you need to transpose the matrix so it becomes rows, do the calculations and transpose again
    X_mean_test <- t(t(x)-colMeans(x))
    X_mean_0 <- sweep(x,2,colMeans(x))
    identical(X_mean_test,X_mean_0)
    X_standardized <- sweep(X_mean_0,2,colSds(x),FUN="/")
    
# multiplication
    x %*% t(x) # this is also called as cross product, multiplying by its transpose
    crossprod(x) # does the same thing as above
# inverse can be achieved thru solve function
#    solve(crossprod(x)) # this function shows an error needs to be investigated
    
    
# assessment
    g <- matrix(rnorm(100*10), 100, 10)
    dim(g)
    nrow(g)
    ncol(g)
    
# Proportion of pixels in the grey area by digits
    training_set <- mnist$train$images
    training_set_grey <- (training_set>49 & training_set<206)*1
    training_set_grey_avg <- rowMeans(training_set_grey)
    label_factor <- as.factor(mnist$train$labels)
    mnist_dataframe <- data.frame(label_factor,training_set_grey_avg)
    mnist_dataframe %>% ggplot(aes(label_factor,training_set_grey_avg)) + geom_boxplot()
    
    # complete proportion
    training_set_grey_vector <- as.vector(training_set_grey)
    mean(training_set_grey_vector)
    
# Distance
    set.seed(0)
    if(!exists("mnist"))
    {
      mnist <- read_mnist()
    }
    ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
    x <- mnist$train$images[ind,]
    y <- mnist$train$labels[ind]
    y[1:3]
    x_1 <- x[1,]
    x_2 <- x[2,]
    x_3 <- x[3,]
    # lets calculate the distances between digits
    sqrt(sum((x_1-x_2)^2))
    sqrt(sum((x_1-x_3)^2))
    sqrt(sum((x_2-x_3)^2))
    # The other way to approach this
    sqrt(crossprod(x_1-x_2))
    sqrt(crossprod(x_1-x_3))
    sqrt(crossprod(x_2-x_3))
    
    # you can calculate the distance for all the observations using the function dist
    d <- dist(x)
    class(d)
    # lets convert dist to matrices
    d_mat <- as.matrix(d)
    d_mat[1:3,1:3]
    # lets create a image out of this
    image(d_mat)
    # lets order them by the labels , 2 and 7
    image(d_mat[order(y),order(y)])
    dim(d_mat)
    
    # lets do the distance between the pixels, first we need to transpose the matrix
    d_t <- dist(t(x))
    d_t_mat <- as.matrix(d_t)
    dim(d_t_mat)
    d_t_mat_492 <- d_t_mat[492,]
    d_t_mat_492
    image(1:28,1:28,matrix(d_t_mat_492,28,28))
    
# Assessment - distance
    library(dslabs)
    data("tissue_gene_expression")
    dim(tissue_gene_expression$x)
    x_input <- tissue_gene_expression$x
    y_tissues <-tissue_gene_expression$y
    x_input[1,1:4]
    table(tissue_gene_expression$y)
    class(tissue_gene_expression$y)
    
    y_tissues[c(1,2,39,40,73,74)]
    
    d_samples <- dist(tissue_gene_expression$x)
    d_samples_mat <- as.matrix(d_samples)
    d_samples_mat[c(1,2,39,40,73,74),c(1,2,39,40,73,74)]
    image(d_samples_mat)
    
# knn exercise
    # Digits data
    #install.packages("dslabs")
    library(dslabs)
    data("mnist_27")
    library(caret)
    mnist_27$train$y
    # need to change the y value to binary, and make 1 for y==7 if thats want we wanted to do
    mnist_27$train <- mnist_27$train %>%
      mutate(y = ifelse(y==7, 1, 0))
    mnist_27$train$y
    fit_glm <- glm(y~x_1+x_2,data=mnist_27$train,family="binomial")
    p_hat_logistic <- predict(fit_glm,mnist_27$test)
    y_hat_logistic <- factor(ifelse(p_hat_logistic>0.5,7,2))
    confusionMatrix(y_hat_logistic,reference=mnist_27$test$y)$overall[1]
    
    # lets build the KNN fit
    knn_fit <- knn3(y~.,data=mnist_27$train)
    knn_fit
    # second way is with matrix
    x <- as.matrix(mnist_27$train[,2:3])
    y <- factor(mnist_27$train$y)
    knn_fit1 <- knn3(x,y)
    knn_fit1
    
    library(dslabs)
    data("mnist_27")
    library(caret)
    # when doing KNN you don't need to do condition probability, because it will calculate probability
    # for all cases. In this case 7 and 2.

    knn_fit <- knn3(y~.,data=mnist_27$train, k=5)
    y_hat_knn <- predict(knn_fit,mnist_27$test, type="class")

    confusionMatrix(y_hat_knn,reference=mnist_27$test$y)$overall[1]
    
    # to detect overtraining run the confusion matrix on the training set, accuracy could be higher
    # make sure you run the predict on the train before running the confusion matrix
    y_hat_knn <- predict(knn_fit,mnist_27$train, type="class")
    confusionMatrix(y_hat_knn,reference=mnist_27$train$y)$overall[1]
    
    # Lets run the same code as above with higher K - by including many more neighbors we are 
    # oversmoothing the situation
    knn_fit <- knn3(y~.,data=mnist_27$train, k=401)
    y_hat_knn <- predict(knn_fit,mnist_27$test, type="class")
    
    confusionMatrix(y_hat_knn,reference=mnist_27$test$y)$overall[1]
    
# KNN assessment
    library(purrr)
    # Machine learning library
    library(caret)
    # Heights dataset
    library("dslabs")
    library(dplyr)
    
    library(HistData)
    galton_heights <- GaltonFamilies %>% 
      filter(childNum == 1 & gender == "male") %>%
      select(father, childHeight) %>% 
      rename(son = childHeight)
    
    library(caret)
    y <- galton_heights$son
    test_index <- createDataPartition(y,times=1,p=0.5, list=FALSE)
    train_set <- galton_heights %>% slice(-test_index)
    test_set <- galton_heights %>% slice(test_index)
    # without linear regression
    avg <- mean(train_set$son)
    # our squared loss
    sqloss <- mean((avg-test_set$son)^2)
    
    # least square method for regression line
    fit <- lm(son~father, data=train_set)
    mse<-mean((test_set$son-(fit$coef[1]+fit$coef[2]*test_set$father))^2)
    rmse_r = sqrt(mse)
    # predict function does the same thing as y_hat formula
    y_hat <- predict(fit,test_set)
    #now you can do the mean square
    mse_p <- mean((y_hat - test_set$son)^2)
    
# lets do the prediction for categorical outcome - Male or Female
    library(dslabs)
    library(dplyr)
    data(heights)
    y <- heights$sex
    x<- heights$height
    test_index <- createDataPartition(y,p=0.5,times=1,list=FALSE)
    train_set <- heights %>% slice(-test_index)
    test_set <- heights %>% slice(test_index)
# when we have ccategorical outcome, that y=1 for female and 0 for not a female (male)
# in this case we are looking for conditional probability which is probability of female
# given the height
    # lets find the proportion of female who are 66 inches tall
    train_set %>% filter(round(height)==66) %>% summarize(mean(sex=="Female"))