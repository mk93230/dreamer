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
  