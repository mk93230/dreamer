#linstall.packages("caret")
library(caret)
# Heights dataset
install.packages("dslabs")
library("dslabs")
data(heights)
y <- heights$sex
x<- heights$height