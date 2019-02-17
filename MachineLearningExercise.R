#install.packages("caret")
#install.packages("dslabs")
# Galton Heights
#install.packages("HistData")
# Machine learning library
library(caret)
# Heights dataset
library("dslabs")
data(heights)
y <- heights$sex
x<- heights$height

library(HistData)
galton_heights <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>% 
  rename(son = childHeight)
