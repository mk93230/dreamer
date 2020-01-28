
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidytext)) install.packages("tidytext")
if(!require(mlr)) install.packages("mlr")
if(!require(kableExtra)) install.packages("kableExtra")





library(tidyverse) #tidyr, #dplyr, #magrittr, #ggplot2
library(tidytext) #unnesting text into single words
library(mlr) #machine learning framework for R
library(kableExtra) #create attractive tables


#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")