# Working on building a model to detect sentiment and emotions from the Dream Journal

if(!require(lubridate)) install.packages("lubridate")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidytext)) install.packages("tidytext")
if(!require(mlr)) install.packages("mlr")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(readr)) install.packages("readr")



library(lubridate) # to deal with dates little bit more easily
library(tidyverse) #tidyr, #dplyr, #magrittr, #ggplot2
library(tidytext) #unnesting text into single words
library(mlr) #machine learning framework for R
library(kableExtra) #create attractive tables
library(readr) # library for reading CSV file


#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")


#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c( "condensed", "bordered"),
                  full_width = FALSE)
}

######### Validation Dataset ##############
# Collecting the emotions data from the source
emotions_data_validation_balanced <- read_csv("data/emotions_data_balanced.csv")
#View(five_emotions_data_balanced)


# Now we will take each text, which could be multiple sentences and split them to words
# so that we get separate row for each work, we will also remove words that are called
# as stop words i.e I, he, she, I'll etc.
##########################################

emotions_data_validation_tidy <- emotions_data_validation_balanced %>% unnest_tokens(word,text) %>%
  anti_join(stop_words)

########## Test Dataset ############################
# We will do the same thing with the test dataset
####################################################
emotions_data_test_balanced <- read_csv("data/emotions_data_test.csv")
#View(emotions_data_test_balanced)

# Now we will take each text, which could be multiple sentences and split them to words
# so that we get separate row for each work, we will also remove words that are called
# as stop words i.e I, he, she, I'll etc.
##########################################
emotions_data_test_tidy <- emotions_data_test_balanced %>% unnest_tokens(word,text) %>%
  anti_join(stop_words)
#View(emotions_data_test_tidy)


