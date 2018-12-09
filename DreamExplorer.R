# Install packages
#install.packages("tidyverse")
#install.packages("lubridate")

#load libraries
library(readr)
library(knitr)
library(tidyverse)

# Including other source files
source("Cleaner.R")
source("DataRetreiver")

DreamExtract <- ExtractDreams()

#str(DreamExtract[139, ]$Scene1, nchar.max = 1000)
DreamExtract[139, ]$Scene1


# fix (expand) contractions
DreamExtract$Scene1 <- sapply(DreamExtract$Scene1, fix.contractions)

# remove special characters
DreamExtract$Scene1 <- sapply(DreamExtract$Scene1, removeSpecialChars)


# convert everything to lower case
DreamExtract$Scene1 <- sapply(DreamExtract$Scene1, tolower)

# Check the work
str(DreamExtract[139, ]$Scene1, nchar.max = 1000)

