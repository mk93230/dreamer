#packages
library(readr)
library(knitr)
library(tidyverse)
library(purr)

# Including other source files
source("Cleaner.R")

DreamExtract <- read.csv("C:/Kamal/R/projects/eyes2C/data/raw/DreamExtract.csv", header=FALSE)
DreamExtract <- setNames(DreamExtract,c("Id","Name","Scene1","Scene2","Scene3","Scene4","Scene5","Created_dDate"))
DreamExtractList <- split(DreamExtract,seq(nrow(DreamExtract)))
DreamExtractList <- DreamExtractList[-1]
#lst <- DreamExtractList[[1]][-which(is.na(DreamExtractList[[1]]))]


# Remove the contracted word
fix.contractions(DreamExtract$Scene1[2])

