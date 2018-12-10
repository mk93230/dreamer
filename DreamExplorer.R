# Install packages
#install.packages("tidyverse")
#install.packages("lubridate")

#load libraries
library(readr)
library(knitr)
library(tidyverse)

# Including other source files
source("Cleaner")
source("DataRetreiver")
source("Utilities")

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

dreamer <- DreamExtract %>% mutate(year=getYear(DreamExtract$Created_dDate))

# Remove all NA's from the year by filtering the data
dreamer <- dreamer %>% filter(!is.na(dreamer$year))


dreamer %>%
  group_by(year) %>%
  summarise(number_of_dreams = n()) %>%
  ggplot(aes(x = year, y = number_of_dreams,color="red")) + 
  geom_bar(stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Dream Stories") +
  labs(x = "Year", y = "Number of Dreams")


