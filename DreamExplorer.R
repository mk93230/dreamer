# Install packages
#install.packages("tidyverse")
#install.packages("lubridate")

#load libraries
library(readr)
library(knitr)
library(tidyverse)
library(tidytext)
# word cloud libraries
library(wordcloud2)

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
  ggplot(aes(x = year, y = number_of_dreams)) + 
  #geom_bar(stat = "identity")  +
  geom_line() + geom_point() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Dream Stories") +
  labs(x = "Year", y = "Number of Dreams")


# Breaking the dreams to words
dreamerWords <- dreamer%>%unnest_tokens(word,Scene1)
head(dreamerWords)

# Lets count the words, arrange them, by the way lets remove the standard words which 
# are called as stop words
# Also some undesirable words
undesirable_words <- c("told", "looked", "started", "dream", 
                       "dreams", "completely","dreamed")
dreamerWords_filtered<-dreamerWords %>% filter(!(word %in% stop_words$word)) %>% filter(!(word %in% undesirable_words)) %>% filter(year %in%2015) %>% count(word) %>% arrange(desc(n))

# word clouds for analysis
#wordcloud2(dreamerWords_filtered, size = .5)

# Words to set alarm
alarm_words <- c("suicide", "die", "depressed", "kill")

dreamerWords %>% filter(!(word %in% stop_words$word)) %>% filter((word %in% alarm_words))  %>% filter(year > 2010) %>% count(word,year) %>% ggplot(aes(year, n, color = word)) +
  geom_line() +
  geom_point() + 
  ggtitle("People who needed attention") +
  labs(x = "Year",
       y = "Occurences",
       color = "")
