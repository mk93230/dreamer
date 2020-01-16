
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(tidytext)) install.packages("tidytext")
if(!require(wordcloud2)) install.packages("wordcloud2")
if(!require(readr)) install.packages("readr")

#load libraries

library(readr)
library(knitr)
library(tidyverse)
library(tidytext)
# word cloud libraries
library(wordcloud2)
library(lubridate)

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("didn't", "did not", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

undesirable_words <- c("yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")


# String to data function
stringToDate<-function(x){
  mdy_hm(x)
}

getYear <- function(x){
  date_value=mdy_hm(x)
  return(year(date_value))
}



# The below function extracts data from the csv file

ExtractDreams <- function(){
  DreamExtract <- read.csv("C:/Kamal/R/projects/eyes2C/data/raw/DreamExtract.csv", header=FALSE)
  DreamExtract <- setNames(DreamExtract,c("Id","Name","Scene1","Scene2","Scene3","Scene4","Scene5","Created_dDate"))
  
  #DreamExtractList <- split(DreamExtract,seq(nrow(DreamExtract)))
  #DreamExtractList <- DreamExtractList[-1]
  return(DreamExtract)
}


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
