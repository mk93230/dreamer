# Dream for wellness project - Kamal & Jeff O'Dell - We believe wellness is yours

# Install packages
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("tidytext")
#install.packages("wordcloud2")
#install.packages("readr")
#install.packages("topicmodels")
#install.packages("kableExtra")
#install.packages("ggrepel")
#install.packages("gridExtra")
#install.packages("formattable")
#install.packages("circlize")
#install.packages("plotly")

library(tidytext) #text mining, unnesting
library(topicmodels) #the LDA algorithm
library(tidyr) #gather()
library(dplyr) #awesome tools
library(ggplot2) #visualization
library(kableExtra) #create attractive tables
library(knitr) #simple table generator
library(ggrepel) #text and label geoms for ggplot2
library(gridExtra)
library(formattable) #color tile and color bar in `kables`
library(tm) #text mining
library(circlize) #to create circle visuals
library(plotly) #interactive ggplot graphs
library(lubridate) # for dates


# Including other source files
source("Cleaner.R")
source("DataRetreiver.R")
source("Utilities.R")
source("DisplayUtilities.R")



#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

# Capture all undesirable words from the dreamer

undesirable_words <- c("told", "looked", "started", "dream", 
                       "dreams", "completely","dreamed","left")

#customize ggplot2's default theme settings
#it's nice to have the options in this function
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #center the title
        axis.ticks = aticks, #set axis ticks to on or off
        panel.grid.minor = pgminor, #turn on or off the minor grid lines
        legend.title = lt, #turn on or off the legend title
        legend.position = lp) #turn on or off the legend
}


#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
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


# Breaking the dreams to words
dreamerWords <- dreamer%>%unnest_tokens(word,Scene1)

# remove the stop words. The stop words have a column called word, unnest tokens creates a column called word, it is easy to anti join them
dreamerWords_stopwords_removed <- dreamerWords%>%anti_join(stop_words)

# make the words distinct, remove the undesirable words and remove words that are less than 3 characters
dreamerWords_tidy <- dreamerWords_stopwords_removed %>% distinct() %>% filter(!word %in% undesirable_words) %>% filter(nchar(word) >3)

# the concept we will use for dreams is that, we will use the "name" as the document, scenes as multiple contents in a document
# that is these scenes(each) could be one or more topics.
# We will use the LDA for identifying words for the topics

# KJ Model - Kamal/Jeff model
#we can use probability to determine probability of words in a document and probability of topics in a document (this logic 
# is one part of LDA.
# in dreams schema we will use "name"

dreamerWords_tidy$Name <- substr(dreamerWords_tidy$Name,1,8)
# Create document term matrix
dreamerWords_dtm_balanced <- dreamerWords_tidy %>%
  # get word count per document/name to pass to cast_dtm
  count(Name,word, sort=TRUE) %>%
  ungroup() %>%
  # create a DTM with docs/Name as rows and words as columns
  cast_dtm(Name,word,n)

#assign the source dataset to generic var names
#so we can use a generic function per model
#source_dtm <- dreamerWords_dtm_balanced
#source_tidy <- dreamerWords_tidy

# remove rows with no entry
rowTotals <- apply(dreamerWords_dtm_balanced , 1, sum) #Find the sum of words in each Document
source_dtm.new  <- dreamerWords_dtm_balanced[rowTotals> 0, ]           #remove all docs without words

k <- 8 #number of topics
seed = 1234 #necessary for reproducibility
#fit the model passing the parameters discussed above
#you could have more control parameters but will just use seed here
lda <- LDA(source_dtm.new, k = k, method = "GIBBS", control = list(seed = seed))
#examine the class of the LDA object
class(lda)

# lets convert the lda object in to tidy form, the beta indicates the probability
top_terms_per_topic(lda,30)