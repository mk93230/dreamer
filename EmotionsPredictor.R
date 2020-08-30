# Working on building a model to detect sentiment and emotions from the Dream Journal

if(!require(lubridate)) install.packages("lubridate")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidytext)) install.packages("tidytext")
if(!require(mlr)) install.packages("mlr")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(readr)) install.packages("readr")
if(!require(circlize)) install.packages("circlize")
if(!require(RWeka)) install.packages("RWeka")
if(!require(xgboost)) install.packages("xgboost")



library(lubridate) # to deal with dates little bit more easily
library(tidyverse) #tidyr, #dplyr, #magrittr, #ggplot2
library(tidytext) #unnesting text into single words
library(mlr) #machine learning framework for R
library(kableExtra) #create attractive tables
library(readr) # library for reading CSV file
library(circlize) # cool circle plots
library(RWeka) # To list and may be use learning algorithms
library(xgboost) # One of the learning algorithms

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")


#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c( "condensed", "bordered"),
                  full_width = FALSE)
}

######### Train Dataset ##############
# Collecting the emotions data from the source
emotions_data_train_balanced <- read_csv("data/emotions_data_balanced.csv")
#View(five_emotions_data_balanced)


# Now we will take each text, which could be multiple sentences and split them to words
# so that we get separate row for each word, we will also remove words that are called
# as stop words i.e I, he, she, I'll etc.
##########################################

emotions_data_train_tidy <- emotions_data_train_balanced %>% unnest_tokens(word,text) %>%
  anti_join(stop_words)

########## Test Dataset ############################
# We will do the same thing with the test dataset
####################################################
emotions_data_test_balanced <- read_csv("data/emotions_data_test.csv")
View(emotions_data_test_balanced)

############## Loading Explicit words ############################
#very small file that has a couple of words that help to identify certain genres
explicit_words <- read_csv("data/explicit_words.csv")

# Now we will take each text, which could be multiple sentences and split them to words
# so that we get separate row for each word, we will also remove words that are called
# as stop words i.e I, he, she, I'll etc.
##########################################
emotions_data_test_tidy <- emotions_data_test_balanced %>% unnest_tokens(word,text) %>%
  anti_join(stop_words)
#View(emotions_data_test_tidy)

######### Examining the data and associating the journal to emotions ############
# We will group the user by emotions and see the emotions exhibited by the user ####

# Train Data
emotions_data_train_balanced %>% group_by(emotions,user) %>%
  summarise(journal_count = n()) %>%
  my_kable_styling("Training Dataset")

# Test Data
emotions_data_test_balanced %>% group_by(emotions,user) %>% 
  summarise(journal_count = n()) %>%
  my_kable_styling("Test Dataset")

############################################################################
###### The below is to create chord diagram that ties user to the emotions
###### To see the chord diagram uncomment line 76 thru 93 ######
###### Anything that is double commented (##) can stay commented
# Lets use the circular layout and use the chord to see the emotions of the users
#emotions_chart <- emotions_data_train_balanced %>% count(emotions, user)
##View(test)

#circos.clear() # very important! Reset the circular layout parameters

##assign chord colors

#grid.col = c("Joy" = my_colors[1], "Sadness" = my_colors[2],
#             "Anger" = my_colors[5], "Disgust" = my_colors[3],
#             "Fear" = my_colors[4])

## set the global parameters for the circular layout. Specifically the gap size
#circos.par(gap.after = c(rep(5, length(unique(emotions_chart[[1]])) - 1), 15,
#                         rep(5, length(unique(emotions_chart[[2]])) - 1), 15))

#chordDiagram(emotions_chart, grid.col = grid.col, transparency = .2)
#title("Relationship Between Emotions and User")

##############################################################################
################ Feature Engineering #########################################
##############################################################################

# We will ask the operations team to classify each journals emotions to the best
# of their knowledge. Once the sample user journals are classified we can then
# look at the words that are part of the journal that could contribute to that
# emotion. We can then take n number of top words that can contribute to the
# classified emotion. Some of the common words that can be seen across emotions
# can be removed. This type of engineering is based on the content (not context)

#play with this number until you get the best results for your model.
number_of_words = 5500

top_words_per_emotion <- emotions_data_train_tidy %>%
  group_by(emotions) %>%
  mutate(emotions_word_count = n()) %>%
  group_by(emotions,word) %>% 
  #note that the percentage is also collected, when really you
  #could have just used the count, but it's good practice to use a %
  mutate(word_count = n(),
         word_pct = word_count / emotions_word_count * 100) %>%
  select(word, emotions, emotions_word_count, word_count, word_pct) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(word_pct)) %>%
  top_n(number_of_words) %>%
  select(emotions, word, word_pct)

#remove words that are in more than one genre
top_words <- top_words_per_emotion %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(multi_emotions = n()) %>%
  filter(multi_emotions < 2) %>%
  select(emotions, top_word = word)

# We will now collect top words for each emotion category
anger_words <- lapply(top_words[top_words$emotions == "Anger",], as.character)
disgust_words <- lapply(top_words[top_words$emotions == "Disgust",], as.character)
fear_words <- lapply(top_words[top_words$emotions == "Fear",], as.character)
joy_words <- lapply(top_words[top_words$emotions == "Joy",], as.character)
sadness_words <- lapply(top_words[top_words$emotions == "Sadness",], as.character)


# We will write a function for features engineering. Adding weightage to the words
# The below function is self explanatory
# Also in the below function I used encodeString function to encode special
# characters in the title description text.

features_func_emotions <- function(data) {
  features <- data %>%
    group_by(title) %>%
    mutate(word_frequency = n(),
           lexical_diversity = n_distinct(word),
           lexical_density = lexical_diversity/word_frequency,
           repetition = word_frequency/lexical_diversity,
           title_text_avg_word_length = mean(nchar(word)),
           title_word_count = lengths(gregexpr("[A-z]\\W+",
                                               title)) + 1L,
           title_length = nchar(encodeString(title)),
           large_word_count =
             sum(ifelse((nchar(word) > 7), 1, 0)),
           small_word_count =
             sum(ifelse((nchar(word) < 3), 1, 0)),
           #assign more weight to these words using "10" below
           explicit_word_count =
             sum(ifelse(word %in% explicit_words$explicit_word,10,0)),
           #assign more weight to these words using "20" below
           # for anger for example I used the weightage of more than 1 if for
           # some reason we can determine clearly if those words have more
           # weightage. See the sum function in comment, but it is for the 
           # future purpose for now I am using the same weightage as for 
           # others
           anger_word_count =
             #sum(ifelse(word %in% anger_words$top_word,20,0)),
             sum(ifelse(word %in% anger_words$top_word,1,0)),
           disgust_word_count =
             sum(ifelse(word %in% disgust_words$top_word,1,0)),
           fear_word_count =
             sum(ifelse(word %in% fear_words$top_word,1,0)),
           joy_word_count =
             sum(ifelse(word %in% joy_words$top_word,1,0)),
           sadness_word_count =
             sum(ifelse(word %in% sadness_words$top_word,1,0))
    ) %>%
    select(-word) %>%
    distinct() %>% #to obtain one record per title
    ungroup()
  
  features$emotions <- as.factor(features$emotions)
  return(features)
}

########### Creating the final training and test dataset #####################
##############################################################################

train <- features_func_emotions(emotions_data_train_tidy)
test  <- features_func_emotions(emotions_data_test_tidy)


#create classification tasks to use for modeling

#this dataset does not include emotion specific words
task_train_subset <- makeClassifTask(id = "Users Feature Subset",
                                     data = train[3:13], target = "emotions")

#create the training dataset task
task_train <- makeClassifTask(id = "Users Feature set",
                              data = train[-c(1:2)], target = "emotions")

#create the testing dataset task
task_test <- makeClassifTask(id = "New Data Test",
                             data = test[-c(1:2)], target = "emotions")

#############################################################
########### Normalize Data #################
#############################################################
#scale and center the training and test datasets
task_train_subset <- normalizeFeatures(task_train_subset, method = "standardize",
                                       cols = NULL, range = c(0, 1), on.constant = "quiet")

task_train <- normalizeFeatures(task_train, method = "standardize",
                                cols = NULL, range = c(0, 1), on.constant = "quiet")

task_test <- normalizeFeatures(task_test, method = "standardize",
                               cols = NULL, range = c(0, 1), on.constant = "quiet")


# List of learners 
#create a list of learners using algorithms you'd like to try out
lrns = list(
  makeLearner("classif.randomForest", id = "Random Forest", predict.type = "prob"),
  makeLearner("classif.rpart", id = "RPART", predict.type = "prob"),
  makeLearner("classif.xgboost", id = "xgBoost", predict.type = "prob"),
  makeLearner("classif.kknn", id = "KNN"),
  makeLearner("classif.lda", id = "LDA"),
  makeLearner("classif.ksvm", id = "SVM"),
  makeLearner("classif.PART", id = "PART"),
  makeLearner("classif.naiveBayes", id = "Naive Bayes"),
  makeLearner("classif.nnet", id = "Neural Net", predict.type = "prob")
)

#################################################################
################ Re Sampling ####################################
#################################################################

# We will take the train dataset and split them in to equal sizes 
# and will have one split as validation data set and the rest being the 
# train set and will repeat that across other data splits so that any one of
# splits could become validation dataset. We will do this n times
# n-fold cross-validation
#use stratify for categorical outcome variables
# in this case n=10
rdesc = makeResampleDesc("CV", iters = 10, stratify = TRUE)

###############################################################
################ Performance Measures #########################
###############################################################
# We will use accuracy and the time to train as the 2 measures. We have
# more measures, which you can get more details here (https://mlr.mlr-org.com/articles/tutorial/performance.html#available-performance-measures)
# let the benchmark function know which measures to obtain
# accuracy, time to train
meas = list(acc, timetrain)

############################################################################
## Lets now train the models
#it would be best to loop through this multiple times to get better results
#so consider adding a for loop here!
set.seed(123)
bmr <- benchmark(lrns, task_train_subset, rdesc, meas, show.info = FALSE)

#I'm just accessing an aggregated result directly so you can see
#the object structure and so I can use the result in markdown narrative
rf_perf <- round(bmr$results$`Users Feature Subset`$`Random Forest`$aggr[[1]],2) * 100

## [1] "BenchmarkResult"
#class(bmr)
bmr
# Looking at the above performance result it is very clear that random forest does 
# well to determine emotions.
