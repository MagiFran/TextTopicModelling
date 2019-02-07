library(xlsx)
library(readr)
library(NLP)
library(tm)
library(magrittr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(reshape2)



#------------import data---------------------#

#xlsx file, skip header
data_raw <- read.xlsx("Main Roads Western Australia - Post Training Review.xlsx", 
                      sheetName = "Question 8", 
                      startRow = 7, as.data.frame=TRUE, header=TRUE)




#select relevant columns
data_column <- data_raw[,3]

#remove(data_column)

#---------------------------------------------#

#create a list of stop words

#common word list###
common_words <- read_csv("CommonWords.txt", col_names = FALSE) 

common_words <- as.character(common_words$X1)

vStopWords <- c(stopwords('english'), common_words, "nil","n/a","adfffffff","xcfghj", "changed","change")
#vStopWords <- removeWords(vStopWords, "more")



#convert to corpus
corpus <- Corpus(VectorSource(data_column))

#------------Text Cleaning----------------------#
corpus_clean <- corpus %>%
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, vStopWords) %>% 
  #tm_map(removeNumbers) %>% 
  tm_map(removePunctuation)


#Stemming
corpus_stem <- tm_map(corpus_clean,stemDocument)


#---------------------------------------------#

#select stemmed or cleaned data for input 
input = corpus_clean

#------------Term Document Matrix and other outputs----------------------#
#create TDM
tdm <- TermDocumentMatrix(input)
word_freq_totals <- tdm %>%
  as.matrix() %>%
  rowSums() %>%
  sort(decreasing = TRUE) 

word_freq_totals <- data.frame(word = names(word_freq_totals), freq=word_freq_totals)

#--------------convert corpus to document term matrix---------------#

dtm <- DocumentTermMatrix(input)
rowSumDoc <- apply(dtm, 1, sum) 
dtm <- dtm[rowSumDoc > 0, ]


#--------------convert document term matrix to tidy frame---------------#

dtc_tidy <- tidy(dtm)

doc_total_words <- dtc_tidy %>% 
  group_by(document) %>% 
  summarize(total = sum(count))

doc_words <- left_join(dtc_tidy, doc_total_words)

doc_words <- doc_words %>%
  bind_tf_idf(term, document, count)

#doc_words %>%
#  select(-total) %>%
#  arrange(desc(tf_idf))

freq_by_rank <- doc_words %>% 
  group_by(document) %>% 
  mutate(rank = row_number(), 
         `term frequency` = count/total)


#----clean up------#

remove(input, rowSumDoc)







