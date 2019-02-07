library(NLP)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(magrittr)


#------------Word Cloud Visualisation of total workd frequency-----------#


wordcloud(word_freq_totals$word, word_freq_totals$freq, 
          random.order = FALSE, 
          rot.per = 0.3, 
          scale = c(4,.5), 
          colors = brewer.pal(8,"Dark2"), #c('grey', 'purple', 'orange', 'blue'),   #
          font.main = 1,
          cex.main= 1.5,
          max.words = 200
          )

#--------

word_freq_totals_topwords <- word_freq_totals %>%
  top_n(25, freq)

ggplot(word_freq_totals_topwords, aes(reorder(word, freq), freq))+
  geom_col(show.legend = FALSE) +
  coord_flip()  



#--------



ggplot((word_freq_totals[word_freq_totals$freq>1,]), aes(reorder(word, freq), freq))+
  geom_col(show.legend = FALSE) +
  coord_flip()  

wordcloud((word_freq_totals[word_freq_totals$freq>1,])$word, (word_freq_totals[word_freq_totals$freq>1,])$freq, 
          random.order = TRUE, 
          rot.per = 0.3, 
          scale = c(4,.5), 
          colors = brewer.pal(8,"Dark2"), #c('grey', 'purple', 'orange', 'blue'),   #
          font.main = 1,
          cex.main= 1.5,
          max.words = 200
)


#dt-idf

dt_idf <- cbind(dtc_tidy, total=sum(dtc_tidy$count))



#View(doc_words[doc_words$count>1,])
