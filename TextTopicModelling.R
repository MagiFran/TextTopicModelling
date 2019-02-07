library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(broom)
library(tidyverse)


#----set controls for model-----#
#set random seed
lda_cl <- list(seed = 1423)

#set number of topics
n = 2


#-----Latent Dirichlet allocation model-----#

x_lda <- LDA(dtm, k=n,control = lda_cl)

x_topics <- tidy(x_lda, matrix = 'beta')
x_topics

x_top_terms <- x_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

x_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


