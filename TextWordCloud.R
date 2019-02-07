library(xlsx)
library(tm) # https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
library(wordcloud)
#library(magrittr)
library(SnowballC)

#create a list of additional stop words for reuse later
addStopWords <- c("nil","na")

tempdata <- read.xlsx("D:/Data_Input/TextTopicModelling/Conference Survey.xlsx", 
                       sheetName = "2018 Conference Survey Results", 
                      startRow = 2, as.data.frame=TRUE, header=FALSE)


q.SupportCentrePurpose <- tempdata[,23]





#convert to corpus

corpus <- Corpus(VectorSource(q.SupportCentrePurpose))

#Text Cleaning

#Eliminating Extra Whitespace
corpus <- tm_map(corpus, stripWhitespace)
#Convert to Lower Case
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove Stopwords 
#- must be before punctuation removal (stop word corpora conatins punctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#remove numbers
corpus <- tm_map(corpus,removeNumbers)
#remove punctuation
corpus <- tm_map(corpus, removePunctuation)

#remove additional stopwords
corpus <- tm_map(corpus, removeWords, addStopWords)

#Stemming
corpus <- tm_map(corpus,stemDocument)


#create TDM
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)


wordcloud(d$word, d$freq)
