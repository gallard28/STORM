####Title: Wordcloud Comparison ####
#Author: Grant A. Allard and Suzie Allard
#Purpose: 

#Set up
sessionInfo()

#libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#Load data


#Wordcloud on abstracts####
abstract_tdm<- TermDocumentMatrix(abstact_docs)
abstract_m<-as.matrix(abstract_tdm)
abstract_v<- sort(rowSums(abstract_m), decreasing=TRUE)
abstract_d<-data.frame(word=names(abstract_v), freq=abstract_v)

set.seed(1234)
wordcloud(words = abstract_d$word, freq = abstract_d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#Wordcloud on Outcomes####
outcomes_v<- sort(rowSums(outcomes_m), decreasing=TRUE)
outcomes_d<-data.frame(word=names(outcomes_v), freq=outcomes_v)

set.seed(1234)
wordcloud(words = outcomes_d$word, freq = outcomes_d$freq, min.freq = 1,
                       max.words=200, random.order=FALSE, rot.per=0.35, 
                       colors=brewer.pal(8, "Dark2"))
