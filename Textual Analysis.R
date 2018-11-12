####Title: Exploratory Textual Analysis ####
#Author: Grant A. Allard and Suzie Allard
#Purpose: 

#Set up####
#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(gtools)
library(tidytext)
library(topicmodels)
library(tm)
library(gridExtra)


#Load Data -update to harvard verse 
load("APIdata_clean.RData")
load("APIdata_clean2.RData")

#Content Analysis####
#Project Abstracts####
data("stop_words")
abstracts<-APIdata_clean2 %>% 
  select(abstractText, title) %>% 
  mutate(linenumber = row_number()) 

#Word counts - bigrams####
tidy_abstracts<-abstracts %>% 
  unnest_tokens(abstractText, title)

#Bitrams Approach###
abstract_bigrams <- abstracts %>% 
  unnest_tokens(bigram, abstractText, token = "ngrams", n=2)

abstract_bigrams %>% 
  count(bigram, sort=TRUE)

abstract_bigrams_separated<-abstract_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep=" ")

abstract_bigrams_filtered<-abstract_bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

abstract_bigrams_filtered %>% 
  count(word1, word2, sort=TRUE)

abstract_bigrams_united<-abstract_bigrams_filtered %>% 
  unite(bigram, word1, word2, sep=" ")

Abstract_TopWords<-abstract_bigrams_united %>% 
  count(bigram) %>% 
  arrange(desc(n)) %>% 
  top_n(25)

#TF-IDF Approach with Bigrams
abstract_bigram_tf_idf<- abstract_bigrams_united %>% 
  count(title, bigram) %>% 
  bind_tf_idf(bigram, title, n) %>% 
  arrange(desc(tf_idf))

#TermFrequency Inverse Document Frequency Graph (Most important words in each award's abstract. Words that are important to one document within collection of documents. )####
abstract_bigram_tf_idf_graph<- abstract_bigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15)

abstract_tf_idf_plot<-ggplot(abstract_bigram_tf_idf_graph, aes(x=reorder(bigram,tf_idf), y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")+
  coord_flip()
abstract_tf_idf_plot

#TF-IDF with Trigrams ####
abstract_trigrams <- abstracts %>% 
  unnest_tokens(trigram, abstractText, token = "ngrams", n=3)

names(abstract_trigrams)

abstract_trigrams %>% 
  count(trigram, sort=TRUE)

abstract_trigrams_separated<-abstract_trigrams %>% 
  separate(trigram, c("word1", "word2", "word3"), sep=" ")


stop_words$word <- c(stop_words$word, "4 6 2012", "january 4 6", "1030838 oden support")

abstract_trigrams_filtered<-abstract_trigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

abstract_trigrams_filtered %>% 
  count(word1, word2, word3, sort=TRUE)

abstract_trigrams_united<-abstract_trigrams_filtered %>% 
  unite(trigram, word1, word2, word3, sep=" ")

abstract_trigrams_united<-abstract_trigrams_united %>% 
  filter(!trigram %in% stop_words$word)

abstract_trigram_tf_idf<- abstract_trigrams_united %>% 
  count(title, trigram) %>% 
  bind_tf_idf(trigram, title, n) %>% 
  arrange(desc(tf_idf))

abstract_trigram_tf_idf_graph<- abstract_trigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15)

abstract_trigram_tf_idf_plot<-ggplot(abstract_trigram_tf_idf_graph, aes(x=reorder(trigram,tf_idf), y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")+
  coord_flip()

abstract_trigram_tf_idf_plot

#LDA Approach###
#Load and Clean Text
abstact_docs<-Corpus(VectorSource(abstracts$abstractText))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
abstact_docs <- tm_map(abstact_docs, toSpace, "/")
abstact_docs <- tm_map(abstact_docs, toSpace, "@")
abstact_docs <- tm_map(abstact_docs, toSpace, "\\|")
abstact_docs <- tm_map(abstact_docs, removePunctuation)
abstact_docs <- tm_map(abstact_docs, content_transformer(tolower))
abstact_docs <- tm_map(abstact_docs, removeWords, c(stopwords("english"), "will", "research"))

#Convert to DTM
abstract_dtm<-DocumentTermMatrix(abstact_docs)

abstractsLDA<- LDA(abstract_dtm, k=2, control=list(seed=1234))
abstractsLDA
astract_top_terms<-tidy(abstractsLDA, matrix="beta") %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(desc(beta))


abstract_lda_plot<-astract_top_terms %>% 
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic)+
  coord_flip()+
  ggtitle("LDA of Abstracts")+
  ylab("probability")

abstract_lda_plot

#ProjectOutcomes Reports####
#Word counts - bigrams####
#Clean Project Outcomes Reports (only look at the ones which have them)
MissingProjectOR<-APIdata_clean2[APIdata_clean2$projectOutComesReport=="",]
nrow(MissingProjectOR)

APIdata_outcomes_clean<- APIdata_clean2[!(APIdata_clean2$id %in% MissingProjectOR$id),]
nrow(APIdata_outcomes_clean)

outcomes<-APIdata_outcomes_clean %>% 
  select(projectOutComesReport, title) %>% 
  mutate(linenumber = row_number()) 

tidy_outcomes<-outcomes %>% 
  unnest_tokens(projectOutComesReport, title)

#Bigrams Approach###
outcomes_bigrams <- outcomes %>% 
  unnest_tokens(bigram, projectOutComesReport, token = "ngrams", n=2)

outcomes_bigrams %>% 
  count(bigram, sort=TRUE)

outcomes_bigrams_separated<-outcomes_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep=" ")

outcomes_bigrams_filtered<-outcomes_bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

outcomes_bigrams_filtered %>% 
  count(word1, word2, sort=TRUE)

outcomes_bigrams_united<-outcomes_bigrams_filtered %>% 
  unite(bigram, word1, word2, sep=" ")

outcomes_TopWords<-outcomes_bigrams_united %>% 
  count(bigram) %>% 
  arrange(desc(n)) %>% 
  top_n(25)

#TF-IDF Approach with Bigrams
outcomes_bigram_tf_idf<- outcomes_bigrams_united %>% 
  count(title, bigram) %>% 
  bind_tf_idf(bigram, title, n) %>% 
  arrange(desc(tf_idf))


#TermFrequency Inverse Document Frequency Graph (Most important words in each award's outcomes. Words that are important to one document within collection of documents. )####
outcomes_bigram_tf_idf_graph<- outcomes_bigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15)
bigram_tf_idf_graph

outcomes_bifgram_tf_idf_plot<-ggplot(outcomes_bigram_tf_idf_graph, aes(x=reorder(bigram,tf_idf), y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")+
  coord_flip()

#TF-IDF with Trigrams ####
outcomes_trigrams <- outcomes %>% 
  unnest_tokens(trigram, projectOutComesReport, token = "ngrams", n=3)

outcomes_trigrams %>% 
  count(trigram, sort=TRUE)

outcomes_trigrams_separated<-outcomes_trigrams %>% 
  separate(trigram, c("word1", "word2", "word3"), sep=" ")

outcomes_trigrams_filtered<-outcomes_trigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

outcomes_trigrams_filtered %>% 
  count(word1, word2, word3, sort=TRUE)

outcomes_trigrams_united<-outcomes_trigrams_filtered %>% 
  unite(trigram, word1, word2, word3, sep=" ")

outcomes_trigrams_united<-outcomes_trigrams_united %>% 
  filter(!trigram %in% stop_words$word)

outcomes_trigram_tf_idf<- outcomes_trigrams_united %>% 
  count(title, trigram) %>% 
  bind_tf_idf(trigram, title, n) %>% 
  arrange(desc(tf_idf))

outcomes_trigram_tf_idf_graph<- outcomes_trigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15)

outcomes_trigram_tf_idf_plot<-ggplot(outcomes_trigram_tf_idf_graph, aes(x=reorder(trigram,tf_idf), y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")+
  coord_flip()


#LDA Approach####

#Load and Clean Text
outcomes_docs<-Corpus(VectorSource(outcomes$projectOutComesReport))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
outcomes_docs <- tm_map(outcomes_docs, toSpace, "/")
outcomes_docs <- tm_map(outcomes_docs, toSpace, "@")
outcomes_docs <- tm_map(outcomes_docs, toSpace, "\\|")
outcomes_docs <- tm_map(outcomes_docs, removePunctuation)
outcomes_docs <- tm_map(outcomes_docs, content_transformer(tolower))
outcomes_docs <- tm_map(outcomes_docs, removeWords, c(stopwords("english"), "will", "research", "used", "use", "can", "new", "also", "last"))

#Convert to DTM
outcomes_dtm<-DocumentTermMatrix(outcomes_docs)

outcomesLDA<- LDA(outcomes_dtm, k=2, control=list(seed=1234))

outcomes_top_terms<-tidy(outcomesLDA, matrix="beta") %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(desc(beta))


LDA_outcomes_plot<-outcomes_top_terms %>% 
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic)+
  coord_flip()+
  ggtitle("LDA of Outcomes Reports")+
  ylab("probability")
  
abstract_lda_plot
LDA_outcomes_plot


grid.arrange(abstract_lda_plot, LDA_outcomes_plot, nrow=2)

