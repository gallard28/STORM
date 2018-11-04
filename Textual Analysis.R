#Textual Analysis 

#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(readxl)
library(gtools)
library(tidytext)
library(topicmodels)
library(tm)


#Load Data
csvloc<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/APIdata.csv"
APIdata<-read.csv(csvloc)

csvloc2<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/APIdata_clean.csv"
APIdata_clean<-read.csv(csvloc2)

csvloc3<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/APINoDates.csv"
APINoDates<-read.csv(csvloc3)


#Content Analysis####

#Project Abstracts####
data("stop_words")
abstracts<-APINoDates %>% 
  select(abstractText, title) %>% 
  mutate(linenumber = row_number()) 

#Word counts - bigrams####
tidy_abstracts<-abstracts %>% 
  unnest_tokens(abstractText, title)

tidy_abstracts

#Bitrams Approach###
abstract_bigrams <- abstracts %>% 
  unnest_tokens(bigram, abstractText, token = "ngrams", n=2)

names(abstract_bigrams)

abstract_bigrams %>% 
  count(bigram, sort=TRUE)

bigrams_separated<-abstract_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep=" ")

bigrams_filtered<-bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered %>% 
  count(word1, word2, sort=TRUE)

bigrams_united<-bigrams_filtered %>% 
  unite(bigram, word1, word2, sep=" ")

TopWords<-bigrams_united %>% 
  count(bigram) %>% 
  arrange(desc(n)) %>% 
  top_n(25)
TopWords

#TF-IDF Approach with Bigrams
bigram_tf_idf<- bigrams_united %>% 
  count(title, bigram) %>% 
  bind_tf_idf(bigram, title, n) %>% 
  arrange(desc(tf_idf))

names(bigram_tf_idf)

#TermFrequency Inverse Document Frequency Graph (Most important words in each award's abstract. Words that are important to one document within collection of documents. )####
bigram_tf_idf_graph<- bigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15)
bigram_tf_idf_graph

ggplot(bigram_tf_idf_graph, aes(x=reorder(bigram,tf_idf), y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")+
  coord_flip()

#TF-IDF with Trigrams ####
abstract_trigrams <- abstracts %>% 
  unnest_tokens(trigram, abstractText, token = "ngrams", n=3)

names(abstract_trigrams)

abstract_trigrams %>% 
  count(trigram, sort=TRUE)

trigrams_separated<-abstract_trigrams %>% 
  separate(trigram, c("word1", "word2", "word3"), sep=" ")

trigrams_separated

stop_words$word <- c(stop_words$word, "4 6 2012", "january 4 6", "1030838 oden support")

trigrams_filtered<-trigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

trigrams_filtered %>% 
  count(word1, word2, word3, sort=TRUE)

trigrams_united<-trigrams_filtered %>% 
  unite(trigram, word1, word2, word3, sep=" ")

trigrams_united<-trigrams_united %>% 
  filter(!trigram %in% stop_words$word)

trigram_tf_idf<- trigrams_united %>% 
  count(title, trigram) %>% 
  bind_tf_idf(trigram, title, n) %>% 
  arrange(desc(tf_idf))

trigram_tf_idf_graph<- trigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15)

ggplot(trigram_tf_idf_graph, aes(x=reorder(trigram,tf_idf), y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")+
  coord_flip()

#LDA Approach###
#Load and Clean Text
docs<-Corpus(VectorSource(abstracts$abstractText))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("english"), "will", "research"))

#Convert to DTM
dtm<-DocumentTermMatrix(docs)

abstractsLDA<- LDA(dtm, k=4, control=list(seed=1234))
abstractsLDA
top_terms<-tidy(abstractsLDA, matrix="beta") %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(desc(beta))
top_terms

top_terms %>% 
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales="free")+
  coord_flip()

#ProjectOutcomes Reports####
#Word counts - bigrams####
outcomes<-APINoDates %>% 
  select(projectOutComesReport, title) %>% 
  mutate(linenumber = row_number()) 

tidy_outcomes<-outcomes %>% 
  unnest_tokens(outcomesText, title)

#Bitrams Approach###
outcomes_bigrams <- outcomes %>% 
  unnest_tokens(bigram, outcomesText, token = "ngrams", n=2)

names(outcomes_bigrams)

outcomes_bigrams %>% 
  count(bigram, sort=TRUE)

bigrams_separated<-outcomes_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep=" ")

bigrams_filtered<-bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered %>% 
  count(word1, word2, sort=TRUE)

bigrams_united<-bigrams_filtered %>% 
  unite(bigram, word1, word2, sep=" ")

TopWords<-bigrams_united %>% 
  count(bigram) %>% 
  arrange(desc(n)) %>% 
  top_n(25)
TopWords

#TF-IDF Approach with Bigrams
bigram_tf_idf<- bigrams_united %>% 
  count(title, bigram) %>% 
  bind_tf_idf(bigram, title, n) %>% 
  arrange(desc(tf_idf))

names(bigram_tf_idf)

#TermFrequency Inverse Document Frequency Graph (Most important words in each award's outcomes. Words that are important to one document within collection of documents. )####
bigram_tf_idf_graph<- bigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15)
bigram_tf_idf_graph

ggplot(bigram_tf_idf_graph, aes(x=reorder(bigram,tf_idf), y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")+
  coord_flip()

#TF-IDF with Trigrams ####
outcomes_trigrams <- outcomes %>% 
  unnest_tokens(trigram, projectOutComesReport, token = "ngrams", n=3)

names(abstract_trigrams)

outcomes_trigrams %>% 
  count(trigram, sort=TRUE)

trigrams_separated<-outcomes_trigrams %>% 
  separate(trigram, c("word1", "word2", "word3"), sep=" ")

trigrams_separated

stop_words$word <- c(stop_words$word, "4 6 2012", "january 4 6", "1030838 oden support")

trigrams_filtered<-trigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

trigrams_filtered %>% 
  count(word1, word2, word3, sort=TRUE)

trigrams_united<-trigrams_filtered %>% 
  unite(trigram, word1, word2, word3, sep=" ")

trigrams_united<-trigrams_united %>% 
  filter(!trigram %in% stop_words$word)

trigram_tf_idf<- trigrams_united %>% 
  count(title, trigram) %>% 
  bind_tf_idf(trigram, title, n) %>% 
  arrange(desc(tf_idf))

trigram_tf_idf_graph<- trigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15)

ggplot(trigram_tf_idf_graph, aes(x=reorder(trigram,tf_idf), y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")+
  coord_flip()


#LDA Approach####


#Load and Clean Text
docs<-Corpus(VectorSource(outcomes$abstractText))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("english"), "will", "research"))

#Convert to DTM
dtm<-DocumentTermMatrix(docs)

outcomesLDA<- LDA(dtm, k=4, control=list(seed=1234))
outcomesLDA
top_terms<-tidy(outcomesLDA, matrix="beta") %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(desc(beta))
top_terms

top_terms %>% 
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales="free")+
  coord_flip()