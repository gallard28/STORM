#Analysis of NSF Files on Survey 

#Libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(rvest)
library(jsonlite) 
library(httr)
library(tidyverse)
library(readxl)
library(gtools)
library(lubridate)
library(plotly)

#Wordcloud 
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")




csvloc <- "/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/NSFSurveydata.csv"
NSFSurveydata<-read.csv(csvloc)

names(NSFSurveydata)

NSFSurveydata$abstractText<-as.character(NSFSurveydata$abstractText)

docs<-NSFSurveydata %>% 
  select(abstractText) %>% 
  filter(abstractText== "galaxies" | abstractText =="astronomy")

docs<-Corpus(VectorSource(NSFSurveydata$abstractText))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

stopwords_ls<-c("will")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("will", "dark", "galaxies")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
