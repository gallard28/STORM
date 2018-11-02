####Title: 
#Author:
#Date
#Purpose 


#Set up####

#SessionInfo for debugging later
sessionInfo()

#Libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(rvest)
library(jsonlite) 
library(httr)
library(readxl)
library(gtools)
library(lubridate)
library(tidytext)

#Install Packages


#API Request to get Data####
#Conducted through Python

#Read in CSV file from Python API data collection#
csvloc<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/APIdata.csv"
APIdata<-read.csv(csvloc)


#Format data
#lubridate 
APIdata$date<-as.Date(APIdata$date, format="%m/%d/%Y")
APIdata$startDate<-as.Date(APIdata$startDate, format="%m/%d/%Y")
APIdata$expDate<-as.Date(APIdata$expDate, format="%m/%d/%Y")
str(APIdata)

#Investigate data - Found 10 results for Michener
str(APIdata[APIdata$piLastName=="Michener",])

APIdata[APIdata$piLastName=="Michener",]

#Investigate data - Found 10 results for Michener
str(APIdata[APIdata$title =="Data Observation Network for Earth",])

#Year
APICyberInfData_2016<- APIdata %>% 
  filter(startDate>"2015/12/31" & startDate<"2017/01/01")
nrow(APICyberInfData_2016)


APICyberInfData_2017<- APIdata %>% 
  filter(date>"2016/12/31" & date<"2018/01/01")
nrow(APICyberInfData_2017)


#Write file to Folder
ExportFile<-paste(Folder,sep="","Data2016",".csv")
write.csv(Data2016, ExportFile)

#Data Exploration####
str(APIdata)
APIdata$awardee<-as.character(APIdata$awardee)
APIdata$awardeeName<-as.character(APIdata$awardeeName)

APIdata$abstractText<-as.character(APIdata$awardee)

APIdata$projectOutComesReport<-as.character(APIdata$projectOutComesReport)

AbstractSust<-APIdata %>% 
  filter(str_detect(abstractText, "sustainability") | str_detect(abstractText, "Sustainability")) %>% 
  nrow()

AbstractCost<-APIdata %>% 
  filter(str_detect(abstractText, "cost") | str_detect(abstractText, "Cost")) %>% 
  nrow()

OutcomesSust<-APIdata %>% 
  filter(str_detect(projectOutComesReport, "sustainability") | str_detect(projectOutComesReport, "Sustainability")) %>% 
  nrow()

OutcomesCost<-APIdata %>% 
  filter(str_detect(projectOutComesReport, "cost") | str_detect(projectOutComesReport, "Cost")) %>% 
  nrow()

OutcomesCost<-APIdata %>% 
  filter(str_detect(projectOutComesReport, "cost") | str_detect(projectOutComesReport, "Cost")) %>% 
  nrow()


ggplot(APIdata, aes(x=awardeeName))+
  geom_bar()

ggplot(APIdata, aes(x=transType))+
  geom_bar()


names(APIdata)
str(APIdata)

#Clean Data
APIdata$agency<-as.character(APIdata$agency)
APIdata$awardeeAddress<-as.character(APIdata$awardeeAddress)
APIdata$awardeeCity<-as.character(APIdata$awardeeCity)
APIdata$awardeeCounty<-as.character(APIdata$awardeeCounty)
APIdata$awardeeDistrictCode<-as.character(APIdata$awardeeDistrictCode)
APIdata$awardeeStateCode<-as.character(APIdata$awardeeStateCode)
APIdata$awardeeZipCode<-as.numeric(APIdata$awardeeZipCode)
APIdata$dunsNumber<-as.character(APIdata$dunsNumber)
APIdata$duns<-APIdata$dunsNumber
APIdata$dunsNumber<-NULL
APIdata$id<-as.character(APIdata$id)
APIdata$parentDunsNumber<-as.character(APIdata$parentDunsNumber)
APIdata$pdPIName<-as.character(APIdata$pdPIName)
APIdata$perfAddress<-as.character(APIdata$perfAddress)
APIdata$perfCity<-as.character(APIdata$perfCity)
APIdata$perfCountryCode<-as.character(APIdata$perfCountryCode)
APIdata$perfCounty<-as.character(APIdata$perfCounty)
APIdata$perfLocation<-as.character(APIdata$perfLocation)
APIdata$perfStateCode<-as.character(APIdata$perfStateCode)
APIdata$awardeeZipCode<-as.numeric(APIdata$awardeeZipCode)
APIdata$piEmail<-as.character(APIdata$piEmail)
APIdata$piFirstName<-as.character(APIdata$piFirstName)
APIdata$piLastName<-as.character(APIdata$piLastName)
APIdata$piMiddeInitial<-as.character(APIdata$piMiddeInitial)
APIdata$piFullName<-paste(APIdata$piFirstName,APIdata$piMiddeInitial, APIdata$piLastName, sep=" ")
APIdata$poEmail<-as.character(APIdata$poEmail)
APIdata$poName<-as.character(APIdata$poName)
APIdata$poName<-as.character(APIdata$poName)
phone <- "([1-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
APIdata$piPhone<-str_replace(APIdata$piPhone,phone, phone)
APIdata$poPhone<-str_replace(APIdata$piPhone,phone, phone  )
APIdata$primaryProgram<-as.character(APIdata$primaryProgram)
APIdata$title<-as.character(APIdata$title)
str(APIdata)



#Clean Data
MissingAmt<-APIdata[is.na(APIdata$estimatedTotalAmt),]
MissingObl<-APIdata[is.na(APIdata$fundsObligatedAmt),]
MissingAmt

APIdata_clean<-APIdata[!(APIdata$X %in% MissingAmt$X),]
APIdata_clean<-APIdata[!(APIdata$X %in% MissingObl$X),]



APIdata_clean[is.na(APIdata_clean$estimatedTotalAmt),]
APIdata_clean[is.na(APIdata_clean$fundsObligatedAmt),]

APIdata_clean$estimatedTotalAmt<-as.numeric(APIdata_clean$estimatedTotalAmt)
APIdata_clean$fundsObligatedAmt<-as.numeric(APIdata_clean$fundsObligatedAmt)


MissingDuration<-APIdata[is.na(APIdata$duration),]
APIdata_clean<-APIdata[!(APIdata$X %in% MissingDuration$X),]


#Awards by Year####
#Check data types
str(APIdata$startDate)
str(APIdata$expDate)

APIdata$interval<- APIdata$startDate %--% APIdata$expDate
str(APIdata$interval)

#seconds to days calculation 
secs_in_days<- (24*60*60)

APIdata$duration<-as.numeric((as.duration(APIdata$interval))/secs_in_days)
str(APIdata$duration)

Duration_count<-APIdata %>% 
  group_by(duration) %>% 
  
ggplot(Duration_count, aes(x=duration, y = n, color=n))+
  geom_point()

#Need to also recode into categorical variable for frequency analysis and to make it consumable to human beings. 

names(APIdata)

#Awards by size
Size_count<-APIdata %>% 
  group_by(estimatedTotalAmt) %>% 
  count()
Size_count


#Scattplot duration by log(estimatedTotalAmt)
ggplot(APIdata_clean, aes(x=log(estimatedTotalAmt), y=duration, color=transType))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  ylim(0,4000)

ggplot(APIdata_clean, aes(x=log(fundsObligatedAmt), y=duration))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  ylim(0,4000)

ggplot(APIdata_clean, aes(x=log(fundsObligatedAmt), y=log(estimatedTotalAmt)))+
  geom_point()+
  geom_smooth(method="lm")

str(APIdata$estimatedTotalAmt)

APIdata$estimatedTotalAmt<-as.numeric(APIdata$estimatedTotalAmt)

#APInoDates without Dates
APINoDates<-APIdata 
APINoDates$date<-NULL
APINoDates$expDate <-NULL
APINoDates$startDate<-NULL
APINoDates$duration<-NULL
APINoDates$interval<-NULL
names(APIdata)

lowcost<-APINoDates %>% 
  filter(estimatedTotalAmt < 100)
lowcost


#Sentiment Analysis
data("stop_words")
abstracts<-APINoDates %>% 
  select(abstractText, title) %>% 
  mutate(linenumber = row_number()) 


tidy_abstracts<-abstracts %>% 
  unnest_tokens(abstractText, title)

tidy_abstracts

abstract_bigrams <- tidy_abstracts %>% 
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
  top_n(150)
TopWords

File<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/TopWords.csv"
write.csv(TopWords, File)

bigram_tf_idf<- bigrams_united %>% 
  count(title, bigram) %>% 
  bind_tf_idf(bigram, title, n) %>% 
  arrange(desc(tf_idf))

names(bigram_tf_idf)

bigram_tf_idf_graph<- bigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(25)
bigram_tf_idf

ggplot(bigram_tf_idf_graph, aes(x=bigram, y=tf_idf, fill=title))+
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="td-idf")

library(topicmodels)


