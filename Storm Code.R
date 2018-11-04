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

#Read in CSV file from Python API data collection - "keyword = cyberinfrastructure"
csvloc<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/APIdata.csv"
APIdata<-read.csv(csvloc)

#Add column with keyword
APIdata$keyword<-"cyberinfrastructure"

#Read in CSV file from Python API data collection - "keyword = cyberinfrastructure"
csvloc2<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/Cyber-infrastructure.csv"
APIdata2<-read.csv(csvloc2)

#Add column with keyword
APIdata2$keyword<-"cyber infrastructure"

#Initial Cleaning 
APIdata$dunsNumber<- as.numeric(APIdata$dunsNumber)
APIdata2$dunsNumber<- as.numeric(APIdata2$dunsNumber)
APIdata$id<- as.numeric(APIdata$id)
APIdata2$id<- as.numeric(APIdata2$id)

#Combine DataFrames 
APIdata<- bind_rows(APIdata, APIdata2 )

#Format data
#lubridate 
APIdata$date<-as.Date(APIdata$date, format="%m/%d/%Y")
APIdata$startDate<-as.Date(APIdata$startDate, format="%m/%d/%Y")
APIdata$expDate<-as.Date(APIdata$expDate, format="%m/%d/%Y")
str(APIdata)

#Investigate data - Found 10 results for Michener
#str(APIdata[APIdata$piLastName=="Michener",])

#APIdata[APIdata$piLastName=="Michener",]

#Investigate data - Found 10 results for Michener
#str(APIdata[APIdata$title =="Data Observation Network for Earth",])


#Data Exploration####
APIdata$awardee<-as.character(APIdata$awardee)
APIdata$awardeeName<-as.character(APIdata$awardeeName)

APIdata$abstractText<-as.character(APIdata$abstractText)

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
APIdata$perfZipCode<-as.numeric(APIdata$perfZipCode)
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
APIdata$estimatedTotalAmt<- as.numeric(APIdata$estimatedTotalAmt)
APIdata$fundsObligatedAmt <- as.numeric(APIdata$fundsObligatedAmt)
APIdata$publicationConference<-as.character(APIdata$publicationConference)
APIdata$publicationResearch<-as.character(APIdata$publicationResearch)




#Clean Data
MissingAmt<-APIdata[is.na(APIdata$estimatedTotalAmt),]
MissingObl<-APIdata[is.na(APIdata$fundsObligatedAmt),]

MissingData<-bind_rows(MissingAmt,MissingObl)
str(MissingData)


APIdata_clean<-APIdata[!(APIdata$id %in% MissingData$id),]

nrow(APIdata_clean[is.na(APIdata_clean$estimatedTotalAmt),])
nrow(APIdata_clean[is.na(APIdata_clean$fundsObligatedAmt),])

MissingDuration<-APIdata[is.na(APIdata$duration),]
nrow(MissingDuration)

#No Duplicates 
nrow(APIdata_clean[duplicated(APIdata_clean$Title),])

###Quantitative Analysis
#Create Interval/Duration####
APIdata_clean$interval<- APIdata_clean$startDate %--% APIdata_clean$expDate

#seconds to days calculation 
secs_in_days<- (24*60*60)

APIdata_clean$duration<-as.numeric((as.duration(APIdata_clean$interval))/secs_in_days)
str(APIdata_clean$duration)

Duration_count<-APIdata_clean %>% 
  group_by(duration) %>% 
  count()
  
ggplot(Duration_count, aes(x=duration, y = n, color=n))+
  geom_point()+
  geom_vline(xintercept=mean(Duration_count$duration), color= "red")

#Write Data Frames to CSV/RDA
#APIData
Folder<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/"
Name<-"APIdata.csv"
#write.csv(APIdata, paste(Folder,Name, sep=""))

Name<-"APIdata.RData"
save(APIdata, file= paste(Folder,Name, sep=""))


#APIData
Folder<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/"
Name<-"APIdata_clean.csv"
#write.csv(APIdata_clean, paste(Folder,Name, sep=""))

Name<-"APIdata_clean.RData"
save(APIdata_clean, file= paste(Folder,Name, sep=""))

#By Transaction Type
APIdata_Grants<-APIdata %>% 
  filter(transType == "Grant")

APIdata_Coop<-APIdata %>% 
  filter(transType == "CoopAgrmnt")

#By Size 
#Awards by size
summary(APIdata$estimatedTotalAmt)

summary(APIdata_Grants$estimatedTotalAmt)
summary(APIdata_Coop$estimatedTotalAmt)

APIdata %>% 
  group_by(transType) %>% 
  summarise(avg_EstimatedAmt = mean(estimatedTotalAmt, na.rm=TRUE)) %>% 
  ggplot(aes(x=transType, y=avg_EstimatedAmt))+
  geom_bar(stat="identity")


#By Size, 75th percentile
APIdata %>% 
  filter(transType =="Grant") %>% 
  filter(estimatedTotalAmt<=719795) %>%
  select(estimatedTotalAmt) %>% 
  summary()

APIdata %>% 
  filter(transType =="Grant") %>% 
  filter(estimatedTotalAmt<=719795) %>% 
  select(estimatedTotalAmt) %>% 
  summarize(avg=mean(estimatedTotalAmt, na.rm=TRUE), n=n(), 
            sd=sd(estimatedTotalAmt, na.rm=TRUE), 
            se = sd/sqrt(n))

APIdata %>% 
  filter(transType =="Grant") %>% 
  filter(estimatedTotalAmt<=719795) %>% 
  ggplot(aes(x=estimatedTotalAmt, y=..count..))+
  geom_histogram()

#Upper 25th Percent
APIdata %>% 
  filter(transType =="Grant") %>% 
  filter(estimatedTotalAmt>719795) %>%
  select(estimatedTotalAmt) %>% 
  summary()

APIdata %>% 
  filter(transType =="Grant") %>% 
  filter(estimatedTotalAmt>719795) %>% 
  select(estimatedTotalAmt) %>% 
  summarize(avg=mean(estimatedTotalAmt, na.rm=TRUE), n=n(), 
            sd=sd(estimatedTotalAmt, na.rm=TRUE), 
            se = sd/sqrt(n))

APIdata %>% 
  filter(transType =="Grant") %>% 
  filter(estimatedTotalAmt>719795) %>% 
  ggplot(aes(x=estimatedTotalAmt, y=..count..))+
  geom_histogram()


#Scattplot duration by log(estimatedTotalAmt)
ggplot(APIdata_clean, aes(x=log(estimatedTotalAmt), y=duration, color=transType))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  ylim(0,4000)+
  geom_hline(yintercept=c(365,730,1095,1460,1825,2190,2555,2920), alpha=.25)

ggplot(APIdata_clean, aes(x=log(fundsObligatedAmt), y=duration))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  ylim(0,4000)

ggplot(APIdata_clean, aes(x=log(fundsObligatedAmt), y=log(estimatedTotalAmt)))+
  geom_point()+
  geom_smooth(method="lm")

str(APIdata$estimatedTotalAmt)

APIdata$estimatedTotalAmt<-as.numeric(APIdata$estimatedTotalAmt)



#APInoDates without Dates (for textual analysis)####
APINoDates<-APIdata 
APINoDates$date<-NULL
APINoDates$expDate <-NULL
APINoDates$startDate<-NULL
APINoDates$duration<-NULL
APINoDates$interval<-NULL

lowcost<-APINoDates %>% 
  filter(estimatedTotalAmt < 100)
lowcost

#Write data to CSV
Folder<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/"
Name<-"APINoDates.csv"
#write.csv(APINoDates, paste(Folder,Name, sep=""))

Name<-"APINoDates.RData"
save(APINoDates, file=paste(Folder,Name, sep=""))







