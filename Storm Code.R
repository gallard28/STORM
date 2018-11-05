####Title: Data Cleaniung 
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
library(gtools)
library(lubridate)


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


#Data Cleaning####
APIdata$awardee<-as.character(APIdata$awardee)
APIdata$awardeeName<-as.character(APIdata$awardeeName)
APIdata$abstractText<-as.character(APIdata$abstractText)
APIdata$projectOutComesReport<-as.character(APIdata$projectOutComesReport)
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


#Missing Data - estimatedTotalAmt and fundsObligatedAmt
MissingAmt<-APIdata[is.na(APIdata$estimatedTotalAmt),]
MissingObl<-APIdata[is.na(APIdata$fundsObligatedAmt),]

MissingData<-bind_rows(MissingAmt,MissingObl)

APIdata_clean<-APIdata[!(APIdata$id %in% MissingData$id),]

nrow(APIdata_clean[is.na(APIdata_clean$estimatedTotalAmt),])
nrow(APIdata_clean[is.na(APIdata_clean$fundsObligatedAmt),])

#No Duplicates 
nrow(APIdata_clean[duplicated(APIdata_clean$Title),])

#Create APIdata_clean w/ Interval/Duration####
APIdata_clean$interval<- APIdata_clean$startDate %--% APIdata_clean$expDate

#seconds to days calculation 
secs_in_days<- (24*60*60)

#convert to numeric
APIdata_clean$duration<-as.numeric((as.duration(APIdata_clean$interval))/secs_in_days)

#check for missing
MissingDuration<-APIdata[is.na(APIdata$duration),]
nrow(MissingDuration)

#Create APIdata_clean2 with dates as characters and duration as numeric - can be used with dplyr functions. 
APIdata_clean2<-APIdata_clean
APIdata_clean2$interval<-NULL
APIdata_clean2$startDate<-as.character(APIdata_clean2$startDate)
APIdata_clean2$expDate<-as.character(APIdata_clean2$expDate)
APIdata_clean2$date<-as.character(APIdata_clean2$date)
APIdata_clean2$duration<-as.numeric(as.character(APIdata_clean2$duration))


#Filter only for Research and Related Activities ####
APIdata_clean2<-APIdata_clean2 %>% 
  filter(primaryProgram == "040100 NSF RESEARCH & RELATED ACTIVIT" | primaryProgram == "490100 NSF RESEARCH & RELATED ACTIVIT")

#Write Data Frames to CSV/RDA####
#APIData (Converted into R data types but unfiltered data from the API)
Folder<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/"

Name<-"APIdata.RData"
save(APIdata, file= paste(Folder,Name, sep=""))

#APIdata_clean - Cleaned and unffiltered data
Folder<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/STORM/"
Name<-"APIdata_clean.csv"
#write.csv(APIdata_clean, paste(Folder,Name, sep=""))

#APIdata_clean2 - cleaned and filtered data that is compatible with dplyr
Name<-"APIdata_clean2.RData" 
save(APIdata_clean2, file= paste(Folder,Name, sep=""))



#Quantitative Analysis####
#Descriptive Stats####

#Transaction Type
APIdata_Grants<-APIdata_clean2 %>% 
  group_by(transType == "Grant") %>% 
  count() %>% 
  
APIdata_Coop<-APIdata %>% 
  filter(transType == "CoopAgrmnt")

#Duration 

#Funds Obligated Amot 
#By Size 
#Awards by size
summary(APIdata$estimatedTotalAmt)
summary(APIdata_Grants$estimatedTotalAmt)
summary(APIdata_Coop$estimatedTotalAmt)


#avg duration by trans type
APIdata_clean %>% 
  group_by(transType) %>% 
  summarise(mean_duration_yrs = mean(duration, na.rm=TRUE)/365)



APIdata_clean %>% 
  group_by(primaryProgram) %>%
  summarise(mean_duration_yrs = mean(duration, na.rm=TRUE)/365) %>% 
  ggplot(aes(x=primaryProgram, y=mean_duration_yrs))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Scattplot duration by log(estimatedTotalAmt)
ggplot(APIdata_clean, aes(x=log(fundsObligatedAmt), y=duration, color=transType))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  ylim(0,4000)+
  geom_hline(yintercept=c(365,730,1095,1460,1825,2190,2555,2920), alpha=.25)

#Scatterplot Relationship between estimated and obligated amount

ggplot(APIdata_clean, aes(x=log(estimatedTotalAmt), y=log(fundsObligatedAmt)))+
  geom_point()+
  geom_smooth(method="lm")

lm(log(APIdata_clean$estimatedTotalAmt)~log(APIdata_clean$fundsObligatedAmt))


APIdata_clean2%>% 
  filter(str_detect(abstractText, "sustainability") |str_detect(projectOutComesReport, "sustainability")) %>% 
  group_by(transType) %>% 
  summarise(mean_duration = mean(duration, na.rm=TRUE)/365)









