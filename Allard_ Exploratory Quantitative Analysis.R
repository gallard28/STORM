####Title: Exploratory Quantitative Analysis####
#Author: Grant A. Allard and Suzie Allard
#Purpose

#Setup####
#SessionInfo for debugging later
sessionInfo()

#Libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(gtools)
library(lubridate)
library(scales)

#Load Data####
#All Data
load("APIdata.RData")

#Unfiltered by Program Data 
load("APIdata_clean.RData")

#Filtered by Program Data
load("APIdata_clean2.RData")


#Quantitative Analysis####

#Descriptive Stats####
#Data Set - Full, Missing, Filtered Data


#Variables - Trans Type
TransTypeCount_plot<-APIdata_clean2 %>% 
  group_by(transType) %>% 
  count() %>% 
  ggplot( aes(x= transType, y=n, fill=transType)) +
  geom_bar(stat="identity")+
  geom_text(aes(x= transType, y=n+100, label=(n)))+
  ylab("number of observations")

TransTypeCount_plot

#Variables - Funds Obligated
summary(APIdata_clean2$fundsObligatedAmt)

APIdata_clean2 %>% 
  group_by(transType) %>% 
  summarise(fundSum = sum(fundsObligatedAmt), fundMean= mean(fundsObligatedAmt), fundSD = sd(fundsObligatedAmt))

#Compare funds obligated between transType
Fund_Box<-APIdata_clean2 %>% 
  group_by(transType) %>% 
  ggplot(aes(x= transType, y=fundsObligatedAmt, color=transType))+
  geom_boxplot(notch=FALSE)
Fund_Box


#Funds Obligated At by TransType
FundsByTransType_plot<-APIdata_clean2 %>% 
  group_by(transType) %>% 
  summarise(sumFunds= sum(fundsObligatedAmt)/1000) %>% 
  ggplot( aes(x= transType, y=sumFunds, fill=transType)) +
  geom_bar(stat="identity")+
  geom_text(aes(x= transType, y=sumFunds+100000, label=(comma(sumFunds))))+
  ylab("Amt of Funds Obligated (in $1,000s)")


#Variables - Duration (in days)####
summary(APIdata_clean2$duration)

#Diff in mean durations between Grants and Cooperative Agreements
mean(APIdata_clean2[APIdata_clean2$transType=="Grant","duration"])
sd(APIdata_clean2[APIdata_clean2$transType=="Grant","duration"])

mean(APIdata_clean2[APIdata_clean2$transType=="CoopAgrmnt","duration"])
sd(APIdata_clean2[APIdata_clean2$transType=="CoopAgrmnt","duration"])

#Diff in means statistically significant 
t.test(x=APIdata_clean2[APIdata_clean2$transType=="Grant","duration"], y=APIdata_clean2[APIdata_clean2$transType=="CoopAgrmnt","duration"], conf.level = .95)

#Boxplot
Duration_Box<-APIdata_clean2 %>% 
  group_by(transType) %>% 
  ggplot(aes(x= transType, y=duration, color=transType))+
  geom_boxplot(notch=TRUE)
Duration_Box
  

#Scatterplots - Duration by transType ####
Duration_Scatter<-APIdata_clean2 %>% 
  group_by(transType) %>% 
  count(duration) %>% 
  ggplot(aes(x= duration, y=n, color=transType))+
  geom_point(alpha=0.4)+
  geom_vline(xintercept=c(365,730,1095,1460,1825,2190,2555,2920), alpha=.25)+
  coord_cartesian()
Duration_Scatter

Duration_Coop_Scatter<-APIdata_clean2 %>% 
  group_by(transType) %>% 
  filter(transType=="CoopAgrmnt") %>% 
  count(duration) %>%  
  ggplot(aes(x= duration, y=n, color=transType))+
  geom_vline(xintercept=c(365,730,1095,1460,1825,2190,2555,2920), alpha=.25)+
  geom_point(alpha=0.4)
Duration_Coop_Scatter

Duration_Grants_Scatter<-APIdata_clean2 %>% 
  group_by(transType) %>% 
  filter(transType=="Grant") %>% 
  count(duration) %>%  
  ggplot(aes(x= duration, y=n, color=transType))+
  geom_vline(xintercept=c(365,730,1095,1460,1825,2190,2555,2920), alpha=.25)+
  geom_point(alpha=0.4)+
  scale_color_manual(values=c("#00BFC4"))
Duration_Grants_Scatter

#Create Categorical Variable for Duration ####
APIdata_clean2$durationCat<-999

# 1 Year or less LTE 365 -> 1 
APIdata_clean2[APIdata_clean2$duration<365,"durationCat"]<-1

# between 1 and 2 years - GT 365 LTE 730 -> 2
APIdata_clean2[APIdata_clean2$duration>=365 & APIdata_clean2$duration<730 ,"durationCat"]<-2


# between 2 and 3 years - GT 730 LTE 1095 -> 3
APIdata_clean2[APIdata_clean2$duration>=730 & APIdata_clean2$duration<1095 ,"durationCat"]<-3

# between 3 and 4 years - GT 1095 LTE 1460 -> 4
APIdata_clean2[APIdata_clean2$duration>=1095 & APIdata_clean2$duration<1460 ,"durationCat"]<-4

# between 4 and 5 years - GT 1460 LTE 1825 -> 5
APIdata_clean2[APIdata_clean2$duration>=1460 & APIdata_clean2$duration<1825 ,"durationCat"]<-5

# between 5 and 6 years - GT 1825 LTE 2190 -> 6
APIdata_clean2[APIdata_clean2$duration>=1825 & APIdata_clean2$duration<2190 ,"durationCat"]<-6

# GTE 6 years - GT 2190 -> 7
APIdata_clean2[APIdata_clean2$duration>=2190,"durationCat"]<-7


#Table of new categorical variable
table(APIdata_clean2$durationCat)

#median 
median(APIdata_clean2[APIdata_clean2$transType=="Grant","duration"])
median(APIdata_clean2[APIdata_clean2$transType=="CoopAgrmnt","duration"])

#bar plot with categorical variable
APIdata_clean2 %>% 
  group_by(transType, durationCat) %>% 
  count() %>%  
  ggplot(aes(x=durationCat, y=n, fill=transType))+
  geom_bar(stat="identity")


#Conduct Descriptive Analysies based on length
APIdata_clean2 %>% 
  group_by(transType, durationCat) %>% 
  summarise(fundSum = sum(fundsObligatedAmt), fundMean= mean(fundsObligatedAmt), fundSD = sd(fundsObligatedAmt))

Fund_Duration_Box<-APIdata_clean2 %>% 
  group_by(transType,durationCat) %>% 
  ggplot(aes(x= factor(durationCat), y=fundsObligatedAmt, color=transType))+
  geom_boxplot(notch=FALSE)+
  ylim(0,5e+07)
Fund_Duration_Box


Fund_Duration_Grant_Box<-APIdata_clean2 %>% 
  group_by(transType,durationCat) %>% 
  filter(transType=="Grant") %>% 
  ggplot(aes(x= factor(durationCat), y=fundsObligatedAmt, color=transType))+
  geom_boxplot(notch=FALSE)+
  ylim(0,5e+06)+
  scale_color_manual(values=c("#00BFC4"))
Fund_Duration_Grant_Box



#Categorical variables based on presence of words in abstract/outcomes report####
#Presence of 'Sustainability' in abstract
APIdata_clean2%>% 
  filter(str_detect(abstractText, "sustainability")| str_detect(abstractText, "Sustainability")  |str_detect(projectOutComesReport, "sustainability") |str_detect(projectOutComesReport, "Sustainability")) %>% 
  group_by(transType) %>% 
  select(transType, duration) %>% 
  summary()

APIdata_clean2%>% 
    filter(str_detect(abstractText, "sustainability")| str_detect(abstractText, "Sustainability")  |str_detect(projectOutComesReport, "sustainability") |str_detect(projectOutComesReport, "Sustainability")) %>% 
    group_by(transType) %>% 
    summarise(mean_duration = mean(duration, na.rm=TRUE)/365)

#Create data frame with subsetted awards   
Sustainability<-APIdata_clean2%>% 
  filter(str_detect(abstractText, "sustainability")| str_detect(abstractText, "Sustainability")  |str_detect(projectOutComesReport, "sustainability") |str_detect(projectOutComesReport, "Sustainability")) %>% 
  group_by(transType)

#Presence of 'commercialization' in abstract
APIdata_clean2%>% 
  filter(str_detect(abstractText, "commercialization")|str_detect(abstractText, "Commercialization") | str_detect(projectOutComesReport, "commercialization") | str_detect(projectOutComesReport, "Commercialization")) %>% 
  group_by(transType) %>% 
  select(transType, duration) %>% 
  summary()

APIdata_clean2%>% 
  filter(str_detect(abstractText, "commercialization")|str_detect(abstractText, "Commercialization") | str_detect(projectOutComesReport, "commercialization") | str_detect(projectOutComesReport, "Commercialization")) %>% 
  group_by(transType) %>% 
  summarise(mean_duration = mean(duration, na.rm=TRUE)/365) 

#Create data frame with subsetted awards   
Commercialization<-APIdata_clean2%>% 
  filter(str_detect(abstractText, "commercialization")|str_detect(abstractText, "Commercialization") | str_detect(projectOutComesReport, "commercialization") | str_detect(projectOutComesReport, "Commercialization")) %>% 
  group_by(transType)


#Technology Transfer 
TT<-APIdata_clean2%>% 
  filter(str_detect(abstractText, "technology transfer")|str_detect(abstractText, "Technology Transfer") | str_detect(projectOutComesReport, "technology transfer") | str_detect(projectOutComesReport, "Technology Transfer")) %>% 
  group_by(transType)


#Join Commercialization and TT####
TT<-bind_rows(TT,Commercialization )

#Find duplicates
nrow(TT[duplicated(TT$id),])

#Remove duplicates
TT<-TT[!duplicated(TT$id),]

#Add TT categorical variable to APIdata_clean2####
nrow(APIdata_clean2[APIdata_clean2$id %in% TT$id,])

APIdata_clean2$TT<-0
APIdata_clean2[APIdata_clean2$id %in% TT$id,"TT"]<-1


#Join TT and Sustainability####
TTandSus<-bind_rows(TT,Sustainability)

#Find duplicates
nrow(TTandSus[duplicated(TTandSus$id),])

#Remove duplicates
TTandSus<-TTandSus[!duplicated(TTandSus$id),]

#Add TTandSus categorical variable to APIdata_clean2####
nrow(APIdata_clean2[APIdata_clean2$id %in% TTandSus$id,])

APIdata_clean2$TTandSus<-0
APIdata_clean2[APIdata_clean2$id %in% TTandSus$id,"TTandSus"]<-1


#Boxplot for TT and Sus on duration 
APIdata_clean2 %>% 
  group_by(TTandSus, transType) %>% 
  ggplot(aes(x=factor(TTandSus), y=duration))+
  geom_boxplot(notch=TRUE)

#Medians are very similar
APIdata_clean2 %>% 
  group_by(TTandSus, transType) %>% 
  ggplot(aes(x=factor(TTandSus), y=fundsObligatedAmt))+
  geom_boxplot(notch=TRUE)+
  ylim(0,2.5e+06)








  