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

#Variables - Funds Obligated
summary(APIdata_clean2$fundsObligatedAmt)

#Funds Obligated At by TransType
FundsByTransType_plot<-APIdata_clean2 %>% 
  group_by(transType) %>% 
  summarise(sumFunds= sum(fundsObligatedAmt)/1000) %>% 
  ggplot( aes(x= transType, y=sumFunds, fill=transType)) +
  geom_bar(stat="identity")+
  geom_text(aes(x= transType, y=sumFunds+100000, label=(comma(sumFunds))))+
  ylab("Amt of Funds Obligated (in $1,000s)")


#Duration (in days)
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
  geom_boxplot(notch=FALSE)
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
median(APIdata_clean2[APIdata_clean2$transType=="Grant","durationCat"])
median(APIdata_clean2[APIdata_clean2$transType=="CoopAgrmnt","durationCat"])

#bar plot with categorical variable
APIdata_clean2 %>% 
  group_by(transType, durationCat) %>% 
  count() %>%  
  ggplot(aes(x=durationCat, y=n, fill=transType))+
  geom_bar(stat="identity")











#Scatterplot Relationship between estimated and obligated amount
  
  ggplot(APIdata_clean, aes(x=log(estimatedTotalAmt), y=log(fundsObligatedAmt)))+
    geom_point()+
    geom_smooth(method="lm")
  
  lm(log(APIdata_clean$estimatedTotalAmt)~log(APIdata_clean$fundsObligatedAmt))
  
  
  APIdata_clean2%>% 
    filter(str_detect(abstractText, "sustainability") |str_detect(projectOutComesReport, "sustainability")) %>% 
    group_by(transType) %>% 
    summarise(mean_duration = mean(duration, na.rm=TRUE)/365)
  
  
  
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
  