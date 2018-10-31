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
library(tidyverse)
library(read)
library(gtools)

#Install Packages


#API Request to get Data####
base<-"http://api.nsf.gov/services/v1/awards.json?"
keyword<-"keyword=" 
PI<-"pdPIName=" 

keywordsearch<-paste(base,sep="",keyword)


#Cyberinfrastrucute Call 
term<-"cyberinfrastructure"
CI_Call<-paste(keywordsearch,sep="",term)
Get_Awards_CI<-GET(CI_Call)
Get_Awards_CI_Text<-content(Get_Awards_CI, "text")
Get_Awards_CI_JSON<-fromJSON(Get_Awards_CI_Text, flatten=TRUE)
Get_Awards_CI_df<-as.data.frame(Get_Awards_CI_JSON)
Awards_CI_df<-Get_Awards_CI_df
#Realized max limit of 25 per page.  Now write search to collect all data.

#Read in CSV file 
csvloc<-"/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/APIdata.csv"
data<-read.csv(csvloc)

#Save data backupdata_backup<-data


#Format data
data$date<-as.Date(data$date, format="%m/%d/%Y")

#Investigate data - Found 10 results for Michener
str(data[data$piLastName=="Michener",])

#Investigate data - Found 10 results for Michener
str(data[data$title=="DataONE",])

#The API ia unreliable. Download XML files and run them. 
require(flatxml)
require(xml2)
File<-'/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/NSF Data/2017/1764467.xml'

#Approach Works 
require(XML)
xmldoc<-xmlParse(File)
rootNode<-xmlRoot(xmldoc)
data<-xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
data_df<-data.frame(t(data), row.names=NULL)


#Loop for 2017####
Folder<-'/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/NSF Data/2017/'
Files<-read.delim('/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/NSF Data/2017/FileNames.txt')
Files<-as_data_frame(Files[-c(1:6),])
colnames(Files)[1]<-"files"
Files$files<-as.character(Files$files)
Files$files<-str_remove(Files$files, "\\\\")
Files$files<-str_remove(Files$files, "\\}")
Files$path<-paste(Folder, sep="", Files$files)

#Create new dataframe
Data2017<-data_df

for (i in 1:length(Files$path)){
  tryCatch({
  start<-Sys.time()
  print(paste("start loop", sep="", start))
  xmldoc<-xmlParse(Files$path[i])
  rootNode<-xmlRoot(xmldoc)
  data<-xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
  data_df<-data.frame(t(data), row.names=NULL)
  data_df$file<-Files$path[i]
  Data2017<-smartbind(Data2017,data_df)
  end<-Sys.time()
  print(paste("end loop", Files$path[i], sep="", end))
}, error=function(e){})
}

Data2017Backup<-Data2017

#Write file to Folder
ExportFile<-paste(Folder,sep="","Data2017",".csv")
write.csv(Data2017, ExportFile)

#Loop for 2016####
Year<-'2016'
Base<-'/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/NSF Data/'
Folder<-paste(Base,sep="",Year)
Folder<-paste(Folder, sep="/", Year)
Files<-data.frame(list.files(Folder))
colnames(Files)[1]<-"files"
Files$path<-paste(Folder, sep="/", Files$files)


#Create new dataframe
name<-paste("data",sep="",Year)


for (i in 1:length(Files$path)){
  tryCatch({
    start<-Sys.time()
    print(paste("start loop", sep="", start))
    xmldoc<-xmlParse(Files$path[i])
    rootNode<-xmlRoot(xmldoc)
    data<-xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
    data_df<-data.frame(t(data), row.names=NULL)
    data_df$file<-Files$path[i]
    Data2016<-smartbind(Data2016,data_df)
    end<-Sys.time()
    print(paste("end loop", Files$path[i], sep="", end))
  }, error=function(e){})
}

Data2016Backup<-Data2016

#Write file to Folder
ExportFile<-paste(Folder,sep="","Data2016",".csv")
write.csv(Data2016, ExportFile)


#Exploratory Data Analysis
str(Data2016)

Data2016$AwardTitle<-as.character(Data2016$AwardTitle)

CI<-Data2016 %>% 
  filter(str_detect(AwardTitle, "cyberinfrastructure") | str_detect(AwardTitle, "Cyberinfrastructure") )
