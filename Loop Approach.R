#Loop Approach

#Check API reliability against the All-Awards XML downloads from NSF 
require(flatxml)
require(xml2)
File<-'/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/NSF Data/2017/1764467.xml'

#Approach Works 
require(XML)
xmldoc<-xmlParse(File)
rootNode<-xmlRoot(xmldoc)
data<-xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
data_df<-data.frame(t(data), row.names=NULL)


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

#Loop for 2017####
#Folder<-'/Users/GrantAllard/Documents/Allard Scholarship/Conferences and Journals - CFPs, Etc. /ASIS&T 2018/Cyberinfrastructure poster/Data and Analysis/NSF Data/2017/'
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