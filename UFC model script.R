install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")

library(rvest)
library(dplyr)
library(stringr)

URLUFCStats <- read_html("http://ufcstats.com/statistics/fighters")

URLtostats<-URLUFCStats%>%
  html_nodes(".b-link")%>%
  html_attrs()

for( i in 1:length(URLtostats)){
  URLtostats[[i]]<-URLtostats[[i]][1]
}

Fighterstatspage<-read_html(as.vector(unique(URLtostats)[[1]]))

StatsofFighter<-Fighterstatspage%>%
  html_nodes("li")
StatsofFighter<-str_replace_all(StatsofFighter,'<.*?>', "")
StatsofFighter<-str_replace_all(StatsofFighter,'\n', "")
StatsofFighter<-str_subset(StatsofFighter,"[:digit:]")
StatsofFighter<-trimws(StatsofFighter)
StatsofFighter<-str_remove_all(StatsofFighter,"[:punct:]")
StatsofFighter<-str_remove(StatsofFighter," ")

Lastfight <-Fighterstatspage%>%
  html_nodes("[class='b-flag__text']")
Lastfight<-Lastfight[1]
Lastfight<-str_replace_all(Lastfight,'<.*?>',"")

Record<-Fighterstatspage%>%
  html_nodes("span")
Record<-Record[2]
Record<-str_remove_all(Record,"<.*?>")
Record<-str_remove_all(Record,"\n")
Record<-trimws(Record)

#TODO: Put data into Database or Data frame
Databaseoffighters<-data.frame(height=c(NA),reach=c(NA),stance=c(NA),slpm=c(NA),stracc=NA,sapm=NA,strdef=NA,tdavg=NA,
           tdacc=NA, tddef=NA, subavg=NA,Records=NA,WinOrLoss=NA)
z<-str_replace_all(StatsofFighter,"[:digit:]","")
z<-str_replace_all(z," ","")
z<-tolower(z)

StatsofFighter<-str_replace_all(StatsofFighter,"[:alpha:]","")
StatsofFighter<-str_replace_all(StatsofFighter," ","")

x<-vector(mode="list",length = 2)
x[[1]]<-z
x[[2]]<-StatsofFighter

Databaseoffighters[nrow(Databaseoffighters)+1,] <- NA

if(x[[1]][3] %in% names(Databaseoffighters)){
  which(names(Databaseoffighters) %in% x[[1]][3])
  
  }


