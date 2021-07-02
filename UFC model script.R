install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
install.packages("RSelenium")

library(rvest)
library(dplyr)
library(stringr)
library(RSelenium)

URLUFCStats <- read_html("http://ufcstats.com/statistics/fighters?char=a&page=all")

URLtostats<-URLUFCStats%>%
  html_nodes(".b-link")%>%
  html_attrs()

for( i in 1:length(URLtostats)){
  URLtostats[[i]]<-URLtostats[[i]][1]
}
URLtostats<-unique(URLtostats)

Databaseoffighters<-data.frame(height=NA,reach=NA,stance=NA,slpm=NA,stracc=NA,sapm=NA,strdef=NA,tdavg=NA,
                               tdacc=NA, tddef=NA, subavg=NA,wins=NA,losses=NA,lastfight=NA)

ischarempty<-character(0)
#TODO:forgot to scrape stances 
for(i in 1:length(URLtostats)){
Fighterstatspage<-read_html(URLtostats[[i]])

#Putting data into a data frame
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

if(identical(Lastfight,ischarempty)){
  Lastfight<-NA
}

Record<-Fighterstatspage%>%
  html_nodes("span")
Record<-Record[2]
Record<-str_remove_all(Record,"<.*?>")
Record<-str_remove_all(Record,"\n")
Record<-trimws(Record)
Record<-str_remove_all(Record,"[:alpha:]")
Record<-str_remove_all(Record,"\\:")
Record<-str_remove_all(Record," ")
Wins<-str_extract(Record,"[:digit:].?")
Wins<-str_remove(Wins,"-")
Losses<-str_extract(Record,"-[:digit:].?")
Losses<-str_remove_all(Losses,"-")

nameofstats<-str_replace_all(StatsofFighter,"[:digit:]","")
nameofstats<-str_replace_all(nameofstats," ","")
nameofstats<-tolower(nameofstats)

StatsofFighter<-str_replace_all(StatsofFighter,"[:alpha:]","")
StatsofFighter<-str_replace_all(StatsofFighter," ","")

Listofstatsandnames<-vector(mode="list",length = 2)
Listofstatsandnames[[1]]<-nameofstats
Listofstatsandnames[[2]]<-StatsofFighter

Rowtobeadded<-rep(NA,length(Databaseoffighters))

Rowtobeadded[12]<-Wins
Rowtobeadded[13]<-Losses
Rowtobeadded[14]<-Lastfight

for(j in 1:length(Listofstatsandnames[[1]])){
  if(Listofstatsandnames[[1]][j] %in% names(Databaseoffighters)){
    Rowtobeadded[which(names(Databaseoffighters) %in% Listofstatsandnames[[1]][j])]<-Listofstatsandnames[[2]][j]
  }
}
Databaseoffighters<-rbind(Databaseoffighters,Rowtobeadded)
}
#Databaseoffighters<-Databaseoffighters[-1,]

## offcial UFC website scraping

URLUFCStats <- read_html("https://www.ufc.com/athletes/all")

URLtostats<-URLUFCStats%>%
  html_nodes("")
#<a href="/athlete/shamil-abdurakhimov" class="e-button--black "><span class="e-button__text">
  
 # Athlete Profile
#</span></a>