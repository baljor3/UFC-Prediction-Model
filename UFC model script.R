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

c<-Fighterstatspage%>%
  html_nodes("li")
c<-str_replace_all(c,'<.*?>', "")
c<-str_replace_all(c,'\n', "")
c<-str_subset(c,"[:digit:]")
c<-trimws(c)

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