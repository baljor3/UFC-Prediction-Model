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

#TODO: figure how to get stats from website
Fighterstatspage%>%
  html_nodes("li")


c<-Fighterstatspage%>%
  html_nodes("[class='b-list__box-list-item b-list__box-list-item_type_block']")
str_subset(c,'[:digit:]')
