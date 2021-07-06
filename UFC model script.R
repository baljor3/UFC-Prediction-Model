# installing packages -----------------------------------------------------


install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
install.packages("RSelenium")


# libraries ---------------------------------------------------------------


library(rvest)
library(dplyr)
library(stringr)
library(RSelenium)


# data scraping from third party UFC site ---------------------------------


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


# UFC official website scraping -------------------------------------------

#TODO:figure out Selenium
# Download binaries, start driver, and get client object.
rd <- rsDriver(browser = "firefox", port = 4444L)
ffd <- rd$client

# Navigate to page.
ffd$open()
ffd$navigate("https://www.ufc.com/athletes/all")

# Find the load button and assign, then send click event.
load_btn <- ffd$findElement(using = "css selector", ".load-more .button")
load_btn$clickElement()

# Wait for elements to load.
Sys.sleep(2)

URLUFCStats <- read_html("https://www.ufc.com/athletes/all")

URLtostats<-URLUFCStats%>%
  html_nodes(".e-button--black")%>%
  html_attrs()

for( i in 1:length(URLtostats)){
  URLtostats[[i]]<-URLtostats[[i]][1]
}

UFCLink<- "https://www.ufc.com"

for(i in 1:length(URLtostats)){
  URLtostats[[i]]<-paste(UFCLink,URLtostats[[i]],sep="")
}

Fighterstatspage<-read_html(URLtostats[[1]])
Fighterstatspage%>%
  html_nodes(".c-record__promoted")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes(".e-chart-circle")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes(".c-overlap__stats")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes(".c-overlap__stats")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes(".c-stat-compare__group-1")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes(".c-stat-compare__group-2")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes(".c-stat-3bar__group")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes("#e-stat-body_x5F__x5F_head-txt")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes("#e-stat-body_x5F__x5F_body-txt")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes("#e-stat-body_x5F__x5F_leg-txt")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes(".c-bio__field")%>%
  str_replace_all("<.*?>","")

Fighterstatspage%>%
  html_nodes(".c-hero__headline-suffix")%>%
  str_replace_all("<.*?>","")

locateofname<-Fighterstatspage%>%
  html_nodes(".c-card-event--athlete-results__headline")%>%
  str_replace_all("<.*?>","")%>%tolower()

Locateof<-URLtostats[[1]]%>%str_locate("-")
Nameoffighter<-URLtostats[[1]]%>%str_sub(start=Locateof[1],end = 500)
Nameoffighter<-str_remove(Nameoffighter,"-")


UpcomingFight<-Fighterstatspage%>%
  html_nodes(".c-card-event--athlete-results__matchup")%>%
  str_replace_all("<.*?>","")

if(UpcomingFight[1]%>%trimws()=="Win"){
  


if(locateofname[1]%>%word(1)%>%tolower() == Nameoffighter){
  Win<-Fighterstatspage%>%
    html_nodes(".c-card-event--athlete-results__red-image")%>%
    str_replace_all("<.*?>","")
  Win<-trimws(Win[1])
}
if(locateofname[1]%>%word(3)%>%tolower() == Nameoffighter){
  Win<-Fighterstatspage%>%
    html_nodes(".c-card-event--athlete-results__blue-image")%>%
    str_replace_all("<.*?>","")
  Win<-trimws(Win[1])
}
  
}else {
  
  
  if(locateofname[1]%>%word(1)%>%tolower() == Nameoffighter){
    Win<-Fighterstatspage%>%
      html_nodes(".c-card-event--athlete-results__red-image")%>%
      str_replace_all("<.*?>","")
    Win<-trimws(Win[2])
  }
  if(locateofname[1]%>%word(3)%>%tolower() == Nameoffighter){
    Win<-Fighterstatspage%>%
      html_nodes(".c-card-event--athlete-results__blue-image")%>%
      str_replace_all("<.*?>","")
    Win<-trimws(Win[2])
  }
  
}

