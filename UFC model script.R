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


# UFC official website scraping -------------------------------------------

# the number of fighers is 1019 the number of fighers per load is 11. Hence,
# (1019-11)/11 = 92 this is the total number of times we should load.

# Download binaries, start driver, and get client object.
rd <- rsDriver(browser = "firefox", port = 4444L)
ffd <- rd$client

# Navigate to page.
ffd$navigate("https://www.ufc.com/athletes/all")

load_btn <- ffd$findElement(using = "id", "onetrust-accept-btn-handler")
load_btn$clickElement()

for(i in 1:92){

# Find the load button and assign, then send click event.
load_btn <- ffd$findElement(using = "class name", "pager__item")
load_btn$clickElement()

# Wait for elements to load.
Sys.sleep(2)
}

#to get the current page with all the loaded fighters.
URLUFCStats<-read_html(ffd$getPageSource()[[1]])


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

ffd$navigate(URLtostats[[1]])

load_btn <- ffd$findElement(using = "class name", "e-button--white")
load_btn$clickElement()

Sys.sleep(2)

Fighterstatspage<-read_html(ffd$getPageSource()[[1]])

Fighterstatspage%>%
  html_nodes(".c-record__promoted")%>%
  str_replace_all("<.*?>","")%>%
  str_remove_all("\n")

percentofaccuracy<-Fighterstatspage%>%
  html_nodes(".e-chart-circle")%>%
  str_replace_all("<.*?>","")

percentofaccuracy[1]%>%
  str_remove("\n")%>%
  str_remove("[:digit:].*")%>%
  tolower()%>%
  str_remove_all(" ")

percentofaccuracy[1]%>%
  str_remove("\n")%>%
  str_remove_all("[:alpha:].*?")%>%
  str_remove("[:digit:]..")%>%
  str_remove("%")

percentofaccuracy[2]%>%
  str_remove("\n")%>%
  str_remove("[:digit:].*")%>%
  tolower()%>%
  str_remove_all(" ")

percentofaccuracy[2]%>%
  str_remove("\n")%>%
  str_remove_all("[:alpha:].*?")%>%
  str_remove("[:digit:]..")%>%
  str_remove("%")
  
LandandAttempted<-Fighterstatspage%>%
  html_nodes(".c-overlap__stats")%>%
  str_replace_all("<.*?>","")

LandandAttempted[1]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")%>%
  str_remove_all("\\.")%>%
  str_locate_all("[[:digit:]]+")

LandandAttempted[2]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")%>%
  str_remove_all("\\.")%>%
  str_locate_all("[[:digit:]]+")


statsOne<-Fighterstatspage%>%
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

