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

Sys.sleep(2)

for(i in 1:92){

# Find the load button and assign, then send click event.
load_btn <- ffd$findElement(using = "class name", "pager__item")
load_btn$clickElement()

# Wait for elements to load.
Sys.sleep(10)
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

load_btn <- ffd$findElement(using = "id", "onetrust-accept-btn-handler")
load_btn$clickElement()

Sys.sleep(2)

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

LandandAttempted[1]<-LandandAttempted[1]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")%>%
  str_remove_all("\\.")
  
str_sub(LandandAttempted[1],str_locate_all(LandandAttempted[1],"[[:digit:]]+")[[1]][1],str_locate_all(LandandAttempted[1],"[[:digit:]]+")[[1]][3])
str_sub(LandandAttempted[1],str_locate_all(LandandAttempted[1],"[[:digit:]]+")[[1]][2],str_locate_all(LandandAttempted[1],"[[:digit:]]+")[[1]][4])

LandandAttempted[2]<-LandandAttempted[2]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")%>%
  str_remove_all("\\.")

str_sub(LandandAttempted[2],str_locate_all(LandandAttempted[2],"[[:digit:]]+")[[1]][1],str_locate_all(LandandAttempted[2],"[[:digit:]]+")[[1]][3])
str_sub(LandandAttempted[2],str_locate_all(LandandAttempted[2],"[[:digit:]]+")[[1]][2],str_locate_all(LandandAttempted[2],"[[:digit:]]+")[[1]][4])

statsOne<-Fighterstatspage%>%
  html_nodes(".c-stat-compare__group-1")%>%
  str_replace_all("<.*?>","")

#sig. str. landed per min
statsOne[1]<-statsOne[1]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

str_sub(statsOne[1],str_locate_all(statsOne[1],"[[:digit:]]+")[[1]][1],str_locate_all(statsOne[1],"[[:digit:]]+")[[1]][4])

#Takedown avg per 15 min
statsOne[2]<-statsOne[2]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")%>%
  str_locate_all("[[:digit:]]+")

str_sub(statsOne[2],str_locate_all(statsOne[2],"[[:digit:]]+")[[1]][1],str_locate_all(statsOne[2],"[[:digit:]]+")[[1]][5])
#Sig. Str. Defense %
statsOne[3]<-statsOne[3]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

str_sub(statsOne[3],str_locate_all(statsOne[3],"[[:digit:]]+")[[1]][1],str_locate_all(statsOne[3],"[[:digit:]]+")[[1]][2])
#Knockdown Ratio
statsOne[4]<-statsOne[4]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

str_sub(statsOne[4],str_locate_all(statsOne[4],"[[:digit:]]+")[[1]][1],str_locate_all(statsOne[4],"[[:digit:]]+")[[1]][4])

statsTwo<-Fighterstatspage%>%
  html_nodes(".c-stat-compare__group-2")%>%
  str_replace_all("<.*?>","")
#Sig. Str. Absorbed
statsTwo[1]<-statsTwo[1]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

str_sub(statsTwo[1],str_locate_all(statsTwo[1],"[[:digit:]]+")[[1]][1],str_locate_all(statsTwo[1],"[[:digit:]]+")[[1]][4])
#Submission avg
statsTwo[2]<-statsTwo[2]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

str_sub(statsTwo[2],str_locate_all(statsTwo[2],"[[:digit:]]+")[[1]][1],str_locate_all(statsTwo[2],"[[:digit:]]+")[[1]][5])

#Takedown Defense %
statsTwo[3]<-statsTwo[3]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

str_sub(statsTwo[3],str_locate_all(statsTwo[3],"[[:digit:]]+")[[1]][1],str_locate_all(statsTwo[3],"[[:digit:]]+")[[1]][2])

#Average fight time
statsTwo[4]<-statsTwo[4]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

str_sub(statsTwo[4],str_locate_all(statsTwo[4],"[[:digit:]]+")[[1]][1],str_locate_all(statsTwo[4],"[[:digit:]]+")[[1]][4])

StatsThree<-Fighterstatspage%>%
  html_nodes(".c-stat-3bar__group")%>%
  str_replace_all("<.*?>","")

StatsThree[1]<-StatsThree[1]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

#SIG. STR. BY POSITION standing
str_sub(StatsThree[1],str_locate_all(StatsThree[1],"[[:digit:]]+")[[1]][1],str_locate_all(StatsThree[1],"[[:digit:]]+")[[1]][3])
#percentage
str_sub(StatsThree[1],str_locate_all(StatsThree[1],"[[:digit:]]+")[[1]][2],str_locate_all(StatsThree[1],"[[:digit:]]+")[[1]][4])

StatsThree[2]<-StatsThree[2]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

#SIG. STR. BY POSITION Clinch
str_sub(StatsThree[2],str_locate_all(StatsThree[2],"[[:digit:]]+")[[1]][1],str_locate_all(StatsThree[2],"[[:digit:]]+")[[1]][3])
#percentage
str_sub(StatsThree[2],str_locate_all(StatsThree[2],"[[:digit:]]+")[[1]][2],str_locate_all(StatsThree[2],"[[:digit:]]+")[[1]][4])

StatsThree[3]<-StatsThree[3]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

#SIG. STR. BY POSITION ground
str_sub(StatsThree[3],str_locate_all(StatsThree[3],"[[:digit:]]+")[[1]][1],str_locate_all(StatsThree[3],"[[:digit:]]+")[[1]][3])
#percentage
str_sub(StatsThree[3],str_locate_all(StatsThree[3],"[[:digit:]]+")[[1]][2],str_locate_all(StatsThree[3],"[[:digit:]]+")[[1]][4])

StatsThree[4]<-StatsThree[4]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

#SIG. STR. BY POSITION WINS BY KO 
str_sub(StatsThree[4],str_locate_all(StatsThree[4],"[[:digit:]]+")[[1]][1],str_locate_all(StatsThree[4],"[[:digit:]]+")[[1]][3])
#percentage
str_sub(StatsThree[4],str_locate_all(StatsThree[4],"[[:digit:]]+")[[1]][2],str_locate_all(StatsThree[4],"[[:digit:]]+")[[1]][4])

StatsThree[5]<-StatsThree[5]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

#SIG. STR. BY POSITION WINS BY DEC
str_sub(StatsThree[5],str_locate_all(StatsThree[5],"[[:digit:]]+")[[1]][1],str_locate_all(StatsThree[5],"[[:digit:]]+")[[1]][3])
#percentage
str_sub(StatsThree[5],str_locate_all(StatsThree[5],"[[:digit:]]+")[[1]][2],str_locate_all(StatsThree[5],"[[:digit:]]+")[[1]][4])

StatsThree[6]<-StatsThree[6]%>%
  str_remove_all("\n")%>%
  str_remove_all("[:alpha:]")

#SIG. STR. BY POSITION WINS BY SUB
str_sub(StatsThree[6],str_locate_all(StatsThree[6],"[[:digit:]]+")[[1]][1],str_locate_all(StatsThree[6],"[[:digit:]]+")[[1]][3])
#percentage
str_sub(StatsThree[6],str_locate_all(StatsThree[6],"[[:digit:]]+")[[1]][2],str_locate_all(StatsThree[6],"[[:digit:]]+")[[1]][4])

strikestohead<-Fighterstatspage%>%
  html_nodes("#e-stat-body_x5F__x5F_head-txt")%>%
  str_replace_all("<.*?>","")

#percentage
str_sub(strikestohead, str_locate_all(strikestohead,"[[:digit:]]+")[[1]][1],str_locate_all(strikestohead,"[[:digit:]]+")[[1]][3])
#values
str_sub(strikestohead, str_locate_all(strikestohead,"[[:digit:]]+")[[1]][2],str_locate_all(strikestohead,"[[:digit:]]+")[[1]][4])

strikestobody<-Fighterstatspage%>%
  html_nodes("#e-stat-body_x5F__x5F_body-txt")%>%
  str_replace_all("<.*?>","")

#percentage
str_sub(strikestobody, str_locate_all(strikestobody,"[[:digit:]]+")[[1]][1],str_locate_all(strikestobody,"[[:digit:]]+")[[1]][3])
#values
str_sub(strikestobody, str_locate_all(strikestobody,"[[:digit:]]+")[[1]][2],str_locate_all(strikestobody,"[[:digit:]]+")[[1]][4])

strikestolegs<-Fighterstatspage%>%
  html_nodes("#e-stat-body_x5F__x5F_leg-txt")%>%
  str_replace_all("<.*?>","")

#percentage
str_sub(strikestolegs, str_locate_all(strikestolegs,"[[:digit:]]+")[[1]][1],str_locate_all(strikestolegs,"[[:digit:]]+")[[1]][3])
#values
str_sub(strikestolegs, str_locate_all(strikestolegs,"[[:digit:]]+")[[1]][2],str_locate_all(strikestolegs,"[[:digit:]]+")[[1]][4])

biostats<-Fighterstatspage%>%
  html_nodes(".c-bio__field")%>%
  str_replace_all("<.*?>","")%>%
  str_remove_all("\n")

for(i in 1:length(biostats)){
  
  if("Age" == str_sub(biostats[i],str_locate_all(biostats[i],"[[:alpha:]]+")[[1]][1],str_locate_all(biostats[i],"[[:alpha:]]+")[[1]][2])){
  
  }
  
  if("Height" == str_sub(biostats[i],str_locate_all(biostats[i],"[[:alpha:]]+")[[1]][1],str_locate_all(biostats[i],"[[:alpha:]]+")[[1]][2])){
    
  }
  
  if("Reach" == str_sub(biostats[i],str_locate_all(biostats[i],"[[:alpha:]]+")[[1]][1],str_locate_all(biostats[i],"[[:alpha:]]+")[[1]][2])){
    
  }
  
  if("Leg reach" == str_sub(biostats[i],str_locate_all(biostats[i],"[[:alpha:]]+")[[1]][1],str_locate_all(biostats[i],"[[:alpha:]]+")[[1]][2])){
    
  }
  
}

Fighterstatspage%>%
  html_nodes(".c-hero__headline-suffix")%>%
  str_replace_all("<.*?>","")%>%
  str_remove_all("\n")

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

