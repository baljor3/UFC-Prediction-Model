install.packages("rvest")

library(rvest)

URLUFCStats <- read_html("http://ufcstats.com/statistics/fighters")

URLtostats<-URLUFCStats%>%
  html_nodes(".b-link")%>%
  html_attrs()
  
