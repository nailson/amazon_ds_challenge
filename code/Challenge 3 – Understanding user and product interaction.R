#install.packages("rjson")
library(jsonlite)

json_data <- fromJSON("data/challenge_3.json", flatten=TRUE)

aux = data.frame(json_data$user[[1]])
for(i in c(2:nrow(json_data))){
  aux = rbind(aux, data.frame(json_data$user[[i]]))
}

json_data = cbind(json_data, aux)
json_data$user<-NULL

json_data = json_data%>%
            mutate(
              session_id = as.character(session_id),
              unix_timestamp = as.character(unix_timestamp),
              cities = as.character(cities)
            )

# Guessing the missing country
json_data%>%count(country)
misteriours_country = json_data%>%filter(country=="")



x = strsplit(json_data$cities,",")

install.packages("combinat")
library(combinat)

y = permn(x[1:10])

install.packages("arules")

library("apriori")
apriori(data, parameter = NULL, appearance = NULL, control = NULL)


