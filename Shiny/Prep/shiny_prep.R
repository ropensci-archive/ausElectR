# ./prep/shiny_prep.R
# Fang Zhou, April 2016
# prepares data for the Shiny app to minimise calculations and payload needed
# for the actual server.

library(nzelect)
library(dplyr)
library(shinyapps)


load("Shiny/Data/nat_data.rda")
load("Shiny/Data/nat_map.rda")
load("Shiny/Data/abs2011.rda")
load("Shiny/Data/election.rda")


election$Name<-factor(election$DivisionNm.x)

library(dplyr)
proportions <- election %>%
  group_by(PollingPlace, PartyAb) %>%
  summarise(Votes = sum(OrdinaryVotes)) %>%
  ungroup() %>%
  group_by(PollingPlace) %>%
  mutate(prop = Votes / sum(Votes)) %>%
  filter(PartyAb %in% c("ALP","LP","LNP","GRN","PUP","NP","IND","KAP")) %>%
  mutate(lab = paste0("<center>", PollingPlace, "<br>", 
                      PartyAb, ": ", Votes, " votesbyparty, ",
                      round(prop * 100), "%</center>")) %>%
  left_join(election,by=c("PollingPlace","PartyAb")) %>%
  select(PollingPlace, PartyAb, Votes, prop, Longitude,Latitude,lab) 

parties <- data_frame(
  party = c("ALP","LP","LNP","GRN","PUP","NP","IND","KAP"),
  colour = c("red", "darkblue", "darkgreen", "purple", "steelblue", "brown", 
             "black", "yellow")
)

names(nat_data)[names(nat_data)=="SORTNAME"]="Name"
datamap<-merge(nat_map,nat_data,by=c("Name","STATE"))
datamap <- datamap[order(datamap$order),]

datamap<-datamap %>%
  group_by(STATE) %>%
  summarize(longcen=(max(long)+min(long))/2,latcen=(max(lat)+min(lat))/2) %>%
  left_join(datamap,by="STATE") %>%
  mutate(lab = paste0("<center>", STATE, "<br>", 
                      " Area Size By State, ", AREA_SQKM, "</center>")) 


states<-data_frame(
  state=c("ACT","NSW","NT","OLD","SA","TAS","VIC","WA"),
  colour=c("red", "darkblue", "darkgreen", "purple", "steelblue", "brown", 
           "black", "yellow")
)

save(parties, file = "Shiny/Data/parties.rda")
save(proportions, file = "Shiny/Data/proportions.rda")
save(states, file="Shiny/Data/states.rda")
save(datamap,file="Shiny/Data/datamap.rda")




cat("Do you want to deploy the Shiny app: [Y/n]")
deploy <- readLines(n = 1)
if(tolower(deploy) == "y"){
    deployApp("Shiny", appName = "Australia Election 2016", account = "ZhouFang928")    
}


