# ./prep/shiny_prep.R
# Fang Zhou, April 2016
# prepares data for the Shiny app to minimise calculations and payload needed
# for the actual server.

library(nzelect)
library(dplyr)
library(shinyapps)


#load("Shiny/nat_data.rda")
#load("Shiny/nat_map.rda")
#load("Shiny/abs2011.rda")
load("Shiny/election.rda")

#mapmerge <- merge(nat_map, abs2011, by="Name")
#mapmerge <- mapmerge[order(mapmerge$order),]
#election$Name<-factor(election$DivisionNm.x)

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

save(parties, file = "Shiny/parties.rda")
save(proportions, file = "Shiny/proportions.rda")



cat("Do you want to deploy the Shiny app: [Y/n]")
deploy <- readLines(n = 1)
if(tolower(deploy) == "y"){
    deployApp("Shiny", appName = "Australia Election 2016", account = "ZhouFang928")    
}


