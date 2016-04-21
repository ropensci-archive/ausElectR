library(dplyr)

# get all the HouseStateFirstPrefsByPollingPlace data
# assume we are in the project dir
dir <- paste0(getwd(), "/AECdata//HouseStateFirstPrefsByPollingPlace")
files <- list.files(dir,  full.names = TRUE )

# list of data frames with each state
library(purrr)
election_results_df <- files %>% map_df(read.csv, skip = 1, stringsAsFactors = FALSE)
names(election_results_df)
str(election_results_df)

# get lat long for polling places
polling_place_location <- read.csv(paste0(getwd(), "/AECdata/GeneralPollingPlacesDownload-17496.csv"), 
                                   skip = 1,
                                   stringsAsFactors =  FALSE)

names(polling_place_location)
head(polling_place_location)

# quick look at locations 
library(ggplot2)
library(plotly)
p <- ggplot(polling_place_location, aes(Longitude, Latitude, label = PremisesNm)) +
  geom_point() +
  coord_equal() 
p
# ggplotly(p)

# join polling place locations to election results
election_results_df_loc <- full_join(election_results_df, polling_place_location, by = "PollingPlaceID")
head(election_results_df_loc)

# only electorates


# do we have any polling places with no location data?
election_results_df_loc %>% 
  select(DivisionID.x, PollingPlace, StateAb, GivenNm, Surname, PartyNm, Elected, Latitude, Longitude) %>% 
  filter(Elected == "Y") %>% 
  filter(is.na(Latitude)) %>% 
  View
# yes, 540 hospitals, etc.  

# do we have polling places with no election data?
election_results_df_loc %>% 
  select(DivisionID.y, PollingPlaceID, Elected, Latitude, Longitude) %>% 
  filter(is.na(Elected)) %>% 
  View
# yes 104 locations with no election data. 

# Let's exclude locaitons with no election data
election_results_df_loc <- election_results_df_loc %>%
                             filter(!is.na(Elected)) 
head(election_results_df_loc)
                            
# plot 
ggplot(election_results_df_loc, aes(Longitude, Latitude)) +
  geom_point() +
  coord_equal() 



str(election_results_df_loc)
# get rid of factors
election_results_df_loc_no_fac <- election_results_df_loc %>% 
  map_if(is.factor, as.character) %>% 
  rbind_list
str(election_results_df_loc_no_fac)


# seems like there's two names for the ALP , called LP in NSW?
unique(election_results_df_loc_no_fac$PartyNm)
election_results_df_loc_no_fac$PartyNm <- with(election_results_df_loc, ifelse(PartyNm == "Labor", 
                                     "Australian Labor Party",
                                     PartyNm))
# similar for the greens
election_results_df_loc_no_fac$PartyNm <- with(election_results_df_loc, ifelse(PartyNm == "Australian Greens", 
                                                                               "The Greens",
                                                                               PartyNm))
str(election_results_df_loc_no_fac)
unique(election_results_df_loc_no_fac$PartyNm)

# missing points for NT
election_results_df_loc_no_fac %>% 
  filter(StateAb == "NT") %>% View
  

## how many electoral districts?
length(unique(election_results_df_loc_no_fac$DivisionID.x))

# remove a few redundant cols
election_results_df_loc_no_fac_no_dup <- 
  election_results_df_loc_no_fac %>% 
  select(-DivisionID.y, -DivisionID.y, -PollingPlaceNm)

# plot 
ggplot(election_results_df_loc_no_fac_no_dup, aes(Longitude, Latitude)) +
  geom_point() +
  coord_equal() 
# why removed? because some with no location, and many dups for many candidates per location

p1 <- election_results_df_loc_no_fac_no_dup %>% 
  select(PollingPlace, Latitude, Longitude) %>% 
  group_by(PollingPlace) %>% 
  slice(1) %>% 
  ggplot(aes(Longitude, Latitude, label = PollingPlace)) +
  geom_point() +
  coord_equal()
p1
# remvoe 147 with no lat longs
# ggplotly(p1)

# save as CSV
write.csv(election_results_df_loc_no_fac_no_dup, "AECdata/HouseFirstPrefsByPollingPlaceAllStates.csv")

# write as rds
aec2013 <- election_results_df_loc_no_fac_no_dup
save(aec2013, file="echidnaR/data/aec2013.rda")
load("echidnaR/data/aec2013.rda")
# load("echidnaR/data/abs2011.rda")


################################################################
## Overall results for first preferences -----------------------
# by party
election_results_df_loc_no_fac %>% 
  select(PartyAb, PartyNm, OrdinaryVotes) %>% 
  group_by(PartyNm, PartyAb) %>% 
  summarise(total_votes = sum(OrdinaryVotes)) %>% 
  ungroup() %>%
  arrange(desc(total_votes))

# compare to AEC data http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm
# First Preferences By Party
aes_first_pref <- read.csv(paste0(getwd(), "/AECdata/HouseFirstPrefsByPartyDownload-17496.csv"), skip = 1)
names(aes_first_pref)
aes_first_pref %>% 
  select(PartyNm, OrdinaryVotes) %>% 
  arrange(desc(OrdinaryVotes)) %>% 
  head()

# my result does not agree with AEC result for OrdinaryVotes... why?
# sum of polling place counts is less than AEC official for ALP and some others...

################################################################
# winner for each electorate ----------------------------------

election_results_df_loc %>% 
  group_by(DivisionID.x) %>% 
  select(DivisionID.x, StateAb, GivenNm, Surname, PartyNm, Elected) %>% 
  filter(Elected == "Y") %>% 
  slice(1) %>% 
  ungroup %>% 
  arrange(desc(Surname)) %>% 
  head

# check with AEC data for winner for each electorate
aes_winners <- read.csv(paste0(getwd(), "/AECdata/HouseMembersElectedDownload-17496.csv"), skip = 1)
names(aes_winners)
aes_winners %>% 
  select(DivisionID, StateAb, GivenNm, Surname, PartyNm) %>%  
  arrange(desc(Surname)) %>% 
  head

## yes, good match

################################################################
# Comparing party and candidate votes of several parties -------
proportions <- election_results_df_loc %>%
  group_by(DivisionID.x) %>%
  summarise(ProportionLabour = sum(OrdinaryVotes[PartyNm == "Australian Labor Party"]) / sum(OrdinaryVotes),
            ProportionLiberal = sum(OrdinaryVotes[PartyNm == "Liberal"]) / sum(OrdinaryVotes),
            ProportionLiberalNational = sum(OrdinaryVotes[PartyNm == "Liberal National Party"]) / sum(OrdinaryVotes),
            ProportionGreens = sum(OrdinaryVotes[PartyNm == "The Greens"]) / sum(OrdinaryVotes),
            ProportionPalmer = sum(OrdinaryVotes[PartyNm == "Palmer United Party"]) / sum(OrdinaryVotes)) 

library(GGally)
ggpairs(proportions, columns = 2:ncol(proportions))

################################################################
# Geographical location of voting places ----------------------

nat_map <- read.csv("AECdata/National-map.csv")
nat_data <- read.csv("AECdata/National-data.csv")

names(nat_map)
names(nat_data)

str(nat_map)
unique(nat_map$ELECT_DIV) # division name

# common variables
# maps   election   census
# "ELECT_DIV" == "DivisionNm" == "ID"
# region is the name of the electorate

names(election_results_df_loc)
names(polling_place_location)


# plot electorates to show proportion of labor votes
election_results_df_loc %>%
  # compute proportion of votes for labor by
  group_by(PollingPlaceID) %>% 
  summarise(ProportionLabour = sum(OrdinaryVotes[PartyNm == "Australian Labor Party"]) / sum(OrdinaryVotes)) %>% 
  mutate(MostlyLabour = ifelse(ProportionLabour > 0.5, 
                                 "Mostly voted ALP", "Mostly didn't vote ALP")) %>% 
  left_join(polling_place_location, by = 'PollingPlaceID') %>% 
  left_join(nat_data, by = c("DivisionNm" = "ELECT_DIV")) %>% 
  # plot
  ggplot(aes(map_id=id, fill = ProportionLabour)) +
  geom_map(map=nat_map) +
  expand_limits(x=nat_map$long, y=nat_map$lat) +
  theme_map() +
  coord_map() +
  theme(legend.position = "bottom")

################################################################
# Rolling up results to electorate

# watch out for zeros...
election_results_df_loc_no_fac %>% 
  group_by(PollingPlaceID) %>%
  summarise(TotalVotes = sum(OrdinaryVotes))   %>% 
  filter(TotalVotes == 0) %>% 
  left_join(polling_place_location, by = 'PollingPlaceID') 

# why so many polling places with no votes?
  
  # exclude polling  places with no votes
library(scales)
p <- election_results_df_loc_no_fac %>%
  group_by(DivisionNm.x) %>%
  summarise(
    TotalVotes = sum(OrdinaryVotes),
    ProportionLabor = round(sum(OrdinaryVotes[PartyNm == "Australian Labor Party"]) / TotalVotes, 3)) %>%
 filter(TotalVotes != 0) %>% 
  arrange(desc(ProportionLabor)) %>% 
  left_join(polling_place_location, by = c("DivisionNm.x" = "DivisionNm")) %>% 
  # str
  group_by(State) %>% 
  do(plots=ggplot(data = .) + 
         aes(x = ProportionLabor, y = reorder(DivisionNm.x, ProportionLabor), size = TotalVotes, label = State) +
         geom_point() +
        labs(title = .$State) + 
        xlab("Electorate") +
        scale_x_continuous("Proportion voting Labor Party", label = percent) +
        scale_size("Number of\nvotes cast", label = comma)  ) 
       

library(gridExtra)
n <- length(p$plots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(p$plots, ncol=nCol))

# have a look at 
# https://github.com/stevenmce/aec_analysis/blob/master/steve_RMarkdown_test1.Rmd
# http://jackman.stanford.edu/oz/Aggregate2010/aec/maps/report.pdf


 




