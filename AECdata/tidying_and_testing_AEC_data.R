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
ggplotly(p)

# join polling place locations to election results
election_results_df_loc <- full_join(election_results_df, polling_place_location, by = "PollingPlaceID")
head(election_results_df_loc)

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
ggplot(election_results_df_loc, aes(Longitude, Latitude, label = PremisesNm)) +
  geom_point() +
  coord_equal() 



str(election_results_df_loc)
# get rid of factors
election_results_df_loc_no_fac <- election_results_df_loc %>% 
  map_if(is.factor, as.character) %>% 
  rbind_list
str(election_results_df_loc_no_fac)


# seems like there's two names for the ALP
unique(election_results_df_loc_no_fac$PartyNm)
election_results_df_loc_no_fac$PartyNm <- with(election_results_df_loc, ifelse(PartyNm == "Labor", 
                                     "Australian Labor Party",
                                     PartyNm))
str(election_results_df_loc_no_fac)
unique(election_results_df_loc_no_fac$PartyNm)

# save as CSV
write.csv(election_results_df_loc_no_fac, "AECdata/HouseFirstPrefsByPollingPlaceAllStates.csv")

## Overall results for first preferences ----------------------------

election_results_df_loc %>% 
  select(PartyNm, OrdinaryVotes) %>% 
  group_by(PartyNm) %>% 
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

# winner for each electorate
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

