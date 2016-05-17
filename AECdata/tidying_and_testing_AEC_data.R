library(dplyr)

######################### data ingest ####################

# get all the HouseStateFirstPrefsByPollingPlace data
# assume we are in the project dir
dir <- paste0(getwd(), "/AECdata//HouseStateFirstPrefsByPollingPlace")
files <- list.files(dir,  full.names = TRUE )

# list of data frames with each state
library(purrr)
election_results_df <- files %>% map_df(read.csv, skip = 1, stringsAsFactors = FALSE)
# names(election_results_df)
# str(election_results_df)

# get lat long for polling places
polling_place_location <- read.csv(paste0(getwd(), "/AECdata/GeneralPollingPlacesDownload-17496.csv"), 
                                   skip = 1,
                                   stringsAsFactors =  FALSE)

# names(polling_place_location)
# head(polling_place_location)

# get two party preferred data
two_party_preferred_by_polling_place <- read.csv(paste0(getwd(), "/AECdata/HouseTppByPollingPlaceDownload-17496.csv"), 
                                                 skip = 1,
                                                 stringsAsFactors =  FALSE)

# add in different vote types
vote_types <- read.csv("AECdata/HouseFirstPrefsByCandidateByVoteTypeDownload-17496.csv", skip = 1)
#  how to join? vote_types is only for each candidate, not each polling place...

aec_first_pref <- read.csv(paste0(getwd(), "/AECdata/HouseFirstPrefsByPartyDownload-17496.csv"), skip = 1)
# names(aec_first_pref)

aec_winners <- read.csv(paste0(getwd(), "/AECdata/HouseMembersElectedDownload-17496.csv"), skip = 1)
names(aec_winners)

# get two candidate preferred data

aec_2candidates <- readr::read_csv(paste0(getwd(), "/AECdata/HouseTcpByCandidateByVoteTypeDownload-17496.csv"), skip = 1)
aec_2candidates_polling_place <- readr::read_csv(paste0(getwd(), "/AECdata/HouseTcpByCandidateByPollingPlaceDownload-17496.csv"), skip = 1)

######################### end data ingest ####################


####################### add locations to fp data##############

# quick look at locations 
library(ggplot2)
library(plotly)
p <- ggplot(polling_place_location, aes(Longitude, Latitude, label = PremisesNm)) +
  geom_point() +
  coord_equal() 
# p
# ggplotly(p)

# join polling place locations to election results
election_results_df_loc <- full_join(election_results_df, polling_place_location, by = "PollingPlaceID")
# head(election_results_df_loc)

# do we have any polling places with no location data?
election_results_df_loc %>% 
  select(DivisionID.x, PollingPlace, StateAb, GivenNm, Surname, PartyNm, Elected, Latitude, Longitude) %>% 
  filter(Elected == "Y") %>% 
  filter(is.na(Latitude)) 
# yes, 540 hospitals, etc.  

# do we have polling places with no election data?
election_results_df_loc %>% 
  select(DivisionID.y, PollingPlaceID, Elected, Latitude, Longitude) %>% 
  filter(is.na(Elected)) 
# yes 104 locations with no election data. 

# Let's exclude locations with no election data
election_results_df_loc <- election_results_df_loc %>%
  filter(!is.na(Elected)) 
# head(election_results_df_loc)

# plot 
p <- ggplot(election_results_df_loc, aes(Longitude, Latitude)) +
  geom_point() +
  coord_equal() 

# str(election_results_df_loc)
# get rid of factors
election_results_df_loc_no_fac <- election_results_df_loc %>% 
  map_if(is.factor, as.character) %>% 
  rbind_list
# str(election_results_df_loc_no_fac)

# how many polling places do we have with election data?
number_of_polling_places <- election_results_df_loc_no_fac %>% 
  group_by(PollingPlaceID) %>% 
  summarize(number_of_candidates_per_polling_place = n()) %>% 
  nrow

####################### add locations to 2pp data ##############

# join with first pref data
# need to aggregate to we have only one polling place per row
election_results_df_loc_pp <- election_results_df_loc %>% 
  group_by(PollingPlaceID) %>% 
  filter(Elected == "Y") %>% 
  arrange(desc(PollingPlaceID)) %>% 
  slice(1) 

# check we have all the polling places
identical(number_of_polling_places, nrow(election_results_df_loc_pp))

election_results_df_loc_pp <- left_join(election_results_df_loc_pp, two_party_preferred_by_polling_place, by = "PollingPlaceID")

# drop some duplicate cols
election_results_df_loc_pp <- election_results_df_loc_pp %>% 
  select(-contains(".y")) 

# rename the cols with .x so they don't have that
names(election_results_df_loc_pp) <- gsub(".x", "", names(election_results_df_loc_pp))
# drop duplicate cols, now that we've got rid of the .x in the names
election_results_df_loc_pp <- election_results_df_loc_pp[, !duplicated(colnames(election_results_df_loc_pp))]

# election_results_df_loc_pp %>% arrange(desc(PollingPlaceID)) %>% View
# head(xx)
# any duplicates?
election_results_df_loc_pp[duplicated(election_results_df_loc_pp),]
# no

######### end of working with th 2pp data ###########

####################### add locations to 2cp data ##############

# join with first pref data
# need to aggregate to we have only one polling place per row
election_results_df_loc_cp <- election_results_df_loc %>% 
  group_by(PollingPlaceID) %>% 
  filter(Elected == "Y") %>% 
  arrange(desc(PollingPlaceID)) %>% 
  slice(1) 

# check we have all the polling places
identical(number_of_polling_places, nrow(election_results_df_loc_cp))

election_results_df_loc_cp <- left_join(election_results_df_loc_cp, aec_2candidates_polling_place, by = "PollingPlaceID")

# drop some duplicate cols
election_results_df_loc_cp <- election_results_df_loc_cp %>% 
  select(-contains(".y")) 

# rename the cols with .x so they don't have that
names(election_results_df_loc_cp) <- gsub(".x", "", names(election_results_df_loc_cp))
# drop duplicate cols, now that we've got rid of the .x in the names
election_results_df_loc_cp <- election_results_df_loc_cp[, !duplicated(colnames(election_results_df_loc_cp))]

# election_results_df_loc_cp %>% arrange(desc(PollingPlaceID)) %>% View
# any duplicates?
election_results_df_loc_cp[duplicated(election_results_df_loc_cp),]
# no

######### end of working with the 2cp data ###########

### start of cleaning the fp data ##################

# seems like there's multiple names for the ALP...
# unique(election_results_df_loc_no_fac$PartyNm)
election_results_df_loc_no_fac$PartyNm <- with(election_results_df_loc_no_fac, ifelse(PartyNm == "Labor" |
                                                                                        PartyNm =="Australian Labor Party (Northern Territory) Branch" |
                                                                                        PartyNm ==  "Country Labor", 
                                                                                      "Australian Labor Party", 
                                                                                      PartyNm))
# similar for the greens
election_results_df_loc_no_fac$PartyNm <- with(election_results_df_loc_no_fac, ifelse(PartyNm == "Australian Greens" |
                                                                                        PartyNm == "The Greens (WA)", 
                                                                                      "The Greens",
                                                                                      PartyNm))
# similar for the Nationals
election_results_df_loc_no_fac$PartyNm <- with(election_results_df_loc_no_fac, ifelse(PartyNm == "The Nationals" |
                                                                                        PartyNm == "National Party", 
                                                                                      "The Nationals",
                                                                                      PartyNm))

# make some electorate names match the spatial data
election_results_df_loc_no_fac$DivisionNm.x <- with(election_results_df_loc_no_fac, ifelse(DivisionNm.x == "McMillan",
                                                                                           "Mcmillan",
                                                                                           DivisionNm.x))

election_results_df_loc_no_fac$DivisionNm.x <- with(election_results_df_loc_no_fac, ifelse(DivisionNm.x == "McPherson",
                                                                                           "Mcpherson",
                                                                                           DivisionNm.x))
# str(election_results_df_loc_no_fac)
# unique(election_results_df_loc_no_fac$PartyNm)


## how many electoral districts?
length(unique(election_results_df_loc_no_fac$DivisionID.x))

# remove a few redundant cols
election_results_df_loc_no_fac_no_dup <- 
  election_results_df_loc_no_fac %>% 
  select(-DivisionID.y, -DivisionID.y, -PollingPlaceNm)

# do we have any duplicate rows?
election_results_df_loc_no_fac_no_dup$uid <- 
  with(election_results_df_loc_no_fac_no_dup, paste0(DivisionID.x, PollingPlaceID, Surname, GivenNm, OrdinaryVotes, Swing))
# what are the duplicates?
election_results_df_loc_no_fac_no_dup <- 
  election_results_df_loc_no_fac_no_dup[!duplicated(election_results_df_loc_no_fac_no_dup$uid), ]

# drop some duplicate cols
election_results_df_loc_no_fac_no_dup <- election_results_df_loc_no_fac_no_dup %>% 
  select(-contains(".y")) 

# rename the cols with .x so they don't have that
names(election_results_df_loc_no_fac_no_dup) <- gsub(".x", "", names(election_results_df_loc_no_fac_no_dup))
# drop duplicate cols, now that we've got rid of the .x in the names
election_results_df_loc_no_fac_no_dup <- election_results_df_loc_no_fac_no_dup[, !duplicated(colnames(election_results_df_loc_no_fac_no_dup))]


# plot 
p <- ggplot(election_results_df_loc_no_fac_no_dup, aes(Longitude, Latitude)) +
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
# remvoe 147 with no lat longs
# ggplotly(p1)

###### end of cleaning the fp data #################

### aggregate fp and 2pp by electorate #############
electorate_variables <-  c("DivisionID", "DivisionNm", "CandidateID",        "Surname",           
                           "GivenNm",            "BallotPosition",     "Elected" ,          
                           "HistoricElected",    "PartyAb" ,           "PartyNm",       "State")
two_pp_variables <- c("Australian.Labor.Party.Votes", "Australian.Labor.Party.Percentage", 
                      "Liberal.National.Coalition.Votes", "Liberal.National.Coalition.Percentage", 
                      "TotalVotes")

# votes per canditate in each electorate
aec2013_fp_electorate <- election_results_df_loc_no_fac_no_dup %>% 
  group_by_(.dots = electorate_variables) %>% 
  summarise(Total_OrdinaryVotes_in_electorate = sum(OrdinaryVotes)) 

# check 
aec2013_fp_electorate %>% 
  group_by(PartyAb) %>% 
  summarise(Total_votes = sum(Total_OrdinaryVotes_in_electorate)) %>% 
  arrange(desc(Total_votes))
# yes, ok

# repeat for 2pp
aec2013_2pp_electorate <- election_results_df_loc_pp %>% 
  group_by_(.dots = electorate_variables) %>% 
  summarise(Total_Australian_Labor_Party_Votes_per_electorate = sum(Australian.Labor.Party.Votes),
            Average_Australian_Labor_Party_Percentage_in_electorate = mean(Australian.Labor.Party.Percentage),
            Total_Liberal_National_Coalition_Votes_per_electorate = sum(Liberal.National.Coalition.Votes),
            Average_Liberal_National_Coalition_Percentage_in_electorate = mean(Liberal.National.Coalition.Percentage),
            Total_2pp_votes_per_electorate = sum(TotalVotes))


###### write data to disk #################

# save as CSV
write.csv(election_results_df_loc_no_fac_no_dup, "AECdata/HouseFirstPrefsByPollingPlaceAllStates.csv")
write.csv(election_results_df_loc_pp,            "AECdata/HouseTwoPartyPrefdByPollingPlaceAllStates.csv")
write.csv(aec2013_fp_electorate,                "AECdata/HouseFirstPrefsByElectorateAllStates.csv")
write.csv(aec2013_2pp_electorate,               "AECdata/HouseTwoPartyPrefdByElectorateAllStates.csv")

# rename to something neater and more logical (nameing is hard!)
aec2013_fp <- election_results_df_loc_no_fac_no_dup
aec2013_fp_electorate <- aec2013_fp_electorate
aec2013_2pp <- election_results_df_loc_pp
aec2013_2pp_electorate <- aec2013_2pp_electorate
aec2013_2cp <- election_results_df_loc_cp


# Change variable names to match abs2011 where possible.
match_abs_2011_names <- function(x){
  x <- rename(x, ID=DivisionID)
  x <- rename(x, Electorate=DivisionNm)
  x$StateAb <- NULL
  x
}

# put data objects in a list to save typing
list_of_data_objects_for_renaming <- list(aec2013_fp = aec2013_fp, 
                             aec2013_fp_electorate = aec2013_fp_electorate, 
                             aec2013_2pp = aec2013_2pp, 
                             aec2013_2pp_electorate = aec2013_2pp_electorate)
# apply the function to change variable names
list_of_data_objects <- lapply(list_of_data_objects_for_renaming, function(i) match_abs_2011_names(i))
# remove objects from env to mimimize confusion
rm(list = names(list_of_data_objects))


# save to rds file (recommended for indiv. items)
for(i in seq_along(list_of_data_objects)){
  # can't save from a list, so make a temp object, with the right name
  temp <- list_of_data_objects[[i]]
  assign(names(list_of_data_objects)[i], temp)
  save(list = names(list_of_data_objects)[i], 
       file=paste0("echidnaR/data/", 
       names(list_of_data_objects)[i], ".rda"),
       compress =  "xz")
}

# load them in to this session
data_file_names <- list.files("echidnaR/data/", pattern = "aec2013", full.names = TRUE)
lapply(data_file_names, load, .GlobalEnv)


# check what we've got
list.files("echidnaR/data/")


########## end of data preparation and file-writing ###########
################################################################
##### Begin exploratory data analysis #########################


## Overall results for first preferences -----------------------

# by party by electorate
aec2013_fp %>% 
  select(Electorate, PartyNm, OrdinaryVotes) %>% 
  group_by(Electorate, PartyNm) %>% 
  summarise(total_votes = sum(OrdinaryVotes)) %>% 
  ungroup() %>%
  arrange(desc(total_votes))

# by party
aec2013_fp %>% 
  select(PartyNm, OrdinaryVotes) %>% 
  group_by(PartyNm) %>% 
  summarise(total_votes = sum(OrdinaryVotes)) %>% 
  ungroup() %>%
  arrange(desc(total_votes))

# compare to AEC data http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm
# First Preferences By Party

aec_first_pref %>% 
  select(PartyNm, OrdinaryVotes) %>% 
  arrange(desc(OrdinaryVotes)) %>% 
  head()

# agrees!

################################################################
# winner for each electorate ----------------------------------

aec2013_fp %>% 
  group_by(DivisionID.x) %>% 
  select(DivisionID.x, DivisionNm.x, StateAb, GivenNm, Surname, PartyNm, Elected) %>% 
  filter(Elected == "Y") %>% 
  slice(1) %>% 
  ungroup %>% 
  arrange(desc(Surname)) %>% 
  head

# check with AEC data for winner for each electorate

aec_winners %>% 
  select(DivisionID, StateAb, GivenNm, Surname, PartyNm) %>%  
  arrange(desc(Surname)) %>% 
  head

## yes, good match

################################################################
# Comparing party and candidate votes of several parties -------
proportions <- aec2013_fp %>%
  group_by(DivisionID.x) %>%
  summarise(Prop_Labour = sum(OrdinaryVotes[PartyNm == "Australian Labor Party"]) / sum(OrdinaryVotes),
            Prop_Liberal = sum(OrdinaryVotes[PartyNm == "Liberal"]) / sum(OrdinaryVotes),
            Prop_Greens = sum(OrdinaryVotes[PartyNm == "The Greens"]) / sum(OrdinaryVotes))
library(GGally)
ggpairs(proportions, columns = 2:ncol(proportions)) + theme_bw()

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
aec2013_fp %>%
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
aec2013_fp %>% 
  group_by(PollingPlaceID) %>%
  summarise(TotalVotes = sum(OrdinaryVotes))   %>% 
  filter(TotalVotes == 0) %>% 
  left_join(polling_place_location, by = 'PollingPlaceID') 

# why so many polling places with no votes?

# exclude polling  places with no votes
library(scales)
p <- aec2013_fp %>%
  group_by(Electorate) %>%
  summarise(
    TotalVotes = sum(OrdinaryVotes),
    ProportionLabor = round(sum(OrdinaryVotes[PartyNm == "Australian Labor Party"]) / TotalVotes, 3)) %>%
  filter(TotalVotes != 0) %>% 
  arrange(desc(ProportionLabor)) %>% 
  left_join(polling_place_location, by = c("Electorate" = "DivisionNm")) %>% 
  # str
  group_by(State) %>% 
  do(plots=ggplot(data = .) + 
       aes(x = ProportionLabor, y = reorder(Electorate, ProportionLabor), size = TotalVotes, label = State) +
       geom_point() +
       ylab("Electorate") +
       labs(title = .$State) + 
       scale_x_continuous("Proportion voting Labor Party", label = percent) +
       scale_size("Number of\nvotes cast", label = comma)  +
       theme_bw())


library(gridExtra)
n <- length(p$plots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(p$plots, ncol=nCol))

# have a look at 
# https://github.com/stevenmce/aec_analysis/blob/master/steve_RMarkdown_test1.Rmd
# http://jackman.stanford.edu/oz/Aggregate2010/aec/maps/report.pdf

# --------------------------------------------------------------------
# Interesting things to show for each electorate

# electorate level vote counts to percentages of the
# formal vote, computes the informal vote percentage, 
#  and the total number of votes cast at a particular
# electorate 

# parties of interest
parties_of_interest <- c("ALP", "GRN", "LP", "NP", "CLP", "LNQ")


country_level_formal_vote_counts_by_major_party <- 
  aec2013_fp %>% 
  mutate(formal = BallotPosition != 999) %>% 
  # proportion of votes for only the selected parties, ie. a relative comparison
  filter(PartyAb %in% parties_of_interest) %>% 
  group_by(PartyAb) %>% 
  summarize(total_formal = sum(OrdinaryVotes[formal], na.rm=TRUE),
            prop_informal  = sum(OrdinaryVotes[!formal]/(sum(OrdinaryVotes, na.rm=TRUE) * 100))) %>%
  mutate(prop_total_votes_in_the_country = total_formal / sum(total_formal))  
 
p <- ggplot(country_level_formal_vote_counts_by_major_party, 
            aes(PartyAb, prop_total_votes_in_the_country)) +
            geom_bar(stat="identity")

# check
sum(country_level_formal_vote_counts_by_major_party$prop_total_votes_in_the_country)


electorate_level_formal_vote_counts_by_major_party <- 
  # formal vote
  aec2013_fp %>% 
  mutate(formal = BallotPosition != 999) %>% 
  # proportion of votes for only the selected parties, ie. a relative comparison
  filter(PartyAb %in% parties_of_interest) %>%
  group_by(Electorate, PartyAb) %>% 
  summarize(total_formal = sum(OrdinaryVotes[formal], na.rm=TRUE),
            prop_informal  = sum(OrdinaryVotes[!formal]/(sum(OrdinaryVotes, na.rm=TRUE) * 100))) %>%
  # each electorate sums to not quite 100%, but pretty close
  mutate(prop_total_of_electorate = total_formal / sum(total_formal)) 

# check that proportions per electorate sum to 1
electorate_level_formal_vote_counts_by_major_party %>% 
  group_by(Electorate) %>% 
  summarize(total = sum(prop_total_of_electorate))

# get some electorates to test
some_electorates <- unique(electorate_level_formal_vote_counts_by_major_party$Electorate)[1:5]

electorate_level_formal_vote_counts_by_major_party %>% 
  filter(Electorate %in% some_electorates[1]) %>% 
  ggplot(aes(PartyAb, prop_total_of_electorate)) +
  geom_bar(stat = "identity") +
  xlab("Party") +
  ylab("Proportion of total formal \nvotes in the electorate") +
  theme_bw()

# show all electorates

p <- electorate_level_formal_vote_counts_by_major_party %>% 
     mutate(label = paste0(Electorate, " (", State, ")")) %>% 
  ggplot(aes(PartyAb, prop_total_of_electorate, label = label)) +
  geom_boxplot(colour = "grey80") +
  geom_jitter(alpha = 0.5, width = 0.5, size = 2) +
  xlab("Party") +
  ylab("Proportion of total formal \nvotes in the electorate") +
  theme_bw()

library(plotly)
ggplotly(p)

library(leaflet)

# make dataframe with electorate proportions
chosen_Electorate <- "Canberra"
elect_df <- aec2013_fp %>% 
  filter(Electorate == chosen_Electorate) %>% 
  group_by(PollingPlace, Longitude, Latitude) %>% 
  summarise(TotalVotes = sum(OrdinaryVotes),
            prop_labor = round(sum(OrdinaryVotes[PartyNm == "Australian Labor Party"]) / TotalVotes, 3))

# http://rstudio.github.io/leaflet/colors.html
# Create a continuous palette function
pal <- colorNumeric(
  palette = "Reds",
  domain = elect_df$prop_labor
)

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(data = elect_df, 
             lng= ~Longitude, 
             lat=~Latitude, 
             popup= ~PollingPlace, 
             color = ~pal(prop_labor),
             fillOpacity = 0.7,
             stroke = FALSE)




# todo
# - add two party preferred data, rename objects as aec2013_hr_fp_pp, aec2013_hr_fp_el, aes2013_hr_2pp_pp, aes2013_hr_2pp_el
# - more detail in readme 
# -- check links to Peter Ellis' work
# -- add links to sources
# -- animated gif of shiny app
# - shiny app theme_bw and some annotation
# - shiny app with leaflet map of electorates to show particular party proportion by polling place (cf. Jackman)
# - vignette: look at electorate segregation with polling place data
# - vignette: look at election + census data
# - submit to CRAN
# - Write R journal paper
# - add data for previous elections and census





