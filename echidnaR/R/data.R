
#' 2011 Census data on all 150 electorates
#'
#' A dataset containing demographic and other information about each electorate.
#' The data were obtained from the Australian Bureau of Statistics, and downloaded 
#' from \url{https://www.censusdata.abs.gov.au/datapacks/}.
#'
#' @format A data frame with 150 rows with the following variables:
#' \itemize{
#'   \item ID: Commonwealth Electoral District identifier.
#'   \item Electorate: Name of electorate
#'   \item State: State containing electorate.
#'   \item Population: Total population of electorate.
#'   \item Area: Area of electorate in square kilometres.
#'   \item MedianIncome: Weekly median income of people within electorate (in $).
#'   \item Unemployed: Percentage of people unemployed.
#'   \item Bachelor: Percentage of people whose highest qualification is a Bachelor degree.
#'   \item Postgraduate: Percentage of people whose highest qualification is a postgraduate degree.
#'   \item Christianity: Percentage of people affiliated with the Christian religion (of all denominations).
#'   \item Catholic: Percentage of people affiliated with the Catholic denomimation.
#'   \item Buddhism: Percentage of people affiliated with the Buddhist religion.
#'   \item Islam: Percentage of people affiliated with the Islam religion.
#'   \item Judaism: Percentageof people affiliated with the Jewish religion. 
#'   \item NoReligion: Percentage of people with no religious affiliation.
#'   \item Age00_04: Percentage of people aged 0-4.
#'   \item Age05_14: Percentage of people aged 5-9.
#'   \item Age15_19: Percentage of people aged 15-19.
#'   \item Age20_24: Percentage of people aged 20-24.
#'   \item Age25_34: Percentage of people aged 25-34.
#'   \item Age35_44: Percentage of people aged 35-44.
#'   \item Age45_54: Percentage of people aged 45-54.
#'   \item Age55_64: Percentage of people aged 55-64.
#'   \item Age65_74: Percentage of people aged 65-74.
#'   \item Age75_84: Percentage of people aged 75-84.
#'   \item Age85plus: Percentage of people aged 85 or higher.
#'   \item BornOverseas: Percentage of people born outside Australia.
#'   \item Indigenous: Percentage of people who are Indigenous.
#'   \item EnglishOnly: Percentage of people who speak only English.
#'   \item OtherLanguageHome: Percentage of people who speak a language other than English at home.
#'   \item Married: Percentage of people who are married.
#'   \item DeFacto: Percentage of people who are in a de facto marriage.
#'   \item FamilyRatio: Total number of families to total number of people (times 100).
#'   \item Internet: Percentage of people with home internet.
#'   \item NotOwned: Percentage of dwellings not owned (either outright or with a mortgage).
#' }
"abs2011"    



#' 2013 General election data for first preference votes for candidates for the House of Representatives for each polling place
#' 
#' A dataset containing first preference vote counts, candidate names, polling place locations,
#' and other results for the
#' House of Representatives from the 2013 Australian federal election. 
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm} and 
#' \url{http://www.aec.gov.au/elections/federal_elections/2013/downloads.htm}.
#' 
#' @format A data frame with the following variables:
#' \itemize{
#'     \item ID: Electoral division ID    
#'     \item Electorate:  Electoral division name   
#'     \item PollingPlaceID: Polling place ID  
#'     \item PollingPlace: Polling place name     
#'     \item CandidateID: Candidate ID       
#'     \item Surname: Candidate surname          
#'     \item GivenNm: Candidate given name            
#'     \item BallotPosition: Candidate's position on the ballot    
#'     \item Elected: Whether the candidate was elected (Y/N)           
#'     \item HistoricElected:   
#'     \item PartyAb: Abbreviation for political party name           
#'     \item PartyNm: Political party name           
#'     \item OrdinaryVotes: Number of ordinates votes cast at the polling place for the candidate     
#'     \item Swing:             
#'     \item State: Abbreviation for state name             
#'     \item PollingPlaceTypeID:
#'     \item Premises Nm:        
#'     \item PremisesAddress1:  
#'     \item PremisesAddress2:  
#'     \item PremisesAddress3:  
#'     \item PremisesSuburb:    
#'     \item PremisesStateAb:   
#'     \item PremisesPostCode:  
#'     \item Latitude:          
#'     \item Longitude:
#'     \item uid:.
#'     }
"aec2013_fp"   



#' 2013 General election data for first preference votes for candidates for the House of Representative for each electorate
#' 
#' A dataset containing first preference vote counts, candidate names, and other results for the
#' House of Representatives from the 2013 Australian federal election. 
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm} and 
#' \url{http://www.aec.gov.au/elections/federal_elections/2013/downloads.htm}.
#' 
#' @format A data frame with the following variables:
#' \itemize{
#'     \item ID: Electoral division ID    
#'     \item Electorate:  Electoral division name   
#'     \item CandidateID: Candidate ID       
#'     \item Surname: Candidate surname          
#'     \item GivenNm: Candidate given name            
#'     \item BallotPosition: Candidate's position on the ballot    
#'     \item Elected: Whether the candidate was elected (Y/N)           
#'     \item HistoricElected:   
#'     \item PartyAb: Abbreviation for political party name           
#'     \item PartyNm: Political party name           
#'     \item OrdinaryVotes: Number of ordinates votes cast at the polling place for the candidate     
#'     \item State: Abbreviation for state name             
#'     }
"aec2013_fp_electorate"

#' 2013 General election data for two party preferred votes for the House of Representatives for each polling place
#' 
#' A dataset containing two party preferred vote counts, winning candidate names, polling place locations,
#' and other results for the House of Representatives from the 2013 Australian federal election. Includes the count of votes for
#' the Australian Labor Party and the count of votes for the Liberal-National Coalition for each polling place.
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm} and 
#' \url{http://www.aec.gov.au/elections/federal_elections/2013/downloads.htm}.
#' 
#' @format A data frame with the following variables:
#' \itemize{
#'     \item ID: Electoral division ID    
#'     \item Electorate:  Electoral division name   
#'     \item PollingPlaceID: Polling place ID  
#'     \item PollingPlace: Polling place name     
#'     \item CandidateID: Candidate ID       
#'     \item Surname: Candidate surname          
#'     \item GivenNm: Candidate given name            
#'     \item BallotPosition: Candidate's position on the ballot    
#'     \item Elected: Whether the candidate was elected (Y/N)           
#'     \item HistoricElected:   
#'     \item PartyAb: Abbreviation for political party name           
#'     \item PartyNm: Political party name           
#'     \item OrdinaryVotes: Number of ordinates votes cast at the polling place for the candidate     
#'     \item Swing:             
#'     \item State: Abbreviation for state name             
#'     \item PollingPlaceTypeID:
#'     \item Premises Nm:        
#'     \item PremisesAddress1:  
#'     \item PremisesAddress2:  
#'     \item PremisesAddress3:  
#'     \item PremisesSuburb:    
#'     \item PremisesStateAb:   
#'     \item PremisesPostCode:  
#'     \item Latitude:          
#'     \item Longitude:
#'     \item Australian.Labor.Party.Votes:        
#'     \item Australian.Labor.Party.Percentage:    
#'     \item Liberal.National.Coalition.Votes:    
#'     \item Liberal.National.Coalition.Percentage:
#'     \item TotalVotes:
#'     }
"aec2013_2pp"

#' 2013 General election data for two party preferred votes for candidates for the House of Representative for each electorate
#' 
#' A dataset containing two party preferred vote counts, winning candidate names, and other results for the House of Representatives from the 2013 Australian federal election. Includes the count of votes for
#' the Australian Labor Party and the count of votes for the Liberal-National Coalition for each polling place.
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm} and 
#' \url{http://www.aec.gov.au/elections/federal_elections/2013/downloads.htm}.
#' 
#' @format A data frame with the following variables:
#' \itemize{
#'     \item ID: Electoral division ID    
#'     \item Electorate:  Electoral division name   
#'     \item CandidateID: Candidate ID       
#'     \item Surname: Candidate surname          
#'     \item GivenNm: Candidate given name            
#'     \item BallotPosition: Candidate's position on the ballot    
#'     \item Elected: Whether the candidate was elected (Y/N)           
#'     \item HistoricElected:   
#'     \item PartyAb: Abbreviation for political party name           
#'     \item PartyNm: Political party name           
#'     \item OrdinaryVotes: Number of ordinates votes cast at the polling place for the candidate     
#'     \item State: Abbreviation for state name        
#'     \item Total_Australian_Labor_Party_Votes_per_electorate:     
#'     \item Average_Australian_Labor_Party_Percentage_in_electorate:    
#'     \item Total_Liberal_National_Coalition_Votes_per_electorate:      
#'     \item Average_Liberal_National_Coalition_Percentage_in_electorate:
#'     \item Total_2pp_votes_per_electorate:     
#'     }
"aec2013_2pp_electorate"

#' 2013 General election data for two candidate preferred votes for the House of Representatives for each polling place
#' 
#' A dataset containing two candidate preferred vote counts,  polling place locations,
#' and other results for the House of Representatives from the 2013 Australian federal election. Includes the count of votes for
#' the leading two candidates in the electorate after distribution of preferences for each polling place.
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm} and 
#' \url{http://www.aec.gov.au/elections/federal_elections/2013/downloads.htm}.
#' 
#' @format A data frame with the following variables:
#' \itemize{
#'     \item ID: Electoral division ID    
#'     \item Electorate:  Electoral division name   
#'     \item PollingPlace: Polling place name     
#'     \item PollingPlaceID: Polling place ID  
#'     \item CandidateID: Candidate ID       
#'     \item Surname: Candidate surname          
#'     \item GivenNm: Candidate given name            
#'     \item BallotPosition: Candidate's position on the ballot    
#'     \item Elected: Whether the candidate was elected (Y/N)           
#'     \item HistoricElected:   
#'     \item PartyAb: Abbreviation for political party name           
#'     \item PartyNm: Political party name           
#'     \item OrdinaryVotes: Number of ordinates votes cast at the polling place for the candidate     
#'     \item Swing: Percentage change since the last election
#'     }
"aec2013_2cp"

#' 2013 General election data for two candidate preferred votes for candidates for the House of Representative for each electorate
#' 
#' A dataset containing two candidate preferred vote counts, and other results for the House of Representatives from the 2013 Australian federal election. Includes the count of votes for
#' the leading two candidates in the electorate after distribution of preferences.
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm} and 
#' \url{http://www.aec.gov.au/elections/federal_elections/2013/downloads.htm}.
#' 
#' @format A data frame with the following variables:
#' \itemize{
#'     \item ID: Electoral division ID    
#'     \item Electorate:  Electoral division name   
#'     \item CandidateID: Candidate ID       
#'     \item Surname: Candidate surname          
#'     \item GivenNm: Candidate given name            
#'     \item BallotPosition: Candidate's position on the ballot    
#'     \item Elected: Whether the candidate was elected (Y/N)           
#'     \item HistoricElected:   
#'     \item PartyAb: Abbreviation for political party name           
#'     \item PartyNm: Political party name           
#'     \item OrdinaryVotes: Number of ordinates votes cast for the candidate     
#'     \item AbsentVotes: Number of absentee votes cast for the candidate
#'     \item ProvisionalVotes: Number of provisional votes cast for the candidate
#'     \item PrePollVotes: Number of pre-poll votes cast for the candidate
#'     \item PostalVotes: Number of postal votes cast for the candidate
#'     \item TotalVotes: Total votes cast for the candidate
#'     \item Swing: Percentage change since the last election
#'     }
"aec2013_2cp_electorate"


#' Map of Australian Electorate from 2013
#'
#' A dataset containing the map of the all 150 Australian electorates using the 2013 boundaries of the 
#' electorates (and downsampled to a 5\% file to allow fast plotting).
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm}.
#' @examples 
#' \dontrun{
#' data(nat_map)
#' # choropleth map with Census data
#' nat_map$region <- nat_map$ELECT_DIV
#' data(abs2011)
#' abs2011$region <- abs2011$Name
#' library(ggplot2)
#' library(ggthemes)
#' both <- intersect(unique(abs2011$region), unique(nat_map$region))
#' ggplot(aes(map_id=region), data=subset(abs2011, region %in% both)) +
#'   geom_map(aes(fill=MedianIncome), map=subset(nat_map, region %in% both)) +
#'   expand_limits(x=nat_map$long, y=nat_map$lat) + 
#'   theme_map()
#' }
"nat_map"

#' (Very small) Map of Australian Electorate from 2013
#'
#' A dataset containing the map of the all 150 Australian electorates using the 2013 boundaries of the 
#' electorates (and downsampled to a 0.5\% file to allow fast plotting).
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm}.
# "nat_map_small"

#' Data of the Australian Electorate from 2013
#'
#' A dataset containing some demographic information for each of the 150 Australian electorates.
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{http://www.aec.gov.au/Electorates/gis/gis_datadownload.htm}.
#' The data is published 
#' @format A data frame with 150 rows with the following variables:
#' \itemize{
#'     \item id: numeric identifier that links the electorate with the corresponding polygon in `nat_map`.
#'     \item ELECT_DIV: Electorate division name   
#'     \item STATE: abbreviation of the state name
#'     \item NUMCCDS: AEC variable that might be filled with meaning or a description down the road
#'     \item AREA_SQKM: combined square kilometers of each electorate
#' }
#' @examples 
#' \dontrun{
#' data(nat_data)
#' library(ggplot2)
#' ggplot(aes(map_id=region), data=subset(abs2011, region %in% both)) +
#'   geom_map(aes(fill=MedianIncome), map=subset(nat_map, region %in% both)) +
#'   expand_limits(x=nat_map$long, y=nat_map$lat) + 
#'   theme_map()
#'   }
"nat_data"



#' This is the same as nat_data, but with a few more cols. 
#' We can probably replace nat_dat with this one
#'
#'
"nat_data_cart"


#' Electorate hexagon data in a tidy form
#' @seealso Thomas Lumley
#' @references Thomas Lumley
#' @format A data frame
#' @examples 
#' data(hexDat)
#' library(plotly)
#' p <- ggplot(hexDat, aes(xcent, ycent, text = Electorate)) + 
#'   geom_hex(stat = "identity") + 
#'   lims(x=c(-80, 8), y=c(-40, 50))
#' ggplotly(p, tooltip = "text")
"hexDat"
