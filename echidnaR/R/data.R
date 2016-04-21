
#' 2011 Census data on all 150 electorates
#'
#' A dataset containing demographic and other information about each electorate.
#' The data were obtained from the Australian Bureau of Statistics, and downloaded 
#' from \url{https://www.censusdata.abs.gov.au/datapacks/}.
#'
#' The variables are as follows:
#'
#' @format A data frame with 150 rows with the following variables:
#' \itemize{
#'   \item ID: Commonwealth Electoral District identifier
#'   \item Name: Name of electorate
#'   \item State: State containing electorate
#'   \item Population: Total population of electorate
#'   \item Area: Area of electorate in square kilometres
#'   \item MedianIncome: Median income of people within electorate
#'   \item Unemployed: Percentage of people unemployed
#'   \item Bachelor: Percentage of people whose highest qualification is a Bachelor degree
#'   \item Postgraduate: Percentage of people whose highest qualification is a postgraduate degree
#'   \item NoReligion: Percentage of people with no religion.
#'   \item Age0_4: Percentage of people aged 0-4.
#'   \item Age5_14: Percentage of people aged 5-9.
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
#'   \item Indigenous: Percentage of people who are Indigenous
#'   \item EnglishOnly: Percentage of people who speak only English
#'   \item OtherLanguageHome: Percentage of people who speak a language other than English at home
#'   \item Married: Percentage of people who are married
#'   \item DeFacto: Percentage of people who are in a de facto marriage
#'   \item FamilyRatio: Total number of families to total number of people (times 100).
#' }
"abs2011"