library(readr)
library(readxl)
library(dplyr)
library(stringr)

# All ABS data are downloaded from
# https://www.censusdata.abs.gov.au/datapacks/
# You need to register (free), log in, and 
# select "Basic Community Profile" and "Short header"
# Then download "Commonwealth Electoral Divisions" for "Aust".
# to get 2011_BCP_CED_for_AUST_short-header.zip
# Unzip to form three folders:
# * 2011_BCP_CED_for_AUST_short-header
# * Metadata
# * About DataPacks
# The main csv files are in 2011_BCP_CED_for_AUST_short-header
# Some xlsx files in Metadata are also useful.
# The files in "About DataPacks" can be ignored.
# Copy all csv files and Metadata xlsx files to the ABSdata folder.

# Grab area data:
areas <- read_excel("ABSdata/2011Census_geog_desc_1st_2nd_3rd_release.xlsx",
                    skip=1)
areas <- filter(areas, AUS=="CED")[,-1]
colnames(areas) <- c("region_id", "Electorate", "Area")

# Get states and electorate names
tmp <- str_split(areas[["Electorate"]],", ")
areas$Name <- unlist(lapply(tmp, function(x)x[1]))
areas$State <- unlist(lapply(tmp, function(x)x[2]))
areas$Electorate <- NULL
abs2011all <- areas
rm(tmp,areas)

# Read CSV files
bfiles <- list.files("ABSdata")
bfiles <- grep("2011Census_B", bfiles, value=TRUE)

# Merge all csv files into single data frame
for(i in 1:length(bfiles))
{
  bnew <- read_csv(paste("ABSdata/",bfiles[i],sep=""))
  # Find unique columns
  newcols <- setdiff(names(bnew),names(abs2011all))
  abs2011all <- merge(abs2011all, bnew[,c("region_id",newcols)], 
                   by="region_id")
}  
rm(bfiles,newcols,bnew,i)

# Filter "electorates" corresponding to shipping and "no usual address"
abs2011all <- abs2011all[-grep("Shipping", abs2011all$Name),]
abs2011all <- abs2011all[-grep("No Usual Address", abs2011all$Name),]

# Create a new data frame with a subset of the variables
abs2011 <- data.frame(ID = substr(abs2011all$region_id,4,6))
abs2011$Name <- abs2011all$Name
abs2011$Name[abs2011$Name=="Mcewen"] <- "McEwen"
abs2011$State <- abs2011all$State
abs2011$Population <- abs2011all$Tot_P_P
abs2011$Area <- abs2011all$`Area sqkm`
abs2011$MedianIncome <- abs2011all$Median_Tot_prsnl_inc_weekly
abs2011$Unemployed <- abs2011all$Percent_Unem_loyment_P
abs2011$Bachelor <- abs2011all$Non_sch_quals_Bchelr_Degree_P / abs2011all$Tot_P_P * 100
abs2011$Postgraduate <-  abs2011all$Non_sch_quals_PostGrad_Dgre_P / abs2011all$Tot_P_P * 100
abs2011$Christianity <- abs2011all$Christianity_Tot_P / abs2011all$Tot_P_P * 100
abs2011$Catholic <- abs2011all$Christianity_Catholic_P / abs2011all$Tot_P_P * 100
abs2011$Buddhism <- abs2011all$Buddhism_P / abs2011all$Tot_P_P * 100
abs2011$Islam <- abs2011all$Islam_P / abs2011all$Tot_P_P * 100
abs2011$Judaism <- abs2011all$Judaism_P / abs2011all$Tot_P_P * 100
abs2011$NoReligion <- abs2011all$No_Religion_P / abs2011all$Tot_P_P * 100
abs2011$Age0_4 <- abs2011all$Age_0_4_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age5_14 <- abs2011all$Age_5_14_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age15_19 <- abs2011all$Age_15_19_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age20_24 <- abs2011all$Age_20_24_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age25_34 <- abs2011all$Age_25_34_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age35_44 <- abs2011all$Age_35_44_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age45_54 <- abs2011all$Age_45_54_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age55_64 <- abs2011all$Age_55_64_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age65_74 <- abs2011all$Age_65_74_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age75_84 <- abs2011all$Age_75_84_yr_P / abs2011all$Tot_P_P * 100
abs2011$Age85plus <- abs2011all$Age_85ov_P / abs2011all$Tot_P_P * 100
abs2011$BornOverseas <- abs2011all$Born_elsewhere_P / abs2011all$Tot_P_P * 100
abs2011$Indigenous <- abs2011all$Indigenous_P_Tot_P / abs2011all$Tot_P_P * 100
abs2011$EnglishOnly <- abs2011all$P_EO_Tot / abs2011all$Tot_P_P * 100
abs2011$OtherLanguageHome <- abs2011all$Lang_spoken_home_Oth_Lang_P / abs2011all$Tot_P_P * 100
abs2011$Married <- abs2011all$P_H_or_W_in_RM_Tot / abs2011all$Tot_P_P * 100
abs2011$DeFacto <- abs2011all$P_Ptn_in_DFM_Tot / abs2011all$Tot_P_P * 100
abs2011$FamilyRatio <- abs2011all$Total_F / abs2011all$Tot_P_P * 100
abs2011$Internet <- 100 - abs2011all$No_IC_Total / abs2011all$Tot_P_P * 100
abs2011$NotOwned <- (abs2011all$Total_Total - 
                    abs2011all$O_OR_Total - 
                    abs2011all$O_MTG_Total)/abs2011all$Total_Total * 100


save(abs2011, file="echidnaR/data/abs2011.rda")
