library(readr)
library(magrittr)
library(dplyr)
library(stringr)

# Read CSV files
abs2011all <- read_csv("ABSdata/2011Census_areas.csv")
bfiles <- list.files("ABSdata")
bfiles <- grep("2011Census_B", bfiles, value=TRUE)

# Get states and electorate names
tmp <- str_split(abs2011all[["Label"]],", ")
abs2011all$Name <- unlist(lapply(tmp, function(x)x[1]))
abs2011all$State <- unlist(lapply(tmp, function(x)x[2]))
abs2011all$Label <- NULL
rm(tmp)

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

# Create a new data frame with a subset of the variables
abs2011 <- data.frame(ID = substr(abs2011all$region_id,4,6))
abs2011$State <- abs2011all$State
abs2011$Area <- abs2011all$`Area sqkm`
abs2011$MedianIncome <- abs2011all$Median_Tot_prsnl_inc_weekly
abs2011$Unemployed <- abs2011all$Percent_Unem_loyment_P
abs2011$Bachelor <- abs2011all$Non_sch_quals_Bchelr_Degree_P /
                    abs2011all$Tot_P_P * 100

