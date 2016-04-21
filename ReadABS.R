library(readr)
library(magrittr)
library(dplyr)
library(stringr)

# Read CSV files
areas <- read_csv("ABSdata/2011Census_areas.csv")
b01 <- read_csv("ABSdata/2011Census_B01_AUST_CED_short.csv")
b02 <- read_csv("ABSdata/2011Census_B02_AUST_CED_short.csv")

# Get states and electorate names
tmp <- str_split(areas[["Label"]],", ")
areas$Name <- unlist(lapply(tmp, function(x)x[1]))
areas$State <- unlist(lapply(tmp, function(x)x[2]))
areas$Label <- NULL
rm(tmp)

# Merge into single data frame
abs2011 <- merge(areas,b01) %>% merge(b02)

