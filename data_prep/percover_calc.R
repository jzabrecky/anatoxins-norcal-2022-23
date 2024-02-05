#### percent cover average calculations
### Jordan Zabrecky
## 01.03.2024

# This code calculates averages using % cover data for each study reach
# and additionally each river section (designated by nearby USGS gage)
# It also tallies the number and percent of transects 
# where Anabaena or Microcoleus was present

#### Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "lubridate"), require, character.only = T)

# loading raw % cover data
percover22_raw <- read.csv("data_prep/raw/percover_2022_raw.csv")
percover23_raw <- read.csv("data_prep/raw/percover_2023_raw.csv")

#### Checking data ####

## Final confirmation of data entry by checking that all observations at each transect sum to 100%!

# making new dataframes with 'total' column (that should be 100%!)
percover22_test <- percover22_raw %>% 
  mutate(total = Green_Algae + Microcoleus + Anabaena + Bare_Biofilm + Other_N.fixers)

percover23_test <- percover23_raw %>% 
  mutate(total = Green_Algae + Microcoleus + Anabaena + Bare_Biofilm + Other_N.fixers)

# iterating through each observation to check if there is one that != 100%
for(i in 1:length(percover22_test)) {
  if(percover22_test$total[i] != 100) {
    print(paste(i, " has an issue!")) 
  }
  
}
for(i in 1:length(percover23_test)) {
  if(percover23_test$total[i] != 100) {
    print(paste(i, " has an issue!")) 
  }
}
# no errors printed-- we are all good :)

#### Calculating % cover averages for each reach on each sampling day #### 

## Creating new dataframe for our manipulations and changing the Date from "character" to "Date"

percover22 <- percover22_raw
percover22$Date <- mdy(percover22_raw$Date)

percover23 <- percover23_raw
percover23$Date <- mdy(percover23_raw$Date)

## Function to calculate % cover averages for each reach on each sampling day

average_per_reach <- function(data) { # data is the full % cover from sampling year
  data %>% 
    group_by(Site, Date, Reach) %>% 
    mutate(
      green_algae = mean(Green_Algae),
      microcoleus = mean(Microcoleus),
      anabaena = mean(Anabaena),
      bare_biofilm = mean(Bare_Biofilm),
      other_nfixers = mean(Other_N.fixers),
      micro_transects = sum(Micro_pres),
      ana_transects = sum(Ana_pres),
      total_transects = length(Transect),
      percent_micro_transects = micro_transects / total_transects,
      percent_ana_transects = ana_transects / total_transects
    ) %>% 
    ungroup() %>% 
    select(Date, Site_Reach, Site, Reach, green_algae, microcoleus,
           anabaena, bare_biofilm, other_nfixers, micro_transects, ana_transects, 
           total_transects, percent_micro_transects, percent_ana_transects) %>% 
    distinct() %>% 
    na.omit()
}

## Applying function to 2022 and 2023 data

percover22_reach <- average_per_reach(percover22)
percover23_reach <- average_per_reach(percover23)

# saving new csvs
setwd("data_prep/working") # saving into the "working" datasets
write.csv(percover22_reach, "percover_2022_byreach.csv", row.names = FALSE)
write.csv(percover23_reach, "percover_2023_byreach.csv", row.names = FALSE)

#### Calculating % cover averages for each USGS gage section on each sampling day #### 
## Only one USGS gage for Russian and Salmon, but 2023 Sfk Eel is divided as Miranda and Standish Hickey

average_per_gage <- function(data) {
  data %>% mutate(
    Gage = case_when(Site == "RUS" ~ "Russian",
                     Site == "SAL" ~ "Salmon",
                     Reach == "STH" ~ "Standish Hickey",
                     Site == "EEL" ~ "Miranda")
  ) %>% group_by(Gage, Date) %>% 
    mutate(
      green_algae = mean(Green_Algae),
      microcoleus = mean(Microcoleus),
      anabaena = mean(Anabaena),
      bare_biofilm = mean(Bare_Biofilm),
      other_nfixers = mean(Other_N.fixers),
      micro_transects = sum(Micro_pres),
      ana_transects = sum(Ana_pres),
      total_transects = length(Transect),
      percent_micro_transects = micro_transects / total_transects,
      percent_ana_transects = ana_transects / total_transects
    ) %>% 
    ungroup() %>% 
    select(Date, Gage, green_algae, microcoleus,
           anabaena, bare_biofilm, other_nfixers, micro_transects, ana_transects, 
           total_transects, percent_micro_transects, percent_ana_transects) %>% 
    distinct() %>% 
    na.omit()
}

# Want to manually merge 7.6.2022 RUS-4S & RUS-3UP and 7.7.2022 RUS-2UP to be on the same day 
# as they are only a day apart and were ideally done on the same day
# things do not always go according to plan!
percover22$Date[144:154] <- mdy("07/06/2022")

## Applying function to 2022 and 2023 data

percover22_gage <- average_per_gage(percover22)
percover23_gage <- average_per_gage(percover23)

# saving new csvs
write.csv(percover22_gage, "percover_2022_bygage.csv", row.names = FALSE)
write.csv(percover23_gage, "percover_2023_bygage.csv", row.names = FALSE)
