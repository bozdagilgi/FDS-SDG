#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven )

#Import "main" and "roster" data file. Here we are using FDS Pakistan dataset. 
library(haven)
main <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/Survey Programme Team/Countries/Pakistan/Data/4 Analysis/FDS_PAK_2024_Allrespondents_complete.dta")

hhroster <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/Survey Programme Team/Countries/Pakistan/Data/4 Analysis/FDS_PAK_2024_Roster_complete.dta")

# Calculate the age of the child which was asked the questions on immunization
main <- main %>%
  mutate(
    childnametouseAGE = as.numeric(childnametouseAGE),  # Convert to numeric
    child_age_immunization = case_when(
      childnametouseAGE != 99 ~ childnametouseAGE,  # Use childnametouseAGE if it’s not 99
      childnametouseAGE == 99 & AN1_5 == 2 ~ age_child_5,
      childnametouseAGE == 99 & AN1_4 == 2 ~ age_child_4,
      childnametouseAGE == 99 & AN1_3 == 2 ~ age_child_3,
      childnametouseAGE == 99 & AN1_2 == 2 ~ age_child_2,
      childnametouseAGE == 99 & AN1 == 2 ~ age_child_1,
      TRUE ~ NA_real_  # Set to NA (blank) if none of the conditions are met
    )
  )
#Rename _uuid to uuid as it keeps giving errors
main <- main %>%
  rename(uuid = `_uuid`)

hhroster <- hhroster %>%
  rename(uuid = `_uuid`)

# Pull the current education of the randomly selected adult into the main dataset (from roster)
main <- main %>%
  left_join(
    hhroster %>% 
      select(uuid, rosterposition, HH_Educ02a),
    by = c("uuid" = "uuid", "selected_adultap" = "rosterposition")
  )
# Rename the variable to match the name which is automatically be created by Kobo (it is alreadt there, Magrith will add it to the dataset)
main <- main %>%
  rename(educ_now = `HH_Educ02a`)