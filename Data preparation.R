########################################################################
#################ADD DISAGGREGATION VARIABLES###########################
########################################################################

rm(list = ls())


#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven, Microsoft365R )

#Import 5 datasets. Here we are using FDS Pakistan dataset last updated on 21/02/2025.  


HHroster <- read_dta("FDS_PAK_2024_Roster_complete.dta")
main <- read_dta("FDS_PAK_2024_HoH_complete.dta")
RA_adult <- read_dta("FDS_PAK_2024_Random_Member_complete.dta")
RA_woman <- read_dta("FDS_PAK_2024_Random_Woman_complete.dta")
RA_caregiver <- read_dta ("FDS_PAK_2024_Random_Child_complete.dta")



# Rename _uuid to uuid in all datasets
main <- main %>%
  rename(uuid = `_uuid`)

HHroster <- HHroster %>%
  rename(uuid = `_uuid`)

RA_adult <- RA_adult %>%
  rename(uuid = `_uuid`)

RA_woman <- RA_woman %>%
  rename(uuid = `_uuid`)

RA_caregiver <- RA_caregiver %>%
  rename(uuid = `_uuid`)

###Age of the child - Immunization 
# Calculate the age of the child which was asked the questions on immunization
RA_caregiver <- RA_caregiver %>%
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
###Age of the child - Child Labour

# Pull the age of the child about whom he child labour questions are asked 
# Perform a left join and extract the child's age
main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, agetouse), # Select relevant columns from hhroster
    by = c("uuid" = "uuid", "selected_childla" = "rosterposition") # Match on uuid and positions
  ) %>%
  rename(child_labor_age = agetouse) # Rename agetouse to child_labor_age


### Household members size
# Create a new variable to count the number of household members in each household 
##Here we are using the variable "member" which assigned a 1 for each person that passes the check for actually being a household member. This check done automatically in the data collection form. 
household_counts <- HHroster %>%
  group_by(uuid) %>%
  summarise(HHmembersize = sum(member == 1, na.rm = TRUE))
##Join with the main data set
main <- main %>%
  left_join(household_counts, by = "uuid")


#Disaggregation variables 
## Roster ----
## 1. Population Groups ----
##Population group variable is available in the main dataset. Here we pull it to the HHroster dataset. 
HHroster <- HHroster %>%
  left_join(
    main %>% select(uuid, Intro_07), # Select relevant columns from main
    by = c("uuid" = "uuid") # Match on uuid 
  ) 


## 2. Age ----

# Convert agetouse from character to numeric
HHroster$agetouse <- as.numeric(HHroster$agetouse)

### Create two new variables for different age categories
### 4 categories (0-4, 5-17, 18-59, 60+)

HHroster$HH_04_cat4 <- cut(HHroster$agetouse, breaks = c(-1, 4, 17, 59, Inf), 
                           labels = c("0-4", "5-17", "18-59", "60+"))

### 2 categories (<18, >18)

HHroster$HH_04_cat2 <- cut(HHroster$agetouse, breaks = c(-1, 17, Inf), 
                           labels = c("0-17", "18-60+"))

table(HHroster$HH_04_cat4)
table(HHroster$HH_04_cat2)

## 3. Disability ----

### Washington Group Index
#Disability status identifiers are calculated based on guidelines by the [Washington Group on Disability Statistics](https://www.washingtongroup-disability.com/fileadmin/uploads/wg/WG_Document__6C_-_Analytic_Guidelines_for_the_WG-ES__Stata_.pdf) for over 5-year-olds. Note that in FDS we have three levels of disability level (some difficulty, a lot of difficulty, cannot do at all, + don't know, refuse to answer).

#Disability is specified as at least one domain/question coded A LOT OF DIFFICULTY or CANNOT DO AT ALL. 

###Step 1: Add the level 0 - no difficulty, for those members above 5 years of age who were not identified as having disabilities. 
#Vision
HHroster <- HHroster %>% 
  mutate(Dis_03 = if_else(is.na(Dis_03) & agetouse > 5, 0, Dis_03))
#Hearing
HHroster <- HHroster %>% 
  mutate(Dis_06 = if_else(is.na(Dis_06) & agetouse > 5, 0, Dis_06))
#Mobility
HHroster <- HHroster %>% 
  mutate(Dis_09 = if_else(is.na(Dis_09) & agetouse > 5, 0, Dis_09))
#Cognition
HHroster <- HHroster %>% 
  mutate(Dis_12 = if_else(is.na(Dis_12) & agetouse > 5, 0, Dis_12))
#Self-care
HHroster <- HHroster %>% 
  mutate(Dis_15 = if_else(is.na(Dis_15) & agetouse > 5, 0, Dis_15))
#Communication
HHroster <- HHroster %>% 
  mutate(Dis_18 = if_else(is.na(Dis_18) & agetouse > 5, 0, Dis_18))

###Step 2: Generate frequency distributions on each of the WG-SS domain variables

### 0 No difficulty 
### 1	Some difficulty
### 2	A lot of difficulties
### 3	Cannot do at all
### 98	Don’t know
### 99	Refused to Answer

#Vision 
barplot(table(HHroster$Dis_03), main = "Vision")
#Hearing
barplot(table(HHroster$Dis_06), main = "Hearing")
#Mobility
barplot(table(HHroster$Dis_09), main = "Mobility")
#Cognition
barplot(table(HHroster$Dis_12), main = "Cognition")
#Self-care
barplot(table(HHroster$Dis_15), main = "Self-care")
#Communication
barplot(table(HHroster$Dis_18), main = "Communicating")


#Step 3: Construct disability variable 
#Initialize disability variable with 2
HHroster$disability <- 0

# Replace disability with NA if all specified columns are missing and age is > 5
HHroster$disability <- ifelse(
  HHroster$agetouse > 5 &
    is.na(HHroster$Dis_03) & is.na(HHroster$Dis_06) & is.na(HHroster$Dis_09) &
    is.na(HHroster$Dis_12) & is.na(HHroster$Dis_15) & is.na(HHroster$Dis_18),
  NA, 
  HHroster$disability
)

# Replace disability with 1 if any of the specified conditions are met and age is > 5
HHroster$disability <- ifelse(
  HHroster$agetouse > 5 & (
    HHroster$Dis_03 %in% c(2, 3) | HHroster$Dis_06 %in% c(2, 3) | 
      HHroster$Dis_09 %in% c(2, 3) | HHroster$Dis_12 %in% c(2, 3) | 
      HHroster$Dis_15 %in% c(2, 3) | HHroster$Dis_18 %in% c(2, 3)
  ),
  1,
  HHroster$disability
)

# Tabulate the disability variable
table(HHroster$disability, useNA = "ifany")

## 4. Country of Origin ----
#The individual information on the country of origin comes from the HHroster. To have one single variable for country of origin information, the country code for the country of enumeration (i.e. PAK for Pakistan) will be entered as below. This question is asked only to individuals older than 15. For individuals younger than 15, the value is equaled to the country of origin of the household (as responded by the Head of the Household)

HHroster <- HHroster %>%
  left_join(
    main %>% select(uuid, origincntry), # Select relevant columns from main
    by = c("uuid" = "uuid") # Match on uuid 
  ) 
HHroster <- HHroster %>%
  rename(origincountry_roster = origincntry) #Rename to origincountry_roster

HHroster <- HHroster %>%
  mutate( # country of origin from ID_00 and ID_00_specify
    COO = case_when(
      ID_00 == 1 ~ "PAK", ##ensure to adjust here the country code (where FDS took place)
      ID_00 == 2 ~ as.character(HHroster$ID_00_specify),
      ID_00 == 3 ~ "Stateless",
      ID_00 == 99 ~ "99",
      ID_00 == 98 ~ "98", 
      is.na(ID_00) ~ as.character(HHroster$origincountry_roster), #For individuals under 15 years, ID_00 is not asked and therefore NA. For these individuals, we take "origincntry" which is a the country of origin of the household (answered by the main)
    )
  ) 


## Head of the Household (main dataset) ----
#1. Age ----
# Age with no categories 
main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, agetouse), # Select relevant columns from hhroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position
  ) %>%
  rename(HH_04_HoH = agetouse) # Rename agetouse to HH_04_hOh


# 2 categories 
main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, HH_04_cat2), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
main <- main %>%
  rename(HH_04_HoH_cat2 = HH_04_cat2) #HH_04_HoH_cat2

# 4 categories 
main <- main %>%
  left_join(
    HHroster %>% select(uuid,rosterposition, HH_04_cat4), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid  and position 
  ) 
main <- main %>%
  rename(HH_04_HoH_cat4 = HH_04_cat4) #HH_04_HoH_cat4

#2. Gender ----

main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, HH_02), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
main <- main %>%
  rename(HH_02_HoH = HH_02) #Rename to HH_02_HoH

#3. Disability ----

main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, disability), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
main <- main %>%
  rename(disability_HoH = disability) #Rename to disability_HoH

#4. Country of Origin ----

main <- main %>%
  left_join(
    HHroster %>% select(uuid,rosterposition, COO), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
main <- main %>%
  rename(COO_HoH = COO) #Rename to COO_HoH


## Randomly Selected Adult (RA_adult dataset) ----

#1. Age ----
# 2 categories 
RA_adult$age_selected <- as.numeric(as.character(RA_adult$age_selected))

RA_adult$RA_HH_04_cat2 <- cut(RA_adult$age_selected, breaks = c(-1, 17, Inf), 
                              labels = c("0-17", "18-60+"))

# 4 categories 
RA_adult$age_selected <- as.numeric(as.character(RA_adult$age_selected))

RA_adult$RA_HH_04_cat4 <- cut(RA_adult$age_selected, breaks = c(-1, 4, 17, 59, Inf), 
                              labels = c("0-4", "5-17", "18-59", "60+"))

#2. Population Group ----

RA_adult <- RA_adult %>%
  left_join(
    main %>% select(uuid, Intro_07), # Select relevant columns from main dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 

#3. Country of origin ----

RA_adult <- RA_adult %>%
  left_join(
    HHroster %>% select(uuid, COO, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_adult <- RA_adult %>%
  rename(COO_RA = COO) #Rename to COO_RA

#4. Disability ----
RA_adult <- RA_adult %>%
  left_join(
    HHroster %>% select(uuid, disability, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition") # Match on uuid and HHroster position
  ) 
RA_adult <- RA_adult %>%
  rename(disability_RA = disability) #Rename to disability_RA

#5. Gender ----

RA_adult <- RA_adult %>%
  rename(HH_02_RA = `HH_02_selected`)

## Randomly Selected Women (RA_woman dataset) ----

#1. Age ----

RA_woman$agerandomwoman <- as.numeric(as.character(RA_woman$agerandomwoman))

RA_woman$RW_HH_04_cat2 <- cut(RA_woman$agerandomwoman, breaks = c(-1, 17, Inf), 
                              labels = c("0-17", "18-60+"))

# 4 categories 
RA_woman$RW_HH_04_cat4 <- cut(RA_woman$agerandomwoman, breaks = c(-1, 4, 17, 59, Inf), 
                              labels = c("0-4", "5-17", "18-59", "60+"))

#2. Population Group  ----

RA_woman <- RA_woman %>%
  left_join(
    main %>% select(uuid, Intro_07), # Select relevant columns from main dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 


#3. Country of origin ----

RA_woman <- RA_woman %>%
  left_join(
    HHroster %>% select(uuid, COO, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_woman <- RA_woman %>%
  rename(COO_RW = COO) #Rename to COO_RW

#4. Disability ----
RA_woman <- RA_woman %>%
  left_join(
    HHroster %>% select(uuid, disability, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_woman <- RA_woman %>%
  rename(disability_RW = disability) #Rename to disability_RW

## Randomly Selected caregiver (RA_caregiver dataset) ----

#1. Population Group ----

table(main$Intro_07)

RA_caregiver <- RA_caregiver %>%
  left_join(
    main %>% select(uuid, Intro_07), # Select relevant columns from main dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 

#2. Disability ----
RA_caregiver <- RA_caregiver %>%
  left_join(
    HHroster %>% select(uuid, disability, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition_caregiver" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(disability_RC = disability) #Rename to disability_RC

#3. Country of Origin ----

RA_caregiver <- RA_caregiver %>%
  left_join(
    HHroster %>% select(uuid, COO, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition_caregiver" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(COO_RC = COO) #Rename to COO_RC

#4. Gender  ----
RA_caregiver <- RA_caregiver %>%
  rename(HH_02_RC = `finalcaregiverSEX`)

#5. Age ----
RA_caregiver <- RA_caregiver %>%
  rename(HH_04_RC = `finalcaregiverAGE`)


#Crowding index

####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep

table(main$HH14) ##How many separate structures or buildings do the members of your household occupy? 
table(main$HHmembersize)

main <- main %>%
  mutate(crowding=HHmembersize/HH14
  ) %>%
  mutate(crowding_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 2)
  )

table(main$crowding_cat)



#### ADD LABELS ----

# Define labels for population groups
# Remember to adjust this for other countries 



popgroup_labels <- c(
  "1" = "Refugees and Asylum Seekers",
  "3" = "Host Community"
)

HHroster <- HHroster %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

main <- main %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_adult <- RA_adult %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_woman <- RA_woman %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

#### Define labels for gender
gender_labels <- c(
  "1" = "Male",
  "2" = "Female"
)

# Apply labels to all gender variables in one block
HHroster <- HHroster %>%
  mutate(HH_02 = recode_factor(HH_02, !!!gender_labels))

main <- main %>%
  mutate(HH_02_HoH = recode_factor(HH_02_HoH, !!!gender_labels))

RA_adult <- RA_adult %>%
  mutate(HH_02_RA = recode_factor(HH_02_RA, !!!gender_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(HH_02_RC = recode_factor(HH_02_RC, !!!gender_labels))

# Define labels for disability
disability_labels <- c(
  "1" = "Disabled",
  "0" = "Non-Disabled"
)

HHroster <- HHroster %>%
  mutate(disability = recode_factor(disability, !!!disability_labels))

main <- main %>%
  mutate(disability_HoH = recode_factor(disability_HoH, !!!disability_labels))

RA_adult <- RA_adult %>%
  mutate(disability_RA = recode_factor(disability_RA, !!!disability_labels))

RA_woman <- RA_woman %>%
  mutate(disability_RW = recode_factor(disability_RW, !!!disability_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(disability_RC = recode_factor(disability_RC, !!!disability_labels))

table(RA_woman$disability_RW)
table(HHroster$disability)
table(main$disability)

###Define labels for Intro_09

intro09_labels <- c(
  "1"= "Rural",
  "2"= "Peri-urban",
  "3"= "Urban"
)

main <- main %>%
  mutate(Intro_09 = recode_factor(Intro_09, !!!intro09_labels))

table(main$Intro_09)


###Add additional variables for some indicators


#Variables needed for education indicators - this should be adapted for each country as the age and the education levels will change
#Step 1: Create a variable assessing completion of primary school
#For those currently in school 
HHroster <- HHroster %>%
  mutate(primary_complete_cur = case_when(
    HH_Educ02a == 1 & HH_Educ03 %in% 6:19 ~ 1, #Currently enrolled in secondary school or higher, therefore primary is completed. 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(primary_complete_past = case_when(
    HH_Educ17 == 1 & HH_Educ18 %in% 5:19 ~ 1, #Primary education completed in Pakistan
    HH_Educ17 == 2 & HH_Educ17_other == "AFG" & HH_Educ18 %in% 6:19 ~ 1, #Primary education completed in Afghanistan
    HH_Educ17 == 2 & HH_Educ17_other == "IRN" & HH_Educ18 %in% 6:15 ~ 1, #Primary education completed in Iran
    HH_Educ17 == 2 & (!is.na(HH_Educ17_other) & HH_Educ17_other != "IRN" & HH_Educ17_other != "AFG") & HH_Educ18 %in% 1:12 ~ 1, #Primary education completed somewhere else
    HH_Educ17 %in% c(3,98,99) & HH_Educ18 %in% 1:12 ~ 1, 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(primary_complete = case_when(
    primary_complete_cur == 1 | primary_complete_past == 1 ~ 1, #Primary completed for those currently is school (sec or higer) or those who indicated that they have completed primary school in the past
    TRUE ~ NA_real_))

#Step 2: Create a variable assessing completion of lower secondary school
#For those currently in school 
HHroster <- HHroster %>%
  mutate(lowseco_complete_cur = case_when(
    HH_Educ02a == 1 & HH_Educ03 %in% 9:19 ~ 1, #Currently enrolled in higher secondary school or higher, therefore lower secondary is completed. 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(lowersec_complete_past = case_when(
    HH_Educ17 == 1 & HH_Educ18 %in% 8:19 ~ 1, #Lower Secondary education completed in Pakistan
    HH_Educ17 == 2 & HH_Educ17_other == "AFG" & HH_Educ18 %in% 9:19 ~ 1, #Lower Secondary education completed in Afghanistan
    HH_Educ17 == 2 & HH_Educ17_other == "IRN" & HH_Educ18 %in% 9:15 ~ 1, #Lower Secondary education completed in Iran
    HH_Educ17 == 2 & (!is.na(HH_Educ17_other) & HH_Educ17_other != "IRN" & HH_Educ17_other != "AFG") & HH_Educ18 %in% 4:12 ~ 1, #Lower Secondary education completed somewhere else
    HH_Educ17 %in% c(3,98,99) & HH_Educ18 %in% 4:12 ~ 1, 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(lowersec_complete = case_when(
    lowersec_complete_past == 1 | lowseco_complete_cur == 1 ~ 1, #Secondary completed for those currently is school (higher sec or higer) or those who indicated that they have completed lower secondary school in the past
    TRUE ~ NA_real_))

#Step 3: Create a variable assessing completion of upper secondary school
#For those currently in school 
HHroster <- HHroster %>%
  mutate(upperseco_complete_cur = case_when(
    HH_Educ02a == 1 & HH_Educ03 %in% 13:19 ~ 1, #Currently enrolled in tertiary education, therefore higher secondary is completed. 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(uppersec_complete_past = case_when(
    HH_Educ17 == 1 & HH_Educ18 %in% 12:19 ~ 1, #Upper secondary education completed in Pakistan
    HH_Educ17 == 2 & HH_Educ17_other == "AFG" & HH_Educ18 %in% 12:19 ~ 1, #Upper secondary education completed in Afghanistan
    HH_Educ17 == 2 & HH_Educ17_other == "IRN" & HH_Educ18 %in% 12:15 ~ 1, #Upper secondary education completed in Iran
    HH_Educ17 == 2 & (!is.na(HH_Educ17_other) & HH_Educ17_other != "IRN" & HH_Educ17_other != "AFG") & HH_Educ18 %in% 11:12 ~ 1, #Upper secondary education completed somewhere else
    HH_Educ17 %in% c(3,98,99) & HH_Educ18 %in% 11:12 ~ 1, 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(uppersec_complete = case_when(
    upperseco_complete_cur == 1 | uppersec_complete_past == 1 ~ 1, #Secondary completed for those currently is tertiary school or those who indicated that they have completed upper secondary school in the past
    TRUE ~ NA_real_))

table(HHroster$primary_complete)
table(HHroster$lowersec_complete)
table(HHroster$uppersec_complete)

#Extract needed variables related to education from the roster to the RA_adult 
HHroster_selected <- HHroster %>%
  select(uuid, rosterposition, HH_Educ02a, HH_Educ07, HH_Educ14)
HHroster_selected <- HHroster_selected %>%
  rename(
    HH_Educ02a_RA = HH_Educ02a,
    HH_Educ07_RA = HH_Educ07,
    HH_Educ14_RA = HH_Educ14
  )
RA_adult <- RA_adult %>%
  left_join(HHroster_selected, by = c("uuid", "rosterposition"))

#Rename anthropometric variables to facilitate the use of anthro packages in the calculation of Z-scores 
RA_caregiver <- RA_caregiver %>%
  mutate(AN8 = case_when(
    AN8 == 1 ~ "h",  # Standing height
    AN8 == 2 ~ "l"   # Recumbent length
  ))
RA_caregiver$AN7 <- as.numeric(RA_caregiver$AN7)
RA_caregiver$AN6 <- as.numeric(RA_caregiver$AN6)
RA_caregiver$AN8 <- as.character(RA_caregiver$AN8)



#Reshaping data for the calculation of **Proportion of total adult population with secure tenure 
#rights to land, (a) with legally recognized documentation, and (b) who perceive their rights to land as secure, by sex and type of tenure**


# Create a subset with `uuid` and all variables starting with `Land`
main_subset_land <- main %>%
  select(uuid, starts_with("Land12b"))



main_subset_land <- main_subset_land %>%
  mutate(across(starts_with("Land12b"), as.numeric))  # Convert to consistent type

main_long <- main_subset_land %>%
  pivot_longer(
    cols = starts_with("Land12b"), 
    names_to = "variable", 
    values_to = "ownership")

#Label various documents 
main_long <- main_long %>%
  mutate(
    document_label = case_when(
      str_detect(variable, "^Land12b_A") ~ "Certificate of customary ownership",
      str_detect(variable, "^Land12b_B") ~ "Certificate of occupancy",
      str_detect(variable, "^Land12b_C") ~ "Certificate of hereditary acquisition listed in registry",
      str_detect(variable, "^Land12b_D") ~ "Rental contract registered",
      str_detect(variable, "^Land12b_E") ~ "Rental contract unregistered",
      str_detect(variable, "^Land12b_F") ~ "Lease registered",
      str_detect(variable, "^Land12b_G") ~ "Title deed",
      str_detect(variable, "^Land12b_H") ~ "Survey plan",
      str_detect(variable, "^Land12b_I") ~ "Note on a sharecropping arrangement",
      TRUE ~ NA_character_  # For any unmatched variables
    )
  )

main_long <- main_long %>%
  mutate(member_id = as.numeric(str_extract(variable, "(?<=Land12b_[A-I])\\d+"))) %>%
  filter(member_id >= 1 & member_id <= 50)  # Keep only valid member IDs


main_long <- main_long %>%
  mutate(ownership = ifelse(ownership >= 1 & ownership <= 50, 1, ownership))

main_long <- main_long %>%
  rename(rosterposition = member_id)

main_long <- main_long %>%
  filter(ownership == 1)


# Aggregate data so that each member gets 1 per document type if they have at least one
main_aggregated <- main_long %>%
  group_by(uuid, rosterposition, document_label) %>%
  summarise(ownership = max(ownership, na.rm = TRUE), .groups = "drop")

main_aggregated <- main_aggregated %>%
  mutate(
    document_label = as.character(document_label),  # Convert to character
    ownership = as.numeric(ownership)  # Convert to numeric
  )


main_wide <- main_aggregated %>%
  pivot_wider(
    names_from = document_label,  # Create separate columns for each document type
    values_from = ownership,      # Fill these columns with ownership values
    values_fill = list(ownership = 0)  # If missing, assume no ownership (0)
  )

# Define a mapping of new names to the expected labels
document_rename_mapping <- c(
  "Certificate of customary ownership" = "Land12b_A",
  "Certificate of occupancy" = "Land12b_B",
  "Certificate of hereditary acquisition listed in registry" = "Land12b_C",
  "Rental contract registered" = "Land12b_D",
  "Rental contract unregistered" = "Land12b_E",
  "Lease registered" = "Land12b_F",
  "Title deed" = "Land12b_G",
  "Survey plan" = "Land12b_H",
  "Note on a sharecropping arrangement" = "Land12b_I"
)

# Filter the mapping for columns that actually exist in the dataset
existing_mapping <- document_rename_mapping[names(document_rename_mapping) %in% names(main_wide)]

# Apply renaming only to the columns that exist
main_wide <- main_wide %>%
  rename_with(
    .fn = ~ existing_mapping[.x],
    .cols = names(existing_mapping)
  )


# Define the mapping of column names to labels
label_mapping <- list(
  Land12b_A = "Certificate of customary ownership",
  Land12b_B = "Certificate of occupancy",
  Land12b_C = "Certificate of hereditary acquisition listed in registry",
  Land12b_D = "Rental contract registered",
  Land12b_E = "Rental contract unregistered",
  Land12b_F = "Lease registered",
  Land12b_G = "Title deed",
  Land12b_H = "Survey plan",
  Land12b_I = "Note on a sharecropping arrangement"
)

# Apply labels only to existing columns
existing_labels <- label_mapping[names(label_mapping) %in% names(main_wide)]

main_wide <- main_wide %>%
  set_variable_labels(!!!existing_labels)  # Use dynamic labeling

HHroster <- HHroster %>%
  mutate(rosterposition = as.numeric(rosterposition))

main_wide <- main_wide %>%
  mutate(rosterposition = as.numeric(rosterposition))


# Merge `main_wide` with `HHroster`
HHroster <- HHroster %>%
  left_join(main_wide, by = c("uuid", "rosterposition"))



# Aggregate at household level (uuid), applying max() to all columns that start with "Land12b"
main_docs <- main_wide %>%
  group_by(uuid) %>%
  summarise(across(starts_with("Land12b"), ~ max(.x, na.rm = TRUE), .names = "{.col}"))


main <- main %>%
  left_join(main_docs, by = "uuid")


# Calculating old-age household members and looking at household that has at least one old_age household member
# Count number of old age people (60+) in each household
##Identify women who has a child below 2 - less than 24 months

#Identify household which have at least 1 woman who gave birth in the last 12 months 
# Step 1: Identify children <= 1 year whose mother is reported to live in the household (HH_18 == 1)

children_with_mothers <- HHroster %>%
  filter(agetouse < 2, HH_18 == 1, member == 1) %>%
  mutate(mother_rosterposition = as.character(HH_19)) %>%
  select(uuid, child_rosterposition = rosterposition, mother_rosterposition)

# Step 2: Check if the mother is a household member in the same household
valid_mothers <- HHroster %>%
  filter(member == 1) %>%
  mutate(rosterposition = as.character(rosterposition)) %>%
  select(uuid, rosterposition)

# Step 3: Join and check if mother is in household
mothers_with_child <- children_with_mothers %>%
  inner_join(valid_mothers, by = c("uuid", "mother_rosterposition" = "rosterposition")) %>%
  group_by(uuid) %>%
  summarise(num_mothers_with_child = n_distinct(mother_rosterposition), .groups = "drop")

# Step 4: Merge into main dataset
main <- main %>%
  left_join(mothers_with_child, by = "uuid") %>%
  mutate(
    num_mothers_with_child = replace_na(num_mothers_with_child, 0),
    has_mother_with_child = if_else(num_mothers_with_child >= 1, 1, 0)
  )


#Create a variable for adult population per household 

adult_pop <- HHroster %>%
  group_by(uuid) %>%
  summarise(adult_pop = sum(agetouse >= 18, na.rm = TRUE)) 

# Add the count to the main dataset
main <- main %>%
  left_join(adult_pop, by = "uuid") %>%
  mutate(adult_pop = replace_na(adult_pop, 0))


##create an age for elderly population

main <- main %>%
  left_join(
    HHroster %>%
      group_by(uuid) %>%
      summarise(elderly_pop = sum(agetouse >= 65, na.rm = TRUE), .groups = "drop"),
    by = "uuid"
  ) %>%
  mutate(has_old_age = if_else(elderly_pop >= 1, 1, 0))

# Summary statistics
summary(main$elderly_pop) # Number of old-age household members
table(main$has_old_age) # Households with at least one old-age member


#Create a variable for  HH members with disability per household 
# Count number of people with disabilities per household


table(HHroster$disability)

HHroster <- HHroster %>%
  mutate(disability = if_else(disability == "Disabled", 1, 0))

disability_count <- HHroster %>%
  group_by(uuid) %>%
  summarise(disability_count = sum(disability, na.rm = TRUE))

# Merge with main dataset
main <- main %>%
  left_join(disability_count, by = "uuid") %>%
  mutate(disability_count = replace_na(disability_count, 0))

# Create indicator for households with at least one person with disability
main <- main %>%
  mutate(has_disability = if_else(disability_count >= 1, 1, 0))

table(main$has_disability)

table(HHroster$disability)

HHroster <- HHroster %>%
  mutate(disability = recode_factor(disability, !!!disability_labels))

##Add the weights

##Import the dataset with weights

#Located here https://unhcr365.sharepoint.com/teams/eo-gdssds-GlobalSurveyTeam/Shared%20Documents/Forms/AllItems.aspx?id=%2Fteams%2Feo%2Dgdssds%2DGlobalSurveyTeam%2FShared%20Documents%2F300%20%2D%20ST%20%2D%20Survey%20Team%20%2D%20Main%2FSurvey%20Programme%20Team%2FProjects%2FFDS%2FCountries%2FPakistan%2FData%20Management%2F1%20Supplementary&FolderCTID=0x012000FFA85D851558C0448148D700F353179C

weights <- read_dta ("PAK24_HH_Weights.dta")


##Add labels to the weights

sample_strata_labels <- c(
  "1" = "REF-Balochistan in camp",
  "2" = "REF-Punjab in camp",
  "3" = "REF-KPK in camp",
  "4" = "REF-Balochistan out",
  "5" = "REF-Punjab out",
  "6" = "REF-KPK out",
  "7" = "REF-Metro-Islamabad out",
  "8" = "REF-Metro-Sindh out",
  "9" = "HOSTS-Balochistan",
  "10" = "HOSTS-Punjab",
  "11" = "HOSTS-KPK"
)


province_labels <- c(
  "1" = "Khyber-Pakhtunkhwa",
  "3" = "Punjab",
  "4" = "Sindh",
  "5" = "Balochistan",
  "6" = "Islamabad",
  "7" = "Gilgit-Baltistan",
  "8" = "Azad Kashmir"
)

weights <- weights %>%
  mutate(prov_strata = recode_factor(prov_strata, !!!province_labels))


weights <- weights %>%
  mutate(samp_strat = recode_factor(samp_strat, !!!sample_strata_labels))

###Merge weights 

weights<- weights %>%
  rename(uuid = `_uuid`)

# Variable Labels
var_label(weights) <- list(
  samp_strat = "Sampling Strata",
  prov_strata = "Province Strata",
  prov_str = "Provincial Population Structure",
  pop_str = "Population Structure Sampling Strata",
  wgh_samp_resc = "Adjusted Inverse Probability Weights (HH & Roster)",
  wgh_strata_spec = "Sampling Strata Specific Weights (HH & Roster)",
  wgh_prov_spec = "Province Specific Weights (HH & Roster)",
  wgh_samp_pop_restr = "National Population Weights (HH & Roster)",
  n_rand_resp = "Number of Eligible Random Respondents",
  n_rand_u5 = "Number of Eligible Random Children",
  n_rand_w = "Number of Eligible Women",
  wgh_strata_spec_resp = "Sampling Strata Specific Weights Random Respondent",
  wgh_samp_pop_restr_resp = "National Population Weights Random Respondent",
  wgh_prov_spec_resp = "Province Specific Weights Random Respondent",
  wgh_strata_spec_u5 = "Sampling Strata Specific Weights Random Child",
  wgh_samp_pop_restr_u5 = "National Population Weights Random Child",
  wgh_prov_spec_u5 = "Province Specific Weights Random Child",
  wgh_strata_spec_w = "Sampling Strata Specific Weights Random Woman",
  wgh_samp_pop_restr_w = "National Population Weights Random Woman",
  wgh_prov_spec_w = "Province Specific Weights Random Woman"
)



###Merging


main <- main %>%
  left_join(weights %>% select(uuid, wgh_strata_spec, wgh_prov_spec, wgh_samp_pop_restr, samp_strat,
                               prov_strata), by = "uuid")

HHroster <- HHroster %>%
  left_join(weights %>% select(uuid, wgh_strata_spec, wgh_prov_spec, wgh_samp_pop_restr, samp_strat,
                               prov_strata), by = "uuid")

RA_adult <- RA_adult %>%
  left_join(weights %>% select(uuid, wgh_strata_spec_resp, wgh_samp_pop_restr_resp, wgh_prov_spec_resp, samp_strat,
                               prov_strata), by = "uuid")

RA_woman <- RA_woman %>%
  left_join(weights %>% select(uuid, wgh_strata_spec_w, wgh_samp_pop_restr_w, wgh_prov_spec_w, samp_strat,
                               prov_strata), by = "uuid")

RA_caregiver <- RA_caregiver %>%
  left_join(weights %>% select(uuid, wgh_strata_spec_u5, wgh_samp_pop_restr_u5, wgh_prov_spec_u5, samp_strat,
                               prov_strata), by = "uuid")




###Add header variables to other datasets

##Sample strata variable

###Location and urban/rural variables from main dataset

main <- main %>%
  mutate(Intro_03a_NUTS1 = recode_factor(Intro_03a_NUTS1, !!!province_labels))


HHroster <- HHroster %>%
  left_join(main %>% select("uuid", Intro_03a_NUTS1, Intro_03b_NUTS2,Intro_03c_NUTS3, Intro_09, 
                            start, end), by = "uuid")

RA_adult <- RA_adult %>%
  left_join(main %>% select("uuid", Intro_03a_NUTS1, Intro_03b_NUTS2,Intro_03c_NUTS3, Intro_09, 
                            start, end), by = "uuid")

RA_woman <- RA_woman %>%
  left_join(main %>% select("uuid", Intro_03a_NUTS1, Intro_03b_NUTS2,Intro_03c_NUTS3, Intro_09, 
                            start, end), by = "uuid")

RA_caregiver <- RA_caregiver %>%
  left_join(main %>% select("uuid", Intro_03a_NUTS1, Intro_03b_NUTS2,Intro_03c_NUTS3, Intro_09, 
                            start, end), by = "uuid")


###Move headers
main <- main %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
    HH_04_HoH, HH_02_HoH, disability_HoH, COO_HoH, Intro_09, 
    wgh_strata_spec, wgh_prov_spec, wgh_samp_pop_restr, samp_strat, prov_strata,
    everything()
  )

HHroster <- HHroster %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3, agetouse,
    HH_04_cat2, HH_04_cat4, HH_02, disability, COO, Intro_09, uuid,
    wgh_strata_spec, wgh_prov_spec, wgh_samp_pop_restr, samp_strat, prov_strata,
    everything()
  )

RA_adult <- RA_adult %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
    age_selected, HH_02_RA, disability_RA, COO_RA, Intro_09, uuid,
    wgh_strata_spec_resp, wgh_samp_pop_restr_resp, wgh_prov_spec_resp, samp_strat, prov_strata,
    everything()
  )

RA_woman <- RA_woman %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
    agerandomwoman, disability_RW, COO_RW, Intro_09, uuid, 
    wgh_strata_spec_w, wgh_samp_pop_restr_w, wgh_prov_spec_w, samp_strat, prov_strata,
    everything()
  )

RA_caregiver <- RA_caregiver %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
    HH_04_RC, HH_02_RC, disability_RC, COO_RC, Intro_09, uuid,
    wgh_strata_spec_u5, wgh_samp_pop_restr_u5, wgh_prov_spec_u5, samp_strat, prov_strata,
    everything()
  )

###Save the datasets in a STATA dta. 
###change your directory
output_path <- "C:/Users/BOZDAG/OneDrive - UNHCR/Desktop/UNHCR/R projets/PAK HL report/PAK_HL_report"

#Save the datasets in a R dataset.

saveRDS(HHroster, "HHroster.rds")
saveRDS(main, "main.rds")
saveRDS(RA_adult,"RA_adult.rds")
saveRDS(RA_woman, "RA_woman.rds")
saveRDS(RA_caregiver, "RA_caregiver.rds")


###WEIGHTED SURVEY OBJECTS BY POPULATION

FDS_PAK_2024_main <- main %>%
  as_survey_design(
    strata=samp_strat,
    weights = wgh_samp_pop_restr, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters )
  )

FDS_PAK_2024_HHroster <- HHroster %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

FDS_PAK_2024_RA_adult <- RA_adult %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr_resp, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

FDS_PAK_2024_RA_caregiver <- RA_caregiver %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr_u5, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


FDS_PAK_2024_RA_woman <- RA_woman %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr_w, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

