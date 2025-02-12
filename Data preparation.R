#setwd("C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis")


## Install pacman if not already installed
if(!require(pacman)) install.packages('pacman')

## Load all required libraries using pacman
pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven )

HHroster <- read_dta("HHroster.dta")
main <- read_dta("main.dta")
RA_adult <- read_dta("RA_adult.dta")
RA_woman <- read_dta("RA_woman.dta")
RA_caregiver <- read_dta ("RA_caregiver.dta")


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


# Define labels for population groups
popgroup_labels <- c(
  "1" = "Refugees and Asylum Seekers",
  "2" = "Host Community"
)

HHroster <- HHroster %>%
  mutate(
    Intro_07 = as_factor(Intro_07) %>%          # Convert to factor
      recode_factor(!!!popgroup_labels)        # Apply recoding
  )

RA_caregiver  <- RA_caregiver %>%
  mutate(
    Intro_07 = as_factor(Intro_07) %>%          # Convert to factor
      recode_factor(!!!popgroup_labels)        # Apply recoding
  )
main <- main %>%
  mutate(
    Intro_07 = as_factor(Intro_07) %>%          # Convert to factor
      recode_factor(!!!popgroup_labels)        # Apply recoding
  )

RA_adult  <- RA_adult %>%
  mutate(
    Intro_07 = as_factor(Intro_07) %>%          # Convert to factor
      recode_factor(!!!popgroup_labels)        # Apply recoding
  )
RA_woman  <- RA_woman %>%
  mutate(
    Intro_07 = as_factor(Intro_07) %>%          # Convert to factor
      recode_factor(!!!popgroup_labels)        # Apply recoding
  )
#### Define labels for gender
gender_labels <- c(
  "1" = "Male",
  "2" = "Female"
)
# Apply labels to all gender variables in one block
HHroster <- HHroster %>%
  mutate(
    HH_02 = as_factor(HH_02) %>%          # Convert to factor
      recode_factor(!!!gender_labels)        # Apply recoding
  )

RA_caregiver  <- RA_caregiver %>%
  mutate(
    HH_02_RC = as_factor(HH_02_RC) %>%          # Convert to factor
      recode_factor(!!!gender_labels)        # Apply recoding
  )
main <- main %>%
  mutate(
    HH_02_HoH = as_factor(HH_02_HoH) %>%          # Convert to factor
      recode_factor(!!!gender_labels)        # Apply recoding
  )

RA_adult  <- RA_adult %>%
  mutate(
    HH_02_RA = as_factor(HH_02_RA) %>%          # Convert to factor
      recode_factor(!!!gender_labels)        # Apply recoding
  )


# Define labels for disability
disability_labels <- c(
  "1" = "Disabled",
  "2" = "Non-Disabled"
)

HHroster <- HHroster %>%
  mutate(
    disability = as_factor(disability) %>%          # Convert to factor
      recode_factor(!!!disability_labels))       # Apply recoding
  

RA_caregiver  <- RA_caregiver %>%
  mutate(
    disability_RC = as_factor(disability_RC) %>%          # Convert to factor
      recode_factor(!!!disability_labels)        # Apply recoding
  )
main <- main %>%
  mutate(
    disability_HoH = as_factor(disability_HoH) %>%          # Convert to factor
      recode_factor(!!!disability_labels)        # Apply recoding
  )

RA_adult  <- RA_adult %>%
  mutate(
    disability_RA = as_factor(disability_RA) %>%          # Convert to factor
      recode_factor(!!!disability_labels)        # Apply recoding
  )
RA_woman <- RA_woman %>%
  mutate(
    disability_RW = as_factor(disability_RW) %>%          # Convert to factor
      recode_factor(!!!disability_labels)        # Apply recoding
  )


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


