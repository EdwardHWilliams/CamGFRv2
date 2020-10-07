library(readr)
# library(tidyverse)
library(dplyr)
# source("../Rfiles_shared/Functions_for_stat_tables.R")

files_location <- getwd()

All_data <- read_csv(paste0(files_location, "/All_clean_GFR_data.csv"),
                     col_types = cols(DOB = col_date(format = "%Y-%m-%d"),
                                      Date_GFR = col_date(format = "%Y-%m-%d"),
                                      Date_creat = col_date(format = "%Y-%m-%d"),
                                      Date_diagnosis = col_date(format = "%Y-%m-%d"),
                                      Date_diff = col_number(),
                                      Age = col_double(), 
                                      norm_GFR = col_double(), 
                                      Creatinine = col_double(),
                                      Sodium = col_double(),
                                      Urea = col_double(),
                                      Albumin = col_double(),
                                      GFR_corrected_error = col_double(),
                                      GFR_corrected_error = col_double(),
                                      norm_GFR_expected = col_double(),
                                      Hb = col_double(),
                                      Platelets = col_double(),
                                      Wbc = col_double(),
                                      Neutrophils = col_double(),
                                      Lymphocytes = col_double(),
                                      Eosinophils = col_double(),
                                      Potassium = col_double(),
                                      Bicarbonate = col_double(),
                                      Chloride = col_double(),
                                      Bilirubin = col_double(),
                                      ALT = col_double(),
                                      AST = col_double(),
                                      ALP = col_double(),
                                      GGT = col_double(),
                                      Total_protein = col_double()
                                      ))

Adden_long_combined <-  read_csv(paste0(files_location, "/Addenbrookes_combined_longitudinal_clean_GFR_data.csv"),
                                 col_types = cols(DOB = col_date(format = "%Y-%m-%d"),
                                                  Date_GFR = col_date(format = "%Y-%m-%d"),
                                                  Date_creat = col_date(format = "%Y-%m-%d"),
                                                  Date_diff = col_number(),
                                                  Age = col_double())) %>%
  filter(Age > 18)

################################################################################


# All data that is used at somepoint in this analysis
New_nonIDMS_data_JNCI <- All_data %>%
  mutate(BSA = SufA) %>%
  filter(Age >= 18) %>% # removing any patients under 18 years old
  # filter(GFR_index == 1) %>% # keeping only a patients first GFR measurment
  group_by(PatientID) %>%
  filter(Creatinine_type != "IDMS") %>%
  mutate(Date_GFR_diff = c(NA, diff(Date_GFR))) %>%
  ungroup() %>%
  # filter(Date_GFR_diff > 365 | is.na(Date_GFR_diff)) %>%
  filter(GFR_index == 1) %>%
  filter(Creat*88.4 > 18) %>% # remove creatinien values less than 18 as some centres do not report creatinine values lwer than this value
  filter(Creat*88.4 < 400) %>% # removed as
  filter(!Centre %in% c("Adden_old", "Glasgow")) %>% # remove data from previous study 
  mutate(Centre = factor(Centre, 
                         labels = c("Manchester", "Edinburgh", "Cambridge", 
                                    "Southampton", "Melbourne", "Wales",
                                    "London-Barts"),
                         levels = c("Manchester", "Edinburgh", "Adden_new",  
                                    "Southampton", "Melbourne", "SW", "Barts"))) %>%
  mutate(Centre = as.character(Centre), Ethnicity = as.character(Ethnicity)) %>%
  mutate(Ethnicity_black = ifelse(is.na(Ethnicity), 0, Ethnicity == "Black")) %>%
  mutate(Ethnicity = ifelse(Ethnicity == "Unknown", NA, Ethnicity)) %>%
  mutate(Diagnosis = ifelse(Diagnosis == "Seminoma", "GCT", Diagnosis)) %>%
  mutate(Patient_type = ifelse(Patient_type %in% c("Donor", "Misc"), "Non-cancer",
                               Patient_type)) %>% 
  mutate(Patient_type = ifelse(Diagnosis == "Non-cancer", "Non-cancer", Patient_type)) %>%
  mutate(Diagnosis = ifelse(Diagnosis == "Non-cancer", "Unknown", Diagnosis)) %>%
  mutate(Ethnicity = ifelse(Ethnicity == "Mixed/Other", "Other", Ethnicity))
