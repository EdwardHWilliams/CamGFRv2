library(readr)
# library(tidyverse)
library(dplyr)
# source("../Rfiles_shared/Functions_for_stat_tables.R")

files_location <- "../../Data/Clean_data/"


All_data <- read_csv(paste0(files_location, "All_clean_GFR_data.csv"),
                     col_types = cols(DOB = col_date(format = "%Y-%m-%d"),
                                      Date_GFR = col_date(format = "%Y-%m-%d"),
                                      Date_creat = col_date(format = "%Y-%m-%d"),
                                      Date_diagnosis = col_date(format = "%Y-%m-%d"),
                                      Date_diff = col_number(),
                                      Age = col_double()))

All_data <- read_csv(paste0(files_location, "All_clean_GFR_data.csv"),
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

# Adden_long <- read_csv(paste0(files_location, "Addenbrooks_longitudinal_creatinine.csv"),
#                        col_types = cols(DOB = col_date(format = "%Y-%m-%d"),
#                                         Date_GFR = col_date(format = "%Y-%m-%d"),
#                                         Date_creat = col_date(format = "%Y-%m-%d"),
#                                         Date_diff = col_number(),
#                                         Age = col_double()))
# 
# Adden_new_long <- read_csv(paste0(files_location, "Addenbrookes_longitudinal_creat_new_annonymised.csv"),
#                        col_types = cols(DOB = col_date(format = "%Y-%m-%d"),
#                                         Date_GFR = col_date(format = "%Y-%m-%d"),
#                                         Date_creat = col_date(format = "%Y-%m-%d"),
#                                         Date_diff = col_number(),
#                                         Age = col_double()))

Adden_long_combined <-  read_csv(paste0(files_location, "Addenbrookes_combined_longitudinal_clean_GFR_data.csv"),
                                 col_types = cols(DOB = col_date(format = "%Y-%m-%d"),
                                                  Date_GFR = col_date(format = "%Y-%m-%d"),
                                                  Date_creat = col_date(format = "%Y-%m-%d"),
                                                  Date_diff = col_number(),
                                                  Age = col_double())) %>%
  filter(Age > 18)

#Sub-variables used by other files

Validation_data <- All_data %>%
  filter(GFR_index == 1) %>%
  filter(!(Centre %in% c("Glasgow", "Adden_old")))

Seminoma_combined <- All_data %>%
  filter(GFR_index == 1) %>%
  filter(Diagnosis == "Testis")

Ovarian_combined <- All_data %>%
  filter(GFR_index == 1) %>%
  filter(Diagnosis == "Ovarian")

Lung_combined <- All_data %>%
  filter(GFR_index == 1) %>%
  filter(Diagnosis == "Lung")

Bladder_combined <-All_data %>%
  filter(GFR_index == 1) %>%
  filter(Diagnosis == "Bladder/TCC")

Adden_data_3 <- read_csv(paste0(files_location, "Addenbrookes_clean_GFR_data_3.csv"),
                      col_types = cols(Age = col_double())) %>%
  # filter(Patient_type == "Cancer") %>%
  filter(Age >= 18)

Adden_new <- read_csv(paste0(files_location, "Addenbrookes_new_clean_GFR_data.csv"),
                     col_types = cols(Age = col_double())) %>%
  # filter(Patient_type == "Cancer") %>%
  filter(Age >= 18)

Adden_new_full <- read_csv(paste0(files_location, "Addenbrookes_new_clean_GFR_data.csv"),
                      col_types = cols(Age = col_double()))

Adden_original <- read_csv(paste0(files_location, "Addenbrookes_original_clean_GFR_data.csv"),
                      col_types = cols(Age = col_double())) %>%
  filter(Age >= 18)

Barts <- read_csv(paste0(files_location, "Barts_clean_GFR_data.csv"),
                     col_types = cols(Age = col_double())) %>%
  filter(Age >= 18)

# need to call it southampton for consistancy
Southampton <- read_csv(paste0(files_location, "Southampton_clean_GFR_data.csv"),
                     col_types = cols(Age = col_double()))  %>%
  filter(Age >= 18)

Edinburgh <- read_csv(paste0(files_location, "Edinburgh_clean_GFR_data.csv"),
                     col_types = cols(Age = col_double())) %>%
  filter(Age >= 18)

Glasgow <- read_csv(paste0(files_location, "Glasgow_clean_GFR_data.csv"),
                    col_types = cols(Age = col_double())) %>%
  filter(Age >= 18)


Goteborg <- read_csv(paste0(files_location, "Goteborg_clean_GFR_data.csv"),
                     col_types = cols(Age = col_double())) %>%
  filter(GFR_index == 1) %>%
  filter(Age >= 18)


Manchester <- read_csv(paste0(files_location, "Manchester_clean_GFR_data.csv"),
                     col_types = cols(Age = col_double())) %>%
  filter(GFR_index == 1) %>%
  filter(Age >= 18)

Melbourne <- read_csv(paste0(files_location, "Melbourne_clean_GFR_data.csv"),
                     col_types = cols(Age = col_double())) %>%
  filter(GFR_index == 1) %>%
  filter(Age >= 18)


SW_combined <- read_csv(paste0(files_location, "SW_clean_GFR_data.csv"),
                     col_types = cols(Age = col_double())) %>%
  filter(GFR_index == 1) %>%
  filter(Age >= 18)


################################################################################

Adden_combined <- Adden_new %>%
  bind_rows(Adden_original) %>%
  bind_rows(Adden_data_3) %>%
  group_by(PatientID) %>%
  arrange(PatientID, Date_GFR) %>%
  mutate(Number_GFR = n(), GFR_index = 1:n()) %>%
  mutate(Which_EPIC = ifelse(is.na(Which_EPIC), "pre", Which_EPIC)) %>%
  ungroup()


All_data_IDMS <- All_data %>%
  filter(Creatinine_type == "IDMS")


################################################################################


Blood_data_long_3 <- read_csv("../../Data/Clean_data/Addenbrookes_full_bloods/Adden_bloods_data_3.csv")
Blood_data_long <- read_csv("../../Data/Clean_data/Addenbrookes_full_bloods/Adden_new_bloods_data.csv") %>%
  bind_rows(Blood_data_long_3)

GFR_data_3 <-read_csv("../../Data/Clean_data/Addenbrookes_full_bloods/Adden_GFR_data_3.csv")
GFR_data <-read_csv("../../Data/Clean_data/Addenbrookes_full_bloods/Adden_new_GFR_data.csv") %>%
  bind_rows(GFR_data_3)



