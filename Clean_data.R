library(readr)
# library(tidyverse)
library(dplyr)
# source("../Rfiles_shared/Functions_for_stat_tables.R")

files_location <- getwd()

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


