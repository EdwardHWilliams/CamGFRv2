library(readr)
library(dplyr)

files_location <- getwd()

All_data <-
  read_csv(
    file.path(files_location, "All_clean_GFR_data.csv"),
    col_types = cols(
      Age = col_double(),
      Albumin = col_double(),
      ALP = col_double(),
      ALT = col_double(),
      AST = col_double(),
      AUC = col_double(),
      Bicarbonate = col_double(),
      Bilirubin = col_double(),
      Barts_additional_dose = col_character(),
      Barts_adj_met = col_character(),
      Barts_Creat_post = col_double(),
      Barts_Date_creat_post = col_date(format = "%Y-%m-%d"),
      Barts_relapse = col_character(),
      BSA = col_double(),
      Chloride = col_double(),
      Clinician = col_character(),
      Creatinine = col_double(),
      Creat_umol = col_double(),
      Date_GFR = col_date(format = "%Y-%m-%d"),
      Date_creat = col_date(format = "%Y-%m-%d"),
      Date_diagnosis = col_date(format = "%Y-%m-%d"),
      Date_diff = col_number(),
      Date_dose = col_date(format = "%Y-%m-%d"),
      Dept = col_character(),
      Diabetes = col_character(),
      Diabetes_numeric = col_number(),
      Diagnosis_comment = col_character(),
      Diagnosis_long = col_character(),
      diagnosis_name = col_character(),
      DOB = col_date(format = "%Y-%m-%d"),
      Dose_given = col_double(),
      Eosinophils = col_double(),
      Ethnicity = col_character(),
      Ethnicity_detailed = col_character(),
      GGT = col_double(),
      GFR_corrected_error = col_double(),
      GFR_corrected_expected = col_double(),
      Hb = col_double(),
      HBP = col_character(),
      ICD_10_code = col_character(),
      icd_10 = col_character(),
      Insuline = col_character(),
      Lymphocytes = col_double(),
      MCV = col_double(),
      Metformin = col_character(),
      Neutrophils = col_double(),
      norm_GFR = col_double(),
      norm_GFR_error = col_double(),
      norm_GFR_expected = col_double(),
      NSAID = col_character(),
      Other = col_character(),
      "Ordering Dept" = col_character(),
      Platelets = col_double(),
      Potassium = col_double(),
      Previous_treatment = col_character(),
      Sodium = col_double(),
      Tumour_grade = col_character(),
      Total_protein = col_double(),
      Urea = col_double(),
      "VD L" = col_double(),
      Wbc = col_double(),
      Which_EPIC = col_character(),
      "+/- ml/min" = col_double(),
      "+/- ml/min/1.73m2" = col_double()
    )
  )

Adden_long_combined <-
  read_csv(
    paste0(
      files_location,
      "/Addenbrookes_combined_longitudinal_clean_GFR_data.csv"
    ),
    col_types = cols(
      Clinician = col_character(),
      Creatinine = col_double(),
      Dept = col_character(),
      Diagnosis_comment = col_character(),
      DOB = col_date(format = "%Y-%m-%d"),
      Date_GFR = col_date(format = "%Y-%m-%d"),
      Date_creat = col_date(format = "%Y-%m-%d"),
      Comment = col_character(),
      Date_diff = col_number(),
      Ethnicity = col_character(),
      Ethnicity_detailed = col_character(),
      GFR_corrected_error = col_double(),
      GFR_corrected_expected = col_double(),
      Age = col_double(),
      BSA = col_double(),
      norm_GFR = col_double(),
      norm_GFR_error = col_double(),
      norm_GFR_expected = col_double(),
      Radiopharm = col_character(),
      "Ordering Dept" = col_character(),
      "VD L" = col_double(),
      Which_EPIC = col_character(),
      "+/- ml/min" = col_double(),
      "+/- ml/min/1.73m2" = col_double()
    )
  ) %>%
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
  filter(Creat * 88.4 > 18) %>% # remove creatinien values less than 18 as some centres do not report creatinine values lwer than this value
  filter(Creat * 88.4 < 400) %>% # removed as
  filter(!Centre %in% c("Adden_old", "Glasgow")) %>% # remove data from previous study
  mutate(Centre = factor(
    Centre,
    labels = c(
      "Manchester",
      "Edinburgh",
      "Cambridge",
      "Southampton",
      "Melbourne",
      "Wales",
      "London-Barts"
    ),
    levels = c(
      "Manchester",
      "Edinburgh",
      "Adden_new",
      "Southampton",
      "Melbourne",
      "SW",
      "Barts"
    )
  )) %>%
  mutate(Centre = as.character(Centre),
         Ethnicity = as.character(Ethnicity)) %>%
  mutate(Ethnicity_black = ifelse(is.na(Ethnicity), 0, Ethnicity == "Black")) %>%
  mutate(Ethnicity = ifelse(Ethnicity == "Unknown", NA, Ethnicity)) %>%
  mutate(Diagnosis = ifelse(Diagnosis == "Seminoma", "GCT", Diagnosis)) %>%
  mutate(Patient_type = ifelse(
    Patient_type %in% c("Donor", "Misc"),
    "Non-cancer",
    Patient_type
  )) %>%
  mutate(Patient_type = ifelse(Diagnosis == "Non-cancer", "Non-cancer", Patient_type)) %>%
  mutate(Diagnosis = ifelse(Diagnosis == "Non-cancer", "Unknown", Diagnosis)) %>%
  mutate(Ethnicity = ifelse(Ethnicity == "Mixed/Other", "Other", Ethnicity))
