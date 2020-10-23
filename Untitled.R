Validation_fitted_df_all$GFR - Validation_fitted_df_all$


Expansion_data %>%
  group_by(PatientID) %>%
  summarise(l = length(unique(fit_index))) %>%
  filter(l != 1) %>% 
  pull(PatientID) -> repeats



Results_CreatType_all <- Validation_fitted_df_all %>%
  mutate(Repeats = PatientID %in% repeats) %>%
  group_by(equation, Creatinine_type, Repeats) %>%
  Statistic_summary_withP10_P20_P30() %>%
  arrange(Creatinine_type, RMSE) 



Results_CreatType_all %>%
  arrange(Repeats, Creatinine_type, RMSE)  %>% print(n = 40 ) %>%
  filter(equation == "CKD-EPI")
filter(equation %in% c("CamGFR v2", "CKD-EPI", "MDRD", "LM", "FAS",
                         "CamGFR v1"))


Validation_fitted_df_all %>%
  filter(equation %in% c("Piecewise linear", "Leave-one-out", "CamGFR v2")) %>%
  group_by(Creatinine_type, equation) %>%
  summarise(n = n(), rmse = sqrt(mean(resid^2))) 



            