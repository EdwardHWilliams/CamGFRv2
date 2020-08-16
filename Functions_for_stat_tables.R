source("Statistical_functions_rmse_etc.R")
source("Published_models.R")

# Calculates fitted values for WilliamsJanowitz and Adj CKD
WilliamsJanowitz_model <- function(Age, Creat, Sex, SufA){
  log_Creat = log(Creat)
  predict(Addenbrooks_sqrt_model, newdata = data.frame(Age, log_Creat, Sex, SufA))^2
}
WilliamsJanowitz_model_data <- function(data){
  predict(Addenbrooks_sqrt_model, 
          newdata = mutate(data, log_Creat = log(Creat)))^2
}
WilliamsJanowitz_model_data_IDMS <- function(data){
  data$Creat = data$Creat*1.065 + 0.067
  predict(Addenbrooks_sqrt_model, 
          newdata = mutate(data, log_Creat = log(Creat)))^2
}
CKD_adj_model_data <- function(data){
  CKD_adj_model(data$Sex, data$Creat, data$Age, BSA = data$SufA)
}

# Calculates fitted values for all models
GFR_fitted_values <- function(data){
  MDRD <- MDRD_equation(data$Age, data$Creat, data$Sex)
  MDRD_adj <- MDRD_equation_corrected(data$Age, data$Creat, data$Sex, data$SufA)
  MDRD_IDMS <- MDRD_equation_IDMS(data$Age, data$Creat, data$Sex)
  MDRD_adj_IDMS <- MDRD_adj_equation_IDMS(data$Age, data$Creat, data$Sex, data$SufA)
  Cockcroft <- Cockcroft_equation(data$Age, data$Wt, data$Sex, data$Creat)
  Jelliffe <- Jelliffe_equation(data$Age, data$Creat, data$Sex)
  Jelliffe_adj <- Jelliffe_adj_equation(data$Age, data$Creat, data$Sex, data$SufA)
  Wright <- Wright_equation(data$Age, data$Creat, data$Sex, data$SufA)
  CKD <- CKD_model(data$Sex, data$Creat, data$Age)
  CKD_adj <- CKD_adj_model(data$Sex, data$Creat, data$Age, BSA = data$SufA)
  Mayo <- Mayo_equation(data$Age, data$Creat, data$Sex)
  Mayo_adj <- Mayo_adj_equation(data$Age, data$Creat, data$Sex, data$SufA)
  Martin <- Martin_equation(data$Age, data$Creat, data$Sex, data$Wt)
  LM <- LM_revised_equation(data$Age, data$Creat, data$Sex)
  LM_adj <- LM_revised_adj_equation(data$Age, data$Creat, data$Sex, data$BSA)
  Giglio_adj <- Giglio_adj(data$Age, data$Creat, data$Sex, data$Ht, data$Wt, data$BSA)
  WJ <- WilliamsJanowitz_model_data(data)
  WJ_IDMS <- WilliamsJanowitz_model_data_IDMS(data)
  fitted_values <- data.frame(WJ, WJ_IDMS, CKD_adj, CKD,  MDRD, MDRD_IDMS, 
                              MDRD_adj, MDRD_adj_IDMS, Cockcroft, Jelliffe, 
                              Jelliffe_adj, Wright, Mayo, Mayo_adj, Martin, LM_adj, Giglio_adj)
  return(fitted_values)
}

GFR_fitted_values_eth <- function(data){
  MDRD_adj <- MDRD_186_ethnicity(data$Age, data$Creat, data$Sex, data$SufA, 
                                 data$Ethnicity == "Black")
  MDRD_adj_IDMS <- MDRD_175_ethnicity(data$Age, data$Creat, data$Sex, data$SufA, 
                                      data$Ethnicity == "Black")
  Cockcroft <- Cockcroft_equation(data$Age, data$Wt, data$Sex, data$Creat)
  Jelliffe_adj <- Jelliffe_adj_equation(data$Age, data$Creat, data$Sex, data$SufA)
  Wright <- Wright_equation(data$Age, data$Creat, data$Sex, data$SufA)
  CKD_adj <- CKD_adj_ethnicity(data$Sex, data$Creat, data$Age, BSA = data$SufA, 
                               data$Ethnicity == "Black")
  Mayo_adj <- Mayo_adj_equation(data$Age, data$Creat, data$Sex, data$SufA)
  Martin <- Martin_equation(data$Age, data$Creat, data$Sex, data$Wt)
  LM_adj <- LM_revised_adj_equation(data$Age, data$Creat, data$Sex, data$BSA)
  FAS_adj <- FAS_adj(data$Age, data$Creat, data$Sex, data$BSA)
  Giglio_adj <- Giglio_adj(data$Age, data$Creat, data$Sex, data$Ht, data$Wt, data$BSA)
  WJ <- WilliamsJanowitz_model_data(data)
  fitted_values <- data.frame(WJ, CKD_adj, LM_adj, FAS_adj, 
                              MDRD_adj, MDRD_adj_IDMS, Cockcroft, 
                              Jelliffe_adj, Wright, Mayo_adj, Martin, Giglio_adj)
  return(fitted_values)
}


apply_func_to_fitted_vals <- function(func, actual, fitted, Dose=F){
  thefunc <- function(x){
    func(actual, x, Dose=Dose)
  }
  apply(fitted, 2, thefunc)
}
apply_func_to_fitted_vals_under60 <- function(func, actual, fitted, Dose=F){
  thefunc <- function(x){
    res = x<60
    func(actual, x, restriction=res, Dose=Dose)
  }
  apply(fitted, 2, thefunc)
}
apply_func_to_fitted_vals_60more <- function(func, actual, fitted, Dose=F){
  thefunc <- function(x){
    res = x>=60
    func(actual, x, restriction=res, Dose=Dose)
  }
  apply(fitted, 2, thefunc)
}



# Takes a dataset that has the correct colnames and produced the stats table
print_stats_tables <- function(dataset){
  GFR <- dataset$GFR
  fitted_values <- GFR_fitted_values(dataset)
  equations <- c(Median_estimate, RMSE, Resid_med, resid_IQR, PE, APE, APE_under20)
  Statistics_table <- sapply(equations, function(x) 
    apply_func_to_fitted_vals(x, GFR, fitted_values))
  colnames(Statistics_table) <- 
    c("GFR measurmet", "RMSE", "Residual Median", "Residual IQR", "PE", "APE", "APE under 20%")
  Statistics_table
}

print_stats_tables_dose <- function(dataset, AUC = 5){
  GFR <- dataset$GFR
  fitted_values <- GFR_fitted_values(dataset)
  equations <- c(Median_estimate, RMSE, Resid_med, resid_IQR, PE, APE, APE_under20)
  GFR <- (GFR +25)*5
  fitted_values <- (fitted_values +25)*5
  Statistics_table <- sapply(equations, function(x) 
    apply_func_to_fitted_vals(x, GFR, fitted_values))
  colnames(Statistics_table) <- 
    c("GFR measurmet", "RMSE", "Residual Median", "Residual IQR", "PE", "APE", "APE under 20%")
  Statistics_table
}

print_stats_tables_simple <- function(dataset){
  Stats_table = print_stats_tables(dataset)
  Stats_table[c(1,2), c(1,2,3,7)]
}


print_stats_tables_RMSE_medres <- function(dataset){
  GFR <- dataset$GFR
  fitted_values <- GFR_fitted_values(dataset)
  equations <- c(RMSE_numeric, Resid_med_numeric)
  Statistics_table <- sapply(equations, function(x) 
    apply_func_to_fitted_vals(x, GFR, fitted_values))
  colnames(Statistics_table) <- 
    c("RMSE", "Residual Median")
  Statistics_table[c(1,2,5,9),]
}



################################################################################
# Functions for writing list of tables to xlsx file

Stats_table_list_write_xlsx <- function(st, filename){
  require(xlsx)
  wb = createWorkbook()
  
  for(i in 1:length(st)){
    name = names(st)[i]
    sheet = createSheet(wb, name)
    addDataFrame(st[[i]], sheet = sheet, startColumn = 1, row.names = T)
  }
  saveWorkbook(wb, filename)
}



################################################################################
# Function that returns residuals for given dataset
get_residuals <- function(dataset, which="simple"){
  GFR <- dataset$GFR
  fitted_values <- GFR_fitted_values(dataset)
  residual = GFR - fitted_values
  if(which == "simple2"){
  	return(residual[,c("WJ", "CKD_adj", "MDRD", "Cockcroft")])
  } else if(which == "simple"){
  	return(residual[,c("WJ", "CKD_adj")])
  } else if(which == "all"){
  	return(residual)
  } else {
  	return(residual[,which])
  }
}



Clean_output_table <- function(table){
  table %>%
    ungroup() %>%
    mutate(equation = factor(equation,
                             levels = c("WJ", "CKD_adj", "Wright", "MDRD_adj",
                                        "Cockcroft", "Jelliffe_adj", "Martin", "Mayo_adj"),
                             labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186",
                                        "Cockcroft-Gault", "Jelliffe", "Martin", "Mayo"))) %>%
    mutate_at(.vars = vars(median_fitted, starts_with("RMSE"), starts_with("IQR"),
                           starts_with("median")),
              funs(round(., 2))) %>%
    mutate_at(.vars = vars(starts_with("P20")),
              funs(round(., 3))) %>%
    rename("median fitted value" = median_fitted) %>%
    mutate(RMSE = paste0(RMSE, " (", RMSE_lwr, ", ", RMSE_upr, ")")) %>%
    mutate(median = paste0(median, " (", median_lwr, ", ", median_upr, ")")) %>%
    mutate(IQR = paste0(IQR, " (", IQR_lwr, ", ", IQR_upr, ")")) %>%
    mutate("P20 dose" = paste0(P20_dose, " (", P20_dose_lwr, ", ", P20_dose_upr, ")")) %>%
    select(-RMSE_lwr, -RMSE_upr, -median_lwr, -median_upr, -IQR_lwr, -IQR_upr, -P20_dose_lwr,
           -P20_dose_upr, -P20_dose)
}

