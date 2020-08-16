source('../Original_regression/Functions_for_step_cross_selection_lo1_lon.R',
       echo=FALSE)
source("../Original_regression/Table_list_glm_function.R", echo=FALSE)

load("2_null_full_models.RData")

rmse <- function(x, y)
  sqrt(mean( (x - y)^2, na.rm = TRUE))


n = 5000
models <- list()
set.seed(1234)

for(i in 1:n){
  
  print(i)
  
  if((i -1) %% 100 == 0){
    
    models[[i]] <-
      RMSE_step(null, direction = "both", scope=list(lower=null, upper=full),
              seed=sample(1:1000000, 1), frac_improv = 0.999 , trace=0, CV = "lon")

  } else{
    models[[i]] <-
      RMSE_step(models[[i-1]], direction = "both", scope=list(lower=null, upper=full),
                seed= sample(1:1000000, 1), frac_improv = 0.999 , trace=0, CV = "lon")
  }
  
  if(i %% 100 == 0){
    table_models = Table_list_glm(models)  
    save(table_models, file = "2_table_models_999_thesis.RData")
    
    print(paste0("SAVED ", i, " MODELS"))
  }
  
}
#


