source('../Original_regression/Functions_for_step_cross_selection_lo1_lon.R', echo=FALSE)

load("2_null_full_models.RData")

rmse <- function(x, y)
  sqrt(mean( (x - y)^2, na.rm = TRUE))


Leave_out_one_model <- RMSE_step(null, direction = "both",
                           scope=list(lower=null, upper=full),
                           frac_improv = 0.999 , trace=1, CV = "lo1")

save(Leave_out_one_model, file = "2_lo1_model_999_thesis.RData")
#