rmse <- function(x, y)
  sqrt(mean( (x - y)^2, na.rm = TRUE))

rmse_CI <- function(x, y, alpha = 0.05){
  RMSE <- rmse(x, y)
  n <- length(x)
  chi1 <- qchisq(1-alpha/2, n)
  chi2 <- qchisq(alpha/2, n)
  low.CI <- sqrt(n/chi1)*RMSE
  high.CI <- sqrt(n/chi2)*RMSE
  return(c(low.CI, high.CI))
}


Within_P30 <- function(actual, fitted, p=30){
  fitted<=actual*(1+p/100) & fitted>=actual*(1-p/100)
}
Within_P30 <- Vectorize(Within_P30)
P30 <- function(actual, fitted, p=30){
  mean(Within_P30(actual, fitted, p=p))
}

Dose_equ <- function(GFR, AUC=5){
  AUC*(GFR+25)}

rmse_median_IQR <- function(actual, estimate, medCI=F, restriction=NULL){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  r <- actual - estimate
  RMSE <- sqrt(mean((r)^2))
  Median <- median(r)
  IQR <- IQR(r)
  pe <- median(r/actual *100)
  ape <- median(abs(r/actual *100))
  # P30 <- P30(actual, estimate)
  if(medCI==T){
    n <- length(actual)
    q <- round(0.5*n + c(-1,1)*1.96*sqrt(n*0.25))
    medianCI <- sort(r)[q]
    medianCI <- c("lower value" = medianCI[1], "upper value" = medianCI[2])
    result <- list(c("RMSE" =  RMSE, "IQR of resid" = IQR, 
                     "Median resid" = Median),
                   "Median 95% CI" = medianCI)
  }
  else{
    result <- c("RMSE" = RMSE, "IQR of residuals" = IQR, MAPE =ape, 
                "Median residual" = Median, MPE = pe
                #, "P30" = P30
                )
  }
  return(result)
}

ape <- function(y, yhat){
  abs((y-yhat)/y*100)
}
  

APE <- function(actual, estimate, restriction=NULL, round.dig = 2, Dose =F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  ape.50 <- sprintf(paste0("%.", round.dig, "f"), round(quantile(ape(actual, estimate), .5), round.dig))
  ape.25 <- sprintf(paste0("%.", round.dig, "f"), round(quantile(ape(actual, estimate), .25), round.dig))
  ape.75 <- sprintf(paste0("%.", round.dig, "f"), round(quantile(ape(actual, estimate), .75), round.dig))
  return(APE = paste0(ape.50, " (", ape.25, ", ", ape.75, ")"))
}



PE <- function(actual, estimate, restriction=NULL, round.dig = 2, Dose =F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  r <- actual - estimate
  ape.50 <- sprintf(paste0("%.", round.dig, "f"), round(quantile((r/actual *100), .5), round.dig))
  ape.25 <- sprintf(paste0("%.", round.dig, "f"), round(quantile((r/actual *100), .25), round.dig))
  ape.75 <- sprintf(paste0("%.", round.dig, "f"), round(quantile((r/actual *100), .75), round.dig))
  return(PE = paste0(ape.50, " (", ape.25, ", ", ape.75, ")"))
}


Resid_med <- function(actual, estimate, restriction=NULL, round.dig = 2, Dose=F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  r <- actual - estimate
  ape.50 <- sprintf(paste0("%.", round.dig, "f"), round(quantile(r, .5), round.dig))
  ape.25 <- sprintf(paste0("%.", round.dig, "f"), round(quantile(r, .25), round.dig))
  ape.75 <- sprintf(paste0("%.", round.dig, "f"), round(quantile(r, .75), round.dig))
  return(Residual = paste0(ape.50, " (", ape.25, ", ", ape.75, ")"))
}

Resid_med_numeric <- function(actual, estimate, restriction=NULL, round.dig = 2, Dose=F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  r <- actual - estimate
  ape.50 <- quantile(r, .5)
  return(Residual_median = ape.50)
}

Resid_med_CI <- function(actual, estimate, restriction=NULL, round.dig = 2, Dose=F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  r <- actual - estimate
  med <- quantile(r, .5)
  n <- length(r)
  res_lwr <- med - 1.814*IQR(r)/sqrt(n)
  res_upr <- med + 1.814*IQR(r)/sqrt(n)
#   res_lwr <- sort(r)[n/2-1.96*sqrt(n)/2]   
#   res_upr <- sort(r)[1+n/2+1.96*sqrt(n)/2]   
  
  ape.50 <- sprintf(paste0("%.", round.dig, "f"), round(med, round.dig))
  ape.25 <- sprintf(paste0("%.", round.dig, "f"), round(res_lwr, round.dig))
  ape.75 <- sprintf(paste0("%.", round.dig, "f"), round(res_upr, round.dig))
  return(Residual = paste0(ape.50, " (", ape.25, ", ", ape.75, ")"))
}


RMSE <- function(actual, estimate, restriction=NULL, round.dig = 3, Dose=F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  rmse = sprintf(paste0("%.", round.dig, "f"), round(rmse(actual, estimate), round.dig))
  rmse.low <- sprintf(paste0("%.", round.dig, "f"), round(rmse_CI(actual, estimate, alpha = 0.05)[1], round.dig))
  rmse.high <- sprintf(paste0("%.", round.dig, "f"), round(rmse_CI(actual, estimate, alpha = 0.05)[2], round.dig))
  return(rmse = paste0(rmse, " (", rmse.low, ", ", rmse.high, ")"))
}


RMSE_numeric <- function(actual, estimate, restriction=NULL, round.dig = 3, Dose=F, CI = F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  rmse = rmse(actual, estimate)
  if(CI == T){
    rmse.low <- rmse_CI(actual, estimate, alpha = 0.05)[1]
    rmse.high <- rmse_CI(actual, estimate, alpha = 0.05)[2]
    return(c(rmse, rmse.low, rmse.high))
  } else {
    return(rmse)
  }

}


Median_estimate <- function(actual, estimate, restriction=NULL, round.dig = 2, Dose=F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  med = sprintf(paste0("%.", round.dig, "f"), round(quantile(estimate, .5), round.dig))
  Q1 <- sprintf(paste0("%.", round.dig, "f"), round(quantile(estimate, .25), round.dig))
  Q3 <- sprintf(paste0("%.", round.dig, "f"), round(quantile(estimate, .75), round.dig))
  return(rmse = paste0(med, " (", Q1, ", ", Q3, ")"))
}


resid_IQR <- function(actual, estimate, restriction=NULL, round.dig = 2, Dose=F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  r <- actual - estimate
  iqr <- sprintf(paste0("%.", round.dig, "f"), round(IQR(r), round.dig))
  return(IQR = iqr)
}


APE_under20 <- function(actual, estimate, restriction=NULL, round.dig = 2, Dose=F){
  if(!is.null(restriction)){
    actual <- actual[restriction]
    estimate <- estimate[restriction]
  }
  if(Dose ==T){
    estimate = Dose_equ(estimate)
    actual = Dose_equ(actual)
  }
  r <- actual - estimate
  ape <- abs(r/actual *100)
  ape_more20 <- sprintf(paste0("%.", round.dig, "f"), round(sum(ape >20)/length(ape)*100, round.dig))
  return(APE_more20 = ape_more20)
}






