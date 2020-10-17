# Bootstrap CI functions 

bootstrap_func <- function(x, index){
  return(c(med = median(x[index]), 
           IQR = IQR(x[index]), 
           RMSE = sqrt(mean(x[index]^2))))
}

boot_normal_ci <- function(t, t0, alpha = 0.05){
  (2*t0 - mean(t)) + c(-1, 1)*qnorm(1 - alpha/2)*sd(t)
}

boot_normal_ci_matrix <- function(t, t0, alpha = 0.05){
  mapply(boot_normal_ci, 
         t = split(t, col(as.matrix(t))), 
         t0 = split(t0, 1:length(t0)), 
         alpha = alpha)
}

boot_normal_func <- function(x, func, R = 2000, alpha = 0.05){
  require(boot)
  boot_result = boot(x, function(x, index) func(x[index]), R = R)
  boot_normal_ci(boot_result$t, boot_result$t0, alpha = alpha)
}

RMSE_resid <- function(x){
  sqrt(mean(x^2))
}

P20_opp <- function(x, p = 20){
  mean(x < 1-p/100 | x > 1 + p/100)
}
 

generate_boot_index <- function(n, R, seed){
  set.seed(seed)
  plyr::rlply(R, sample(1:n, replace = TRUE))
}




# calculated bootstrap 95% confidence interval based on the normal distribution assumption
my_normal_boot = function(x, R=2000, func = median, name = "statistic", alpha = 0.05, seed =1234) {
  
  set.seed(seed)
  
  # Bootstrap 95% CI
  # t = replicate(R, func(sample(x, replace=TRUE)))
  n = length(x)
  
  sample_index = generate_boot_index(n = n, R = R, seed = seed)
  t = lapply(sample_index, function(i){ func(x[i]) }) %>%
    unlist()
  t0 = func(x)
  cis = (2*t0 - mean(t)) + c(-1, 1)*qnorm(1 - alpha/2)*sd(t)
  
  # Return data frame of results
  df = data.frame(length(x), t0, cis[1], cis[2])
  names(df) = c("n", name, paste0(name, "_lwr"), paste0(name, "_upr"))
  df
}

# Equation to calculate carboplatin dose from GFR
Dose_equ <- function(x, AUC = 5){
  AUC*(x+25)
}

# Performs bootstrap CI on all statistics apart from P20
Statistic_summary <- function(df, R = 2000){
  df_IQR = df %>%   
    do(my_normal_boot(.$resid, func = IQR, name = "IQR", R = R)) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  df_RMSE = df %>%   
    do(my_normal_boot(.$resid, func = RMSE_resid, name = "RMSE")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  df_med = df %>%   
    do(my_normal_boot(.$resid, func = median, name = "median")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  df %>%
    dplyr::summarise(n = n(), median_fitted = median(fitted)) %>%
    bind_cols(df_RMSE, df_med, df_IQR) 
}

# Performs bootstrap CI on all 4 statistics considered
Statistic_summary_withP20 <- function(df, R = 2000){
  df_IQR = df %>%   
    do(my_normal_boot(.$resid, func = IQR, name = "IQR", R = R)) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  df_RMSE = df %>%   
    do(my_normal_boot(.$resid, func = RMSE_resid, name = "RMSE")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  df_med = df %>%   
    do(my_normal_boot(.$resid, func = median, name = "median")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  
  df_P20_dose = df %>%
    do(my_normal_boot(Dose_equ(.$fitted)/Dose_equ(.$GFR), func = P20_opp, name = "P20_dose")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  
  df %>%
    dplyr::summarise(n = n(), median_fitted = median(fitted)) %>%
    bind_cols(df_RMSE, df_med, df_IQR, df_P20_dose) 
}

Statistic_summary_withP20_both <- function(df, R = 2000){
  df_IQR = df %>%   
    do(my_normal_boot(.$resid, func = IQR, name = "IQR", R = R)) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  df_RMSE = df %>%   
    do(my_normal_boot(.$resid, func = RMSE_resid, name = "RMSE")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  df_med = df %>%   
    do(my_normal_boot(.$resid, func = median, name = "median")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  
  df_P20 = df %>%
    do(my_normal_boot(.$fitted/.$GFR, func = P20_opp, name = "P20")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  df_P20_dose = df %>%
    do(my_normal_boot(Dose_equ(.$fitted)/Dose_equ(.$GFR), func = P20_opp, name = "P20_dose")) %>%
    ungroup() %>% dplyr::select((ncol(.)-2):ncol(.))
  
  df %>%
    dplyr::summarise(n = n(), median_fitted = median(fitted)) %>%
    bind_cols(df_RMSE, df_med, df_IQR, df_P20, df_P20_dose) 
}




my_boot_pvalue = function(x1, x2, R=2000, func = median, name = "statistic", alpha = 0.05, seed =1234) {
  # method from Davison and Hinkley (1997), Bootstrap Methods and their Application, p. 141
  set.seed(seed)
  n = length(x1)
  require(plyr)
  sample_index = generate_boot_index(n = n, R = R, seed = seed)

  t = lapply(sample_index, 
             function(i){
               func(x1[i]) - func(x2[i])
             }) %>%
    unlist()
  pval = (1+sum(t >= 0))/(R+1)
  pval
}

my_boot_pvalue_multi = function(x, R=2000, func = median, name = "statistic", alpha = 0.05, seed =1234){
  
  # method from Davison and Hinkley (1997), Bootstrap Methods and their Application, p. 141
  n = nrow(x)
  sample_index = generate_boot_index(n = n, R = R, seed = seed)
  t = lapply(sample_index, 
             function(index){
               apply(x[index,], 2, func)
             }) %>%
    do.call("rbind", .)
  combinations <- combn(1:ncol(x), 2, simplify = F)
  
  diff_t <- lapply(combinations, function(i) t[,i[1]] - t[,i[2]])
  pval <- lapply(diff_t, function(t) 1- abs(2*(sum(t >= 0) + 1)/(R + 1) -1))

  res = matrix(NA, nrow = ncol(x), ncol = ncol(x))
  for(i in 1:length(combinations)){
    res[combinations[[i]][1], combinations[[i]][2]] <- pval[[i]] 
  }
  colnames(res) = rownames(res) = colnames(x)
  res
}

calculate_statistic_pvalues <- function(df, R = 2000, seed = 1234){

  x = df %>%
    dplyr::select(resid, equation) %>%
    group_by(equation) %>%
    dplyr::mutate(ID = 1:n()) %>%
    spread(value = resid, key = equation) %>%
    dplyr::select(-ID)
  
  x_P20 <- df %>% 
    mutate(per_diff =  Dose_equ(.$fitted)/Dose_equ(.$GFR)) %>%
    dplyr::select(per_diff, equation) %>%
    group_by(equation) %>%
    dplyr::mutate(ID = 1:n()) %>%
    spread(value = per_diff, key = equation) %>%
    dplyr::select(-ID)
  
  pval_IQR <- my_boot_pvalue_multi(x, func = IQR, name = "IQR", R = R)
  pval_RMSE <- my_boot_pvalue_multi(x, func = RMSE_resid, name = "RMSE", R = R)
  pval_median <- my_boot_pvalue_multi(x, func = median, name = "median", R = R)
  pval_P20 <- my_boot_pvalue_multi(x_P20, func = P20_opp, name = "P20", R = R)
  
  return(list(RMSE = pval_RMSE, median = pval_median, IQR = pval_IQR, P20 = pval_P20))
  
}
################################################################################

# new bootstrap procedure 

Bootstrap_test <- function(x1, x2, func, sample_index = NULL, R = 2000, return_all = F){
  n = length(x1)
  m = length(x2)
  
  pooled = c(x1, x2)
  
  obs = func(x1) - func(x2)
  
  if(is.null(sample_index)){
    sample_index =  plyr::rlply(R, sample(1:(n+m), replace = TRUE)) 
  }
  
  t = sapply(sample_index, function(x) func(pooled[x[1:m]]) - func(pooled[x[(m+1):(m+n)]]))
  
  p = (1 + sum(abs(t) > abs(obs))) / (R+1)
  
  if(return_all == T){
    return(m = m, n =n, func1 = func(x1), func2 = func(x2), p = p)
  } else{
    return(p)
  }
  
  
}




Bootstrap_test_multi <- function(x, func, R = 2000, seed = 1234){
  # takes data frame x (data frame with each colomn for each equation/model) and
  # the statisctic func (eg RMSE, medain residual, ...) and applys
  # Bootstrap_test on it
  set.seed(seed)
  index =  plyr::rlply(R, sample(1:(nrow(x)*2), replace = TRUE)) 
  
  combinations <- combn(1:ncol(x), 2, simplify = F)
  
  pvals = lapply(combinations, function(c){
    Bootstrap_test(x[,c[1]], x[,c[2]], 
                   sample_index = index, 
                   func = func, R = R)})
  
  a_func = function(name){
    func(x[,as.character(name)])
  }
  
  sapply(combinations, function(c) colnames(x)[c]) %>% 
    t() %>%
    as.data.frame() %>%
    `colnames<-`(c("equation1", "equation2")) %>%
    # rename(equation1 = V1, equation2 = V2) %>%
    mutate(stat1 = sapply(.$equation1, a_func)) %>% 
    mutate(stat2 = lapply(.$equation2, a_func)) %>% 
    mutate(p_value = unlist(pvals)) 
}

Bootstrap_all_stats_all_equation <- function(df, R = 2000, seed = 1234, Dose = F){
  # x must be a datframe with columns equation, resid, fitted and GFR
  # equal number of observations must be in each group
  
  m = length(unique(df$equation))
  n = table(df$equation)[1]
  
  x_resid =  df %>% select(equation, resid) %>% 
    mutate(ID = rep(1:n, m)) %>% 
    spread(key = equation, value = resid) %>% 
    select(-ID) %>%
    as.data.frame() 
  
  x_perdiff =  df %>% 
    mutate(per_diff = if(Dose == T){
      Dose_equ(.$fitted)/Dose_equ(.$GFR)
    } else {
      (.$fitted)/(.$GFR)
    }) %>% 
    select(equation, per_diff) %>% 
    mutate(ID = rep(1:n, m)) %>% 
    spread(key = equation, value = per_diff) %>% 
    select(-ID) %>%
    as.data.frame() 
  
  res_RMSE = Bootstrap_test_multi(x_resid, func = RMSE_resid, R = R, seed = seed) %>% 
    mutate(stat = "RMSE")
  res_median = Bootstrap_test_multi(x_resid, func = median, R = R, seed = seed) %>% 
    mutate(stat = "median")
  res_IQR = Bootstrap_test_multi(x_resid, func = IQR, R = R, seed = seed) %>% 
    mutate(stat = "IQR")
  res_P20 = Bootstrap_test_multi(x_perdiff, func = P20_opp, R = R, seed = seed) %>% 
    mutate(stat = "P20")
  rbind(res_RMSE, res_IQR, res_median, res_P20)
}


Bootstrap_all_stats_all_equation_uneven <- function(df,  R = 2000, seed = 1234, Dose = F){
  
}


################################################################################

bootstrap_plots <- function(results, x, group, colour, colour_values = NULL){
  p1 <- results %>% 
    ggplot(aes_string(group = group, x = x, y = "RMSE")) + 
    geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = colour), 
                  position= position_dodge(width = .5), width = 0.3, size = 1) + 
    geom_point(position= position_dodge(width = .5)) + 
    ggplot_theme() + 
    theme(legend.position = "none", 
          axis.title.x = element_blank()) + 
    scale_color_manual(values = colour_values)
  
  p2 <- results %>% 
    ggplot(aes_string(group = group, x = x, y = "median")) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = colour), 
                  position= position_dodge(width = .5), width = 0.3, size = 1) + 
    geom_point(position= position_dodge(width = .5)) + 
    ggplot_theme() + 
    theme(legend.position = "none", 
          legend.title = element_blank(),
          axis.title.x = element_blank()) + 
    scale_color_manual(values = colour_values)
  
  p3 <- results %>% 
    ggplot(aes_string(group = group, x = x, y = "IQR")) + 
    geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = colour), 
                  position= position_dodge(width = .5), width = 0.3, size = 1) + 
    geom_point(position= position_dodge(width = .5)) + 
    ggplot_theme() + 
    theme(legend.position = "bottom", 
          axis.title.x = element_blank()) + 
    scale_color_manual(values = colour_values)
  
  
  grid.draw(rbind(ggplotGrob(p2 + theme(axis.ticks.x = element_blank(), 
                                        axis.text.x = element_blank(), 
                                        axis.line.x = element_blank())), 
                  ggplotGrob(p1 + theme(axis.ticks.x = element_blank(), 
                                        axis.text.x = element_blank(), 
                                        axis.line.x = element_blank())), 
                  
                  ggplotGrob(p3), size = "last"))
}



################################################################################
# Permutation test based on paper by Hilko van der Voet 1994
################################################################################

Squared_error_permutation_test <- 
  function(x1, x2, sample_index = NULL, R = 2000, return_all = F){
  
  d = x1 - x2
  t_star = mean(d)
  t_star
  
  if(is.null(sample_index)){
    sample_index =  plyr::rlply(R, if_else(runif(d) < 0.5, -1, 1)) 
  }
  
  t = sapply(sample_index, function(x) mean(d*x))
  

  p = (1 + sum(abs(t_star) < abs(t))) / (R+1)
  
  if(return_all == T){
    return(m = m, n =n, func1 = func(x1), func2 = func(x2), p = p)
  } else{
    return(p)
  }
  
  
}


Squared_error_permutation_test_all_equations <- 
  function(df, R = 10000, seed = 1234, Dose = F){
  # x must be a datframe with columns equation, resid, fitted and GFR
  # equal number of observations must be in each group
  
  e =  df %>% 
    select(PatientID, GFR_index, equation, resid) %>% 
    mutate(resid = resid^2) %>%
    pivot_wider(names_from = equation, values_from = resid) %>%
    select(-PatientID, -GFR_index) %>%
    as.data.frame() 
  
  set.seed(seed)
  index =  plyr::rlply(R, if_else(runif(nrow(e)) < 0.5, -1, 1)) 
  
  combinations <- combn(1:ncol(e), 2, simplify = F)
  
  pvals = lapply(combinations, function(c){
    Squared_error_permutation_test(
      e[,c[1]], e[,c[2]], sample_index = index, R = R
      )
    })
  
  a_func = function(name1, name2){
    mean(e[,as.character(name1)] - e[,as.character(name2)])
  }
  
  sapply(combinations, function(c) colnames(e)[c]) %>% 
    t() %>%
    as.data.frame() %>%
    `colnames<-`(c("equation1", "equation2")) %>%
    # rename(equation1 = V1, equation2 = V2) %>%
    mutate(stat = mapply(a_func, .$equation1, .$equation2)) %>% 
    mutate(p_value = unlist(pvals)) 

}






