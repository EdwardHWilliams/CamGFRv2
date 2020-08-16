# Function for published formule

################################################################################

Addenbrooks_sqrt_model <- readRDS("Addenbrooks_sqrt_model.rds")

################################################################################

CKD_model <- function(Sex, Creat, Age){
  ifelse(Sex == "F", 
         141 * min(Creat/0.7, 1)^(-0.329) * max(Creat/0.7, 1)^(-1.209) * 0.993^Age * 1.018, 
         141 * min(Creat/0.9, 1)^(-0.411) * max(Creat/0.9, 1)^(-1.209) * 0.993^Age)
}
CKD_model <- Vectorize(CKD_model)

# 
# CKD_model_free = function(a, b_F, b_M, c_F, c_M, d, e, f, Sex, Creat, Age){
#   # the CKD-EPI model with all parsamaters as variables
#     if(Sex == "F"){
#       log_GFR  <- log(a) + c_F*log(min(Creat/b_F,1)) + 
#         d*log(max(Creat/b_F,1)) + Age*log(e) + log(f)
#     }
#     else if(Sex == "M"){
#       log_GFR  <- log(a) + c_M*log(min(Creat/b_M,1)) + 
#         d*log(max(Creat/b_M,1)) + Age*log(e)
#     }
#     else{
#       stop("Sex must be F or M")
#     }
#   GFR = exp(log_GFR)
#   return(GFR)
# }  
# CKD_model_free = Vectorize(CKD_model_free)
# CKD_model = function(Sex, Creat, Age){
#   CKD_model_free(a=141, b_F=0.7, b_M=0.9, c_F=-0.329, c_M=-0.411, d=-1.209, e=0.993,
#             f=1.018, Sex=Sex, Creat=Creat, Age=Age)
# }


CKD_adj_model = function(Sex, Creat, Age, BSA){
  CKD_model(Sex, Creat, Age)*(BSA/1.73)
}

CKD_adj_ethnicity <- function(Sex, Creat, Age, BSA, Black){
  CKD_adj_model(Sex, Creat, Age, BSA)*
    (1 + 0.159*(ifelse(Black != T | is.na(Black), 0, 1)))
}


################################################################################

Cockcroft_equation <- function(Age, Weight, Sex, Scr){
  GFR <-  (140-Age)*Weight*(1-0.15*(Sex=="F"))*(1/(Scr*72))
}
Cockcroft_equation <- Vectorize(Cockcroft_equation)

################################################################################

MDRD_equation <- function(Age, Creat, Sex, a=186, b=-1.154, c=-0.203, d=0.742){
  GFR <- a * (Creat^b) * (Age^c) * (1 - (1-0.742)*(Sex == "F"))
  return(GFR)
}


MDRD_equation_corrected <- function(Age, Creat, Sex, BSA, a=186, b=-1.154, c=-0.203, d=0.742){
  GFR <- a * (Creat^b) * (Age^c) * (1 - (1-0.742)*(Sex == "F"))
  GFR <- GFR*(BSA/1.73)
  return(GFR)
}

MDRD_equation_IDMS <- function(Age, Creat, Sex){
  MDRD_equation(Age, Creat, Sex, a = 175)
}

MDRD_adj_equation_IDMS <- function(Age, Creat, Sex, BSA){
  MDRD_equation_corrected(Age, Creat, Sex, BSA, a = 175)
}

MDRD_186_ethnicity <- function(Age, Creat, Sex, BSA, Black){
  MDRD_equation_corrected(Age, Creat, Sex, BSA)*
    (1 + 0.212*(ifelse(Black != T | is.na(Black), 0, 1)))
}

MDRD_175_ethnicity <- function(Age, Creat, Sex, BSA, Black){
  MDRD_adj_equation_IDMS(Age, Creat, Sex, BSA)*
    (1 + 0.212*(ifelse(Black != T | is.na(Black), 0, 1)))
}

MDRD_6_var <- function(Age, Creat, Sex, BSA, Urea, Alb){
  GFR <- 170*Creat^(-0.999) * Age^(-0.176) * Urea^(-0.17) * Alb^(0.318) * (1 - (1-0.762)*(Sex == "F"))
  GFR <- GFR*(BSA/1.73)
}


################################################################################

Jelliffe_equation <- function(Age, Creat, Sex){
  GFR <- (98-0.8*(Age-20))*(1-0.1*(Sex=="F"))*(1/Creat)
  return(GFR)
}
Jelliffe_equation <- Vectorize(Jelliffe_equation)

Jelliffe_adj_equation <- function(Age, Creat, Sex, BSA){
  GFR <- (98-0.8*(Age-20))*(1-0.1*(Sex=="F"))*(BSA/1.73)*(1/Creat)
  return(GFR)
}
Jelliffe_adj_equation <- Vectorize(Jelliffe_adj_equation)

################################################################################

Wright_equation <- function(Age, Creat, Sex, BSA){
  GFR <- (6580-38.8*Age)*(1-0.168*(Sex=="F"))*BSA*(1/(Creat*88.42))
  return(GFR)
}
Wright_equation <- Vectorize(Wright_equation)

################################################################################

Mayo_equation <- function(Age, Creat, Sex){
  if(Creat < 0.8){
    Creat = 0.8
  }
  Creat <- 1/Creat
  GFR <- exp(1.911 + 5.249*Creat - 2.114*Creat^2 -0.00686*Age - 0.205*(Sex=="F"))
  return(GFR)
}
Mayo_equation <- Vectorize(Mayo_equation)

Mayo_adj_equation <- function(Age, Creat, Sex, BSA){
  if(Creat < 0.8){
    Creat = 0.8
  }
  Creat <- 1/Creat
  GFR <- exp(1.911 + 5.249*Creat - 2.114*Creat^2 -0.00686*Age - 0.205*(Sex=="F"))
  GFR <- GFR*(BSA/1.73)
  return(GFR)
}
Mayo_adj_equation <- Vectorize(Mayo_adj_equation)

################################################################################

Martin_equation <- function(Age, Creat, Sex, Wt){
  Creat = Creat*88.4
  GFR = 163*Wt*(1-0.00496*Age)*(1-0.252*(Sex=="F"))/Creat
  return(GFR)
}

################################################################################

LM_revised_equation <- function(Age, Creat, Sex){
  Creat_mol <- Creat*88.4
  if(Sex=="F"){
    if(Creat_mol<150){
      X = 2.5 + 0.0121*(150-Creat_mol)
    } else if (Creat_mol >=150){
      X = 2.5-0.926*log(Creat_mol/150)
    }
  } else if (Sex=="M"){
    if(Creat_mol<150){
      X = 2.56 + 0.00968*(180-Creat_mol)
    } else if (Creat_mol >=150){
      X = 2.56-0.926*log(Creat_mol/180)
    }
  }
  GFR = exp(X-0.0158*Age+0.438*log(Age))
  return(GFR)
}
LM_revised_equation <- Vectorize(LM_revised_equation)

LM_revised_adj_equation <- function(Age, Creat, Sex, BSA){
  LM_revised_equation(Age, Creat, Sex)*(BSA/1.73)
}
LM_revised_adj_equation <- Vectorize(LM_revised_adj_equation)  

LM_revised_adj_data <- function(data){
  LM_revised_adj_equation(data$Age, data$Creat, data$Sex, BSA = data$SufA)
}


################################################################################

# Schwartz equation

Schwartz <- function(Creat, Ht, BSA, BSA_adjust = T){
  GFR = 0.413*Ht/Creat
  GFR*(1-BSA_adjust) + GFR*(BSA_adjust)*BSA/1.73
}

Counahan <- function(Creat, Ht, BSA, BSA_adjust = T){
  GFR = 0.43*Ht/Creat
  GFR*(1-BSA_adjust) + GFR*(BSA_adjust)*BSA/1.73
}

Langer <- function(Creat, Ht, Wt, BSA, BSA_adjust = T){
  GFR = 0.641*Wt/Creat + 16.063*(Ht/100)^2/Creat
  GFR*(1-BSA_adjust) + GFR*(BSA_adjust)*BSA/1.73
}


Schwartz_urea <- function(Creat, Ht, BSA, Urea, BSA_adjust = T){
  GFR = 40.7*((Ht/100)/Creat)^0.640 * (30/Urea*2.8)^0.202
  GFR*(1-BSA_adjust) + GFR*(BSA_adjust)*BSA/1.73
}



Schwartz_power <- function(Creat, Ht, BSA, BSA_adjust = T){
  GFR = 41.2*((Ht/100)/Creat)^0.775
  GFR*(1-BSA_adjust) + GFR*(BSA_adjust)*BSA/1.73
}




################################################################################
# FAS equaiton

FAS <- function(Age, Creat, Sex){
  Q_table = data.frame(
    Age = rep(1:20, 2),
    Sex = rep(c("M", "F"), each = 20),
    Q = c(0.26, 0.29, 0.31, 0.34, 0.38, 0.41, 0.44, 0.46, 0.49, 0.51, 
              0.53, 0.57, 0.59, 0.61, 0.72, 0.78, 0.82, 0.85, 0.88, 0.9, 
              0.26, 0.29, 0.31, 0.34, 0.38, 0.41, 0.44, 0.46, 0.49, 0.51, 
              0.53, 0.57, 0.59, 0.61, 0.64, 0.67, 0.69, 0.69, 0.7, 0.7)
  )
  Q_func <- function(Age, Sex, Q_table){
    Age = ifelse(Age > 20, 20, round(Age))
    Q_table$Q[Q_table$Age == Age & Q_table$Sex == Sex]
  }
  Q = Q_func(Age, Sex, Q_table)
  GFR = 107.3/(Creat/Q)
  GFR = ifelse(Age <= 40, GFR, GFR*0.988^(Age-40))
  return(GFR)
}
FAS <- Vectorize(FAS)

FAS_adj <- function(Age, Creat, Sex, BSA){
  FAS(Age, Creat, Sex)*(BSA/1.73)
}


################################################################################
# Giglio

Giglio <- function(Age, Creat, Sex, Ht, Wt, NSAID = 1){
  BMI = Wt/((Ht/100)^2)
  GFR = 2.36 - 0.33*log10(Age) - 0.78*log10(Creat*88.4) +  0.87*log10(Wt) - 
    0.03*NSAID*log10(Creat*88.4) + (Sex == "F")*(0.05*NSAID - 0.003*BMI)
  return(10^GFR)
}


Giglio_adj <-  function(Age, Creat, Sex,Ht, Wt, BSA, NSAID = 1){
  Giglio(Age, Creat, Sex,  Ht, Wt, NSAID = NSAID)*(BSA/1.73)
}
