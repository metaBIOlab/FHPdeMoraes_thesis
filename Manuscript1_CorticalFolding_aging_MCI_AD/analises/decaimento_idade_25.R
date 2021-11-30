# CORRIGINDO OS PARAMETROS PELA IDADE ----

source("analises/decay_thickness.R")

source("analises/decay_logareatotal.R")

source("analises/decay_logareasmooth.R")

dados_ses1 <- unique(
  filter(dados, Session == "1") %>% droplevels() %>% mutate(
           logAvgThickness_age_decay = as.numeric(
             ifelse(
               ROI == "hemisphere",
               logAvgThickness - lambda_t_CTL_hemi_YW * (Age - Age.cor),
               ifelse(
                 ROI == "F",
                 logAvgThickness - lambda_t_CTL_F_YW * (Age - Age.cor),
                 ifelse(
                   ROI == "O",
                   logAvgThickness - lambda_t_CTL_O_YW * (Age - Age.cor),
                   ifelse(
                     ROI == "P",
                     logAvgThickness - lambda_t_CTL_P_YW * (Age - Age.cor),
                     ifelse(
                       ROI == "T",
                       logAvgThickness - lambda_t_CTL_T_YW * (Age - Age.cor),
                       ""
                     )
                   )
                 )
               )
             )
           ),
           logTotalArea_age_decay = as.numeric(
             ifelse(
               ROI == "hemisphere",
               logTotalArea - lambda_logAt_CTL_hemi_YW * (Age - Age.cor),
               ifelse(
                 ROI == "F",
                 logTotalArea_corrected - lambda_logAt_CTL_F_YW * (Age - Age.cor),
                 ifelse(
                   ROI == "O",
                   logTotalArea_corrected - lambda_logAt_CTL_O_YW * (Age - Age.cor),
                   ifelse(
                     ROI == "P",
                     logTotalArea_corrected - lambda_logAt_CTL_P_YW * (Age - Age.cor),
                     ifelse(
                       ROI == "T",
                       logTotalArea_corrected - lambda_logAt_CTL_T_YW * (Age - Age.cor),
                       ""
                     )
                   )
                 )
               )
             )
           ),
           logExposedArea_age_decay = as.numeric(
             ifelse(
               ROI == "hemisphere",
               logExposedArea - lambda_Ae_CTL_hemi_YW * (Age - Age.cor),
               ifelse(
                 ROI == "F",
                 logExposedArea_corrected - lambda_Ae_CTL_F_YW * (Age - Age.cor),
                 ifelse(
                   ROI == "O",
                   logExposedArea_corrected - lambda_Ae_CTL_O_YW * (Age - Age.cor),
                   ifelse(
                     ROI == "P",
                     logExposedArea_corrected - lambda_Ae_CTL_P_YW * (Age - Age.cor),
                     ifelse(
                       ROI == "T",
                       logExposedArea_corrected - lambda_Ae_CTL_T_YW * (Age - Age.cor),
                       ""
                     )
                   )
                 )
               )
             )
           )
         ))

dados <- full_join(dados, dados_ses1)

# K ----

dados <- dados %>%
  mutate(
    K_age_decay = logTotalArea_age_decay + 1/2 * logAvgThickness_age_decay - 5/4 * logExposedArea_age_decay,
    I_age_decay = logTotalArea_age_decay + logExposedArea_age_decay + logAvgThickness_age_decay^2,
    S_age_decay = 3/2 * logTotalArea_age_decay + 3/4 * logExposedArea_age_decay - 9/4*logAvgThickness_age_decay^2,
    Knorm_age_decay = K_age_decay/sqrt(1 + (1/4)^2 + (5/4)^2),
    Snorm_age_decay = S_age_decay/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
    Inorm_age_decay = I_age_decay/sqrt(1^2 + 1^2 + 1^2))

