# CORRIGINDO OS PARAMETROS PELA IDADE ----

summary(lm(logAvgThickness ~ sex, data = filter(dados_hemi_v1_CH, Diagnostic == "CTL")))

dados_ses1_FS <-
  filter(dados,
         method == "FreeSurferStandard",
         Longitudinal_correction == "yes") %>% droplevels() %>% mutate(
           logAvgThickness_age_decay = as.numeric(
             ifelse(
               ROI == "hemisphere",
               logAvgThickness - b_t_CTL_hemi_FS - lambda_t_CTL_hemi_FS * Age,
               ""
             )
           ),
           logTotalArea_age_decay = as.numeric(
             ifelse(
               ROI == "hemisphere",
               logTotalArea - b_logAt_CTL_hemi_FS - lambda_logAt_CTL_hemi_FS * Age,
               ""
             )
           ),
           logExposedArea_age_decay = as.numeric(
             ifelse(
               ROI == "hemisphere",
               logExposedArea - b_Ae_CTL_hemi_FS - lambda_Ae_CTL_hemi_FS * Age,
               ""
             )
           )
         )



dados_ses1_YW <- unique(
  filter(dados,
         method == "Yujiang_script",
         Longitudinal_correction == "yes") %>% droplevels() %>% mutate(
           logAvgThickness_age_decay = as.numeric(
             ifelse(
               ROI == "hemisphere",
               logAvgThickness - b_t_CTL_hemi_YW - lambda_t_CTL_hemi_YW * Age,
               ifelse(
                 ROI == "F",
                 logAvgThickness - b_t_CTL_F_YW - lambda_t_CTL_F_YW * Age,
                 ifelse(
                   ROI == "O",
                   logAvgThickness - b_t_CTL_O_YW - lambda_t_CTL_O_YW * Age,
                   ifelse(
                     ROI == "P",
                     logAvgThickness - b_t_CTL_P_YW - lambda_t_CTL_P_YW * Age,
                     ifelse(
                       ROI == "T",
                       logAvgThickness - b_t_CTL_T_YW - lambda_t_CTL_T_YW * Age,
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
               logTotalArea - b_logAt_CTL_hemi_YW - lambda_logAt_CTL_hemi_YW * Age,
               ifelse(
                 ROI == "F",
                 logTotalArea_corrected - b_logAt_CTL_F_YW - lambda_logAt_CTL_F_YW * Age,
                 ifelse(
                   ROI == "O",
                   logTotalArea_corrected - b_logAt_CTL_O_YW - lambda_logAt_CTL_O_YW * Age,
                   ifelse(
                     ROI == "P",
                     logTotalArea_corrected - b_logAt_CTL_P_YW - lambda_logAt_CTL_P_YW * Age,
                     ifelse(
                       ROI == "T",
                       logTotalArea_corrected - b_logAt_CTL_T_YW - lambda_logAt_CTL_T_YW * Age,
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
               logExposedArea - b_Ae_CTL_hemi_YW - lambda_Ae_CTL_hemi_YW * Age,
               ifelse(
                 ROI == "F",
                 logExposedArea_corrected - b_Ae_CTL_F_YW - lambda_Ae_CTL_F_YW * Age,
                 ifelse(
                   ROI == "O",
                   logExposedArea_corrected - b_Ae_CTL_O_YW - lambda_Ae_CTL_O_YW * Age,
                   ifelse(
                     ROI == "P",
                     logExposedArea_corrected - b_Ae_CTL_P_YW - lambda_Ae_CTL_P_YW * Age,
                     ifelse(
                       ROI == "T",
                       logExposedArea_corrected - b_Ae_CTL_T_YW - lambda_Ae_CTL_T_YW * Age,
                       ""
                     )
                   )
                 )
               )
             )
           )
         ))

#, logConvexHullArea_age_decay = as.numeric(ifelse(ROI == "hemisphere", logConvexHullArea - b_logCH_CTL_hemi - lambda_logCH_CTL_hemi * Age, ""  ))

dados <- full_join(dados, dados_ses1_FS)
dados <- full_join(dados, dados_ses1_YW)

# K ----

dados <- dados %>%
  mutate(K_age_decay = 
  logTotalArea_age_decay + 1/2 * logAvgThickness_age_decay - 5/4 * logExposedArea_age_decay,
  I_age_decay = logTotalArea_age_decay + logExposedArea_age_decay + logAvgThickness_age_decay^2,
  S_age_decay = 3/2 * logTotalArea_age_decay + 3/4 * logExposedArea_age_decay - 9/4*logAvgThickness_age_decay^2)

#dados_ses1_YW <- dados_ses1_YW %>% mutate(K_age_decay = (logTotalArea_age_decay + (1 / 2) * logAvgThickness_age_decay - ((5 / 4) * logExposedArea_age_decay)))

#dados_hemi_v1_CH <- unique(filter(dados, ROI == "hemisphere", method == "Yujiang_script", Longitudinal_correction == "yes"))