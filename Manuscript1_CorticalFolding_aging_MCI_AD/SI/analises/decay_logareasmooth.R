
decay_logExposedArea <- filter(dados, Diagnostic == "CONTROLE") %>% droplevels() %>%
  group_by(ROI) %>%
  do(fit_decay_logExposedArea = rlm(logExposedArea_corrected ~ Age, data = .))


# get the coefficients por intervalo de idade ----
decay_logExposedArea_Coef = tidy(decay_logExposedArea,
                                 fit_decay_logExposedArea)
decay_logExposedArea_Coef <- filter(decay_logExposedArea_Coef, term == "Age")
# lambda, b e Ko CTL ----

lambda_Ae_CTL_hemi_YW <-
  decay_logExposedArea_Coef$estimate[decay_logExposedArea_Coef$ROI == "hemisphere"]

lambda_Ae_CTL_F_YW <-
  decay_logExposedArea_Coef$estimate[decay_logExposedArea_Coef$ROI == "F"]

lambda_Ae_CTL_O_YW <-
  decay_logExposedArea_Coef$estimate[decay_logExposedArea_Coef$ROI == "O"]

lambda_Ae_CTL_P_YW <-
  decay_logExposedArea_Coef$estimate[decay_logExposedArea_Coef$ROI == "P"]

lambda_Ae_CTL_T_YW <-
  decay_logExposedArea_Coef$estimate[decay_logExposedArea_Coef$ROI == "T"]
