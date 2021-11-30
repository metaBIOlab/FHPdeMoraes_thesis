
decay_logExposedArea <- filter(dados, Session == "1", Diagnostic == "CONTROLE") %>% droplevels() %>%
  group_by(ROI) %>%
  do(fit_decay_logExposedArea = tidy(rlm(logExposedArea_corrected ~ Age, data = .), conf.int = TRUE))  %>% unnest(cols = c(fit_decay_logExposedArea))

# get the coefficients por intervalo de idade ----

decay_logExposedArea <- filter(decay_logExposedArea, term == "Age")
# lambda, b e Ko CTL ----

lambda_Ae_CTL_hemi_YW <-
  decay_logExposedArea$estimate[decay_logExposedArea$ROI == "hemisphere"]

lambda_Ae_CTL_F_YW <-
  decay_logExposedArea$estimate[decay_logExposedArea$ROI == "F"]

lambda_Ae_CTL_O_YW <-
  decay_logExposedArea$estimate[decay_logExposedArea$ROI == "O"]

lambda_Ae_CTL_P_YW <-
  decay_logExposedArea$estimate[decay_logExposedArea$ROI == "P"]

lambda_Ae_CTL_T_YW <-
  decay_logExposedArea$estimate[decay_logExposedArea$ROI == "T"]
