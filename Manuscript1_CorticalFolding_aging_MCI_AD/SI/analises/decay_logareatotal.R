# taxa de caimento da girificacao (log k teorico)----

decay_logTotalArea <- filter(dados, Diagnostic == "CONTROLE") %>% droplevels() %>%
  group_by(ROI) %>%
  do(fit_decay_logTotalArea = rlm(logTotalArea_corrected ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_logTotalArea_Coef = tidy(decay_logTotalArea,
                               fit_decay_logTotalArea,
                               conf.int = TRUE,
                               conf.level = 0.95)
decay_logTotalArea_Coef <- filter(decay_logTotalArea_Coef, term == "Age")
# lambda, b e Ko CTL ----

lambda_logAt_CTL_hemi_YW <-
  decay_logTotalArea_Coef$estimate[decay_logTotalArea_Coef$ROI == "hemisphere"]

lambda_logAt_CTL_F_YW <-
  decay_logTotalArea_Coef$estimate[decay_logTotalArea_Coef$ROI == "F"]

lambda_logAt_CTL_O_YW <-
  decay_logTotalArea_Coef$estimate[decay_logTotalArea_Coef$ROI == "O"]


lambda_logAt_CTL_P_YW <-
  decay_logTotalArea_Coef$estimate[decay_logTotalArea_Coef$ROI == "P"]

lambda_logAt_CTL_T_YW <-
  decay_logTotalArea_Coef$estimate[decay_logTotalArea_Coef$ROI == "T"]
