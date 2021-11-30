# taxa de caimento da girificacao (log k teorico)----

decay_logTotalArea <- filter(dados, Session == "1", Diagnostic == "CONTROLE") %>% droplevels() %>%
  group_by(ROI) %>%
  do(fit_decay_logTotalArea = tidy(rlm(logTotalArea_corrected ~ Age, data = .),conf.int=TRUE)) %>% unnest(cols = c(fit_decay_logTotalArea))

# get the coefficients por intervalo de idade ----

decay_logTotalArea <- filter(decay_logTotalArea, term == "Age")
# lambda, b e Ko CTL ----

lambda_logAt_CTL_hemi_YW <-
  decay_logTotalArea$estimate[decay_logTotalArea$ROI == "hemisphere"]

lambda_logAt_CTL_F_YW <-
  decay_logTotalArea$estimate[decay_logTotalArea$ROI == "F"]

lambda_logAt_CTL_O_YW <-
  decay_logTotalArea$estimate[decay_logTotalArea$ROI == "O"]

lambda_logAt_CTL_P_YW <-
  decay_logTotalArea$estimate[decay_logTotalArea$ROI == "P"]

lambda_logAt_CTL_T_YW <-
  decay_logTotalArea$estimate[decay_logTotalArea$ROI == "T"]
