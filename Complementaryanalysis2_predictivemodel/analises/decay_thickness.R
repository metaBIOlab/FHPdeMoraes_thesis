# taxa de caimento da girificacao (espessura)----

decay_AvgThickness <-
  filter(dados, Session == "1", Diagnostic == "CONTROLE") %>% droplevels() %>%
  group_by(ROI) %>%
  do(fit_decay_AvgThickness = tidy(rlm(logAvgThickness ~ Age, data = .), conf.int=TRUE)) %>% unnest(cols = c(fit_decay_AvgThickness))

# get the coefficients por intervalo de idade ----

decay_AvgThickness <- filter(decay_AvgThickness,term == "Age")
# lambda, b e Ko CTL - sem correcao longitudinal ----

lambda_t_CTL_hemi_YW <-
  decay_AvgThickness$estimate[decay_AvgThickness$ROI == "hemisphere"]

lambda_t_CTL_F_YW <-
  decay_AvgThickness$estimate[decay_AvgThickness$ROI == "F"]

lambda_t_CTL_O_YW <-
  decay_AvgThickness$estimate[decay_AvgThickness$ROI == "O"]

lambda_t_CTL_P_YW <-
  decay_AvgThickness$estimate[decay_AvgThickness$ROI == "P"]

lambda_t_CTL_T_YW <-
  decay_AvgThickness$estimate[decay_AvgThickness$ROI == "T"]
