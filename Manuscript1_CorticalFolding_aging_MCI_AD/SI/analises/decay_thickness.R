# taxa de caimento da girificacao (espessura)----

decay_AvgThickness <-
  filter(dados, Diagnostic == "CONTROLE") %>% droplevels() %>%
  group_by(ROI) %>%
  do(fit_decay_AvgThickness = rlm(logAvgThickness ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_AvgThickness_Coef = tidy(
  decay_AvgThickness,
  fit_decay_AvgThickness,
  conf.int = TRUE,
  conf.level = 0.95
)

decay_AvgThickness_Coef <- filter(decay_AvgThickness_Coef,term == "Age")
# lambda, b e Ko CTL - sem correcao longitudinal ----

lambda_t_CTL_hemi_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$ROI == "hemisphere"]

lambda_t_CTL_F_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$ROI == "F"]

lambda_t_CTL_O_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$ROI == "O"]

lambda_t_CTL_P_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$ROI == "P"]

lambda_t_CTL_T_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$ROI == "T"]
