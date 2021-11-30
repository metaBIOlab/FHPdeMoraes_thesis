# taxa de caimento da girificacao (espessura)----

decay_AvgThickness <-
  filter(dados, Session == "1", method == "FreeSurferStandard", Longitudinal_correction == "yes") %>% droplevels() %>%
  group_by(Diagnostic, ROI) %>%
  do(fit_decay_AvgThickness = rlm(logAvgThickness ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_AvgThickness_Coef = tidy(
  decay_AvgThickness,
  fit_decay_AvgThickness,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda, b e Ko CTL - sem correcao longitudinal ----

lambda_t_CTL_hemi_FS <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "Age" &
                                     decay_AvgThickness_Coef$ROI == "hemisphere"]

b_t_CTL_hemi_FS <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "(Intercept)" &
                                     decay_AvgThickness_Coef$ROI == "hemisphere"]



# taxa de caimento da girificacao (espessura)----

decay_AvgThickness <-
  filter(dados, Session == "1", method == "Yujiang_script", Longitudinal_correction == "yes") %>% droplevels() %>%
  group_by(Diagnostic, ROI) %>%
  do(fit_decay_AvgThickness = rlm(logAvgThickness ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_AvgThickness_Coef = tidy(
  decay_AvgThickness,
  fit_decay_AvgThickness,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda, b e Ko CTL - sem correcao longitudinal ----

lambda_t_CTL_hemi_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "Age" &
                                     decay_AvgThickness_Coef$ROI == "hemisphere"]

lambda_t_CTL_F_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "Age" &
                                     decay_AvgThickness_Coef$ROI == "F"]

lambda_t_CTL_O_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "Age" &
                                     decay_AvgThickness_Coef$ROI == "O"]

lambda_t_CTL_P_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "Age" &
                                     decay_AvgThickness_Coef$ROI == "P"]

lambda_t_CTL_T_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "Age" &
                                     decay_AvgThickness_Coef$ROI == "T"]

b_t_CTL_hemi_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "(Intercept)" &
                                     decay_AvgThickness_Coef$ROI == "hemisphere"]

b_t_CTL_F_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "(Intercept)" &
                                     decay_AvgThickness_Coef$ROI == "F"]

b_t_CTL_O_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "(Intercept)" &
                                     decay_AvgThickness_Coef$ROI == "O"]

b_t_CTL_P_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "(Intercept)" &
                                     decay_AvgThickness_Coef$ROI == "P"]

b_t_CTL_T_YW <-
  decay_AvgThickness_Coef$estimate[decay_AvgThickness_Coef$Diagnostic == "CONTROLE" &
                                     decay_AvgThickness_Coef$term == "(Intercept)" &
                                     decay_AvgThickness_Coef$ROI == "T"]
