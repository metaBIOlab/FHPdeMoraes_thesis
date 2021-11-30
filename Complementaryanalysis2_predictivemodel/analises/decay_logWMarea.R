# taxa de caimento da girificacao (log k teorico)----

decay_logWhiteSurfArea <-
  filter(dados,
         Session == "1",
         ROI == "hemisphere",
         method == "FreeSurferStandard", Longitudinal_correction == "yes")  %>%
  group_by(Diagnostic) %>%
  do(fit_decay_logWhiteSurfArea = rlm(logWhiteSurfArea ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_logWhiteSurfArea_Coef = tidy(decay_logWhiteSurfArea,
                           fit_decay_logWhiteSurfArea,
                           conf.int = TRUE,
                           conf.level = 0.95)

# lambda, b e Ko CTL ----

lambda_logWMA_CTL_hemi_FS <-
  decay_logWhiteSurfArea_Coef$estimate[decay_logWhiteSurfArea_Coef$Longitudinal_correction == "yes" &
                                         decay_logWhiteSurfArea_Coef$Diagnostic == "CONTROLE" &
                                         decay_logWhiteSurfArea_Coef$term == "Age"]

b_logWMA_CTL_hemi_FS <-
  decay_logWhiteSurfArea_Coef$estimate[decay_logWhiteSurfArea_Coef$Longitudinal_correction == "yes" &
                                         decay_logWhiteSurfArea_Coef$Diagnostic == "CONTROLE" &
                                         decay_logWhiteSurfArea_Coef$term == "(Intercept)"]
# taxa de caimento da girificacao (log k teorico)----

decay_logWhiteSurfArea <-
  filter(dados,
         Session == "1",
         ROI == "hemisphere",
         method == "Yujiang_script", Longitudinal_correction == "yes")  %>%
  group_by(Diagnostic) %>%
  do(fit_decay_logWhiteSurfArea = rlm(logWhiteSurfArea ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_logWhiteSurfArea_Coef = tidy(decay_logWhiteSurfArea,
                                   fit_decay_logWhiteSurfArea,
                                   conf.int = TRUE,
                                   conf.level = 0.95)

# lambda, b e Ko CTL ----

lambda_logWMA_CTL_hemi_YW <-
  decay_logWhiteSurfArea_Coef$estimate[decay_logWhiteSurfArea_Coef$Longitudinal_correction == "yes" &
                                         decay_logWhiteSurfArea_Coef$Diagnostic == "CONTROLE" &
                                         decay_logWhiteSurfArea_Coef$term == "Age"]

b_logWMA_CTL_hemi_YW <-
  decay_logWhiteSurfArea_Coef$estimate[decay_logWhiteSurfArea_Coef$Longitudinal_correction == "yes" &
                                         decay_logWhiteSurfArea_Coef$Diagnostic == "CONTROLE" &
                                         decay_logWhiteSurfArea_Coef$term == "(Intercept)"]

