# taxa de caimento da girificacao (log k teorico)----

decay_WMVolume_CC <- filter(dados, Session == "1", method == "Yujiang_script", Longitudinal_correction == "yes") %>% group_by(Diagnostic) %>%
  do(fit_decay_WMVolume = rlm(log10(WM_volume_CC) ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_WMVolume_Coef = tidy(
  decay_WMVolume_CC,
  fit_decay_WMVolume)

# lambda, b e Ko CTL ----

lambda_WMCC_CTL_CC <-
  decay_WMVolume_Coef$estimate[decay_WMVolume_Coef$Diagnostic == "CONTROLE" &
                                    decay_WMVolume_Coef$term == "Age"]

b_WMCC_CTL_CC <-
  decay_WMVolume_Coef$estimate[decay_WMVolume_Coef$Diagnostic == "CONTROLE" &
                                 decay_WMVolume_Coef$term == "(Intercept)"]

# ----
dados <-
  dados %>% mutate(
    WM_volume_CC_age_decay = WM_volume_CC - b_WMCC_CTL_CC - lambda_WMCC_CTL_CC * Age)