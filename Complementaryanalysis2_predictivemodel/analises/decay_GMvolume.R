# taxa de caimento da girificacao (log k teorico)----

decay_GMvolume <- filter(dados, Session == "1", ROI == "hemisphere", method == "FreeSurferStandard" | method == "Yujiang_script", Longitudinal_correction == "yes")  %>%
  group_by(method, Diagnostic) %>%
  do(fit_decay_GMvolume = rlm(GMvolume ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_GMvolume_Coef = tidy(decay_GMvolume,
                            fit_decay_GMvolume,
                            conf.int = TRUE,
                            conf.level = 0.95)

# lambda, b e Ko CTL ----


lambda_GMV_CTL_hemi_FS <-
  decay_GMvolume_Coef$estimate[decay_GMvolume_Coef$method == "FreeSurferStandard" &
                                 decay_GMvolume_Coef$Diagnostic == "CONTROLE" &
                                 decay_GMvolume_Coef$term == "Age"]
b_GMV_CTL_hemi_FS <-
  decay_GMvolume_Coef$estimate[decay_GMvolume_Coef$method == "FreeSurferStandard" &
                                 decay_GMvolume_Coef$Diagnostic == "CONTROLE" &
                                 decay_GMvolume_Coef$term == "(Intercept)"]


lambda_GMV_CTL_hemi_YW <-
  decay_GMvolume_Coef$estimate[decay_GMvolume_Coef$method == "Yujiang_script" &
                                 decay_GMvolume_Coef$Diagnostic == "CONTROLE" &
                                 decay_GMvolume_Coef$term == "Age"]
b_GMV_CTL_hemi_YW <-
  decay_GMvolume_Coef$estimate[decay_GMvolume_Coef$method == "Yujiang_script" &
                                 decay_GMvolume_Coef$Diagnostic == "CONTROLE" &
                                 decay_GMvolume_Coef$term == "(Intercept)"]