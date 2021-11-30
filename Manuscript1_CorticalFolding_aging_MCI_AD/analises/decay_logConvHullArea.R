# taxa de caimento da girificacao (espessura)----

decay_logConvexHullArea <- filter(dados, Session == "1", ROI == "hemisphere", Longitudinal_correction == "yes", method == "Yujiang_script") %>% droplevels() %>%
  group_by(method, Diagnostic) %>%
  do(fit_decay_logConvexHullArea = rlm(logConvexHullArea ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_logConvexHullArea_Coef = tidy(
  decay_logConvexHullArea,  fit_decay_logConvexHullArea,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda, b e Ko CTL - com correcao longitudinal ----

lambda_logCH_CTL_hemi <-
  decay_logConvexHullArea_Coef$estimate[decay_logConvexHullArea_Coef$Diagnostic == "CONTROLE" &
                                     decay_logConvexHullArea_Coef$term == "Age"]

b_logCH_CTL_hemi <-
  decay_logConvexHullArea_Coef$estimate[decay_logConvexHullArea_Coef$Diagnostic == "CONTROLE" &
                                       decay_logConvexHullArea_Coef$term == "(Intercept)"]
