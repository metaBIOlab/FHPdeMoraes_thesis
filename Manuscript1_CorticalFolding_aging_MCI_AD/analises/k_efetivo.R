# CALCULO K EFETIVO ----

lm_keffective <- filter(dados, Session == "1") %>%
  group_by(Longitudinal_correction, ROI, Diagnostic, Age_interval10) %>%
  do(fit_keffective = lm(1/2 * logAvgThickness + logTotalArea ~ logExposedArea, data = ., na.action = na.omit))

keffective_Coef = tidy(lm_keffective,
                       fit_keffective)
keffective_Coef
#keffective_Coef %>% kable(digits = 4) %>% kable_styling()