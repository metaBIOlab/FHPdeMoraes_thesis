lm_Age <- filter(dados_hemi_v1, Diagnostic == "CTL", Age_interval != "40-45", Age_interval != "80-85") %>%
  group_by(Age_interval) %>%
  do(fit_Age = tidy(lm(1/2 * logAvgThickness + logTotalArea ~ logExposedArea, data = ., na.action = na.omit), conf.int= TRUE)) %>% unnest(cols = c(fit_Age))

lm_Age <- filter(lm_Age, term == "logExposedArea")

lm_Age$Age_intervalnumber <- as.numeric(factor(lm_Age$Age_interval))-1

cor.test(lm_Age$estimate, lm_Age$Age_intervalnumber)