dados_lm_age_10 <- filter(dados_hemi_v1, Diagnostic == "CONTROLE")  %>%
  group_by(Age_interval10) %>%
  do(fit_age_10 = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients por intervalo de idade ----
age_10_Coef = tidy(dados_lm_age_10,
                   fit_age_10,
                conf.int = TRUE,
                conf.level = 0.95)
age_10_Coef

age_10_R2 = glance(dados_lm_age_10, fit_age_10)
age_10_R2

# plot do modelo ----

ggplot(filter(dados_hemi_v1, Diagnostic == "CONTROLE"),
         aes(
           x = (log(ExposedArea)),
           y = (log(sqrt(AvgThickness) * TotalArea)),
           color = Age_interval10
         )) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Girificacao por decada",
    subtitle=  "controles",
    x = "Log Area exposta",
    y = "Log T^(1/2)*At",
    colour = "Idade em decadas",
    tag = "A"
  ) +
  theme_pubclean() +
  annotate(
    "text",
    label = paste(
      "Slope (40) = ",
      signif(age_10_Coef$estimate[2], 3),
      "\nSlope (50) = ",
      signif(age_10_Coef$estimate[4], 3) ,
      "\nSlope (60) = ",
      signif(age_10_Coef$estimate[6], 3),
      "\nSlope (70) = ",
      signif(age_10_Coef$estimate[8], 3),
      "\nSlope (80) = ",
      signif(age_10_Coef$estimate[10], 3)
    ),
    x = 4.6,
    y = 5.15,
    size = 3,
    colour = "black"
  )