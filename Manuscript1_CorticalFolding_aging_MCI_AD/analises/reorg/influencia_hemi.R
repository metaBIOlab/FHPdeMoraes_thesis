# INFLUENCIA DA LATERIDADE ----

# COMPARA AS MEDIDAS ESTRUTURAIS ----
ttest_AreaT_hemi <-
  t.test(TotalArea ~ hemi, data = dados_hemi_v1, var.equal = FALSE)
Cohend_AreaT_hemi <- cohen.d(TotalArea ~ hemi, data = dados_hemi_v1)
ttest_AreaT_hemi
Cohend_AreaT_hemi


ttest_AreaE_hemi <-
  t.test(ExposedArea ~ hemi, data = dados_hemi_v1, var.equal = FALSE)
Cohend_AreaE_hemi <- cohen.d(ExposedArea ~ hemi, data = dados_hemi_v1)
ttest_AreaE_hemi
Cohend_AreaE_hemi


ttest_AvgThick_hemi <-
  t.test(AvgThickness ~ hemi, data = dados_hemi_v1, var.equal = FALSE)
Cohend_AvgThick_hemi <-
  cohen.d(AvgThickness ~ hemi, data = dados_hemi_v1)
ttest_AvgThick_hemi
Cohend_AvgThick_hemi


ttest_lGI_hemi <-
  t.test(localGI ~ hemi, data = dados_hemi_v1, var.equal = FALSE)
Cohend_lGI_hemi <- cohen.d(localGI ~ hemi, data = dados_hemi_v1)
ttest_lGI_hemi
Cohend_lGI_hemi

ttest_logk_hemi <-
  t.test(K ~ hemi, data = dados_hemi_v1, var.equal = FALSE)
Cohend_logk_hemi <- cohen.d(K ~ hemi, dados_hemi_v1)
ttest_logk_hemi
Cohend_logk_hemi


# plots de comparacao - histogramas ----

hist_AreaT_hemi <-
  ggplot(data = dados_hemi_v1, aes(TotalArea, fill = hemi)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Total grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaT_hemi$estimate, 5),
      ", p = ",
      signif(ttest_AreaT_hemi$p.value, 2)
    ),
    x = "Total Area",
    y = "Count"
  ) +
  theme_pubclean()
hist_AreaT_hemi

hist_AreaE_hemi <-
  ggplot(data = dados_hemi_v1, aes(ExposedArea, fill = hemi)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Exposed grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaE_hemi$estimate, 5),
      ", p = ",
      signif(ttest_AreaE_hemi$p.value, 2)
    ),
    x = "Smooth Area",
    y = "Count"
  ) +
  theme_pubclean()
hist_AreaE_hemi

hist_AvgThick_hemi <-
  ggplot(data = dados_hemi_v1, aes(AvgThickness, fill = hemi)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Average Cortical Thickness",
    subtitle = paste(
      "d = ",
      signif(Cohend_AvgThick_hemi$estimate, 5),
      ", p = ",
      signif(ttest_AvgThick_hemi$p.value, 2)
    ),
    x = "AvgThick",
    y = "Count"
  ) +
  theme_pubclean()

hist_AvgThick_hemi

hist_lGI_hemi <-
  ggplot(data = dados_hemi_v1, aes(localGI, fill = hemi)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Local Gyrification Index",
    subtitle = paste(
      "d = ",
      signif(Cohend_lGI_hemi$estimate, 5),
      ", p = ",
      signif(ttest_lGI_hemi$p.value, 2)
    ),
    x = "Local Gyrification Index",
    y = "Count"
  ) +
  theme_pubclean()

hist_lGI_hemi

hist_logk_hemi <-
  ggplot(data = dados_hemi_v1, aes(K, fill = hemi)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "log Offset k",
    subtitle = paste(
      "d = ",
      signif(Cohend_logk_hemi$estimate, 5),
      ", p = ",
      signif(ttest_logk_hemi$p.value, 2)
    ),
    x = "Total Area",
    y = "Count",
    tag = "E"
  ) +
  theme_pubclean()
hist_logk_hemi

# plots de comparacao - boxplot ----

bxplt_AreaT_hemi <-
  ggplot(data = dados_hemi_v1, aes(hemi, TotalArea, color = hemi)) +
  geom_boxplot() +
  labs(
    title = "Total grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaT_hemi$estimate, 5),
      ", p = ",
      signif(ttest_AreaT_hemi$p.value, 2)
    ),
    x = "hemi",
    y = "Total Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AreaT_hemi

bxplt_AreaE_hemi <-
  ggplot(data = dados_hemi_v1, aes(hemi, ExposedArea, color = hemi)) +
  geom_boxplot() +
  labs(
    title = "Exposed grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaE_hemi$estimate, 5),
      ", p = ",
      signif(ttest_AreaE_hemi$p.value, 2)
    ),
    x = "hemi",
    y = "Smooth Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AreaE_hemi

bxplt_AvgThick_hemi <-
  ggplot(data = dados_hemi_v1, aes(hemi, AvgThickness, color = hemi)) +
  geom_boxplot() +
  labs(
    title = "Average Cortical Thickness",
    subtitle = paste(
      "d = ",
      signif(Cohend_AvgThick_hemi$estimate, 5),
      ", p = ",
      signif(ttest_AvgThick_hemi$p.value, 2)
    ),
    x = "hemi",
    y = "AvgThickness (mm)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AvgThick_hemi

bxplt_lGI_hemi <-
  ggplot(data = dados_hemi_v1, aes(hemi, localGI, color = hemi)) +
  geom_boxplot() +
  labs(
    title = "Local Gyrification Index",
    subtitle = paste(
      "d = ",
      signif(Cohend_lGI_hemi$estimate, 5),
      ", p = ",
      signif(ttest_lGI_hemi$p.value, 2)
    ),
    x = "hemi",
    y = "local Gyrification Index"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_lGI_hemi

bxplt_logk_hemi <-
  ggplot(data = dados_hemi_v1, aes(hemi, K, color = hemi)) +
  geom_boxplot() +
  labs(
    title = "log Offset k",
    subtitle = paste(
      "d = ",
      signif(Cohend_logk_hemi$estimate, 5),
      ", p = ",
      signif(ttest_logk_hemi$p.value, 2)
    ),
    x = "hemi",
    y = "log Offset k",
    tag = "E"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_logk_hemi

# regr linear para k vs hemi ----

LM_hemi <-
  lm(formula = K ~ hemi * Diagnostic,
     data = filter(dados_hemi_v1))

summary(LM_hemi)

# regr linear para modelo GENERO ----

dados_lm_hemi <- dados_hemi_v1  %>%
  group_by(hemi) %>%
  do(fit_hemi = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients by group GENERO----
hemi_Coef = tidy(dados_lm_hemi,
                 fit_hemi,
                 conf.int = TRUE,
                 conf.level = 0.95)
hemi_Coef

hemi_Pred = augment(dados_lm_hemi, fit_hemi)
hemi_Pred

hemi_R2 = glance(dados_lm_hemi, fit_hemi)
hemi_R2

# grafico de aplicacao do modelo  ----

model_hemi <-
  ggplot(dados_hemi_v1,  aes(
    x = (log(ExposedArea)),
    y = (log(sqrt(AvgThickness) * TotalArea)),
    color = hemi
  )) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Girificacao por hemi",
    subtitle = paste(
      "Slope alpha (L) = ",
      signif(hemi_Coef$estimate[2], 5),
      ", Slope alpha (R) = ",
      signif(hemi_Coef$estimate[4], 5)
    ),
    x = "Log Area exposta",
    y = "Log T^(1/2)*At",
    colour = "hemi"
  ) +
  theme_pubclean() + ggsave("model_hemi.png", width = 7 , height = 4)

model_facet_hemi <-
  ggplot(dados_hemi_v1,  aes(
    x = (log(ExposedArea)),
    y = (log(sqrt(AvgThickness) * TotalArea)),
    color = hemi
  )) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(x = "Log Ae",
       y = "Log T^(1/2)*At",
       colour = "hemi") +
  theme_pubclean() + facet_grid(Diagnostic ~ .) + ggsave("model_hemi_facet.png", width = 7 , height = 4)

figure_model_hemi <-
  ggarrange(
    model_hemi,
    model_facet_hemi,
    nrow = 2,
    labels = c("E", "F"),
    heights = c(2, 3),
    common.legend = TRUE
  ) + ggsave("model_hemi_facet.png", width = 7 , height = 7)

# junta os graficos ----

figure2_hemi <-
  ggarrange(
    hist_AreaT_hemi,
    hist_AreaE_hemi,
    hist_AvgThick_hemi,
    hist_lGI_hemi,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D"),
    common.legend = TRUE
  )
figure2_hemi

figure2bxplt_hemi <-
  ggarrange(
    bxplt_AreaT_hemi,
    bxplt_AreaE_hemi,
    bxplt_AvgThick_hemi,
    bxplt_lGI_hemi,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D"),
    common.legend = TRUE
  )
figure2bxplt_hemi + ggsave("figure2bxplt.png", width = 7, height = 5)

# regr linear para modelo GENERO ----

dados_lm_hemi_age <-
  dados_hemi_v1 %>%
  group_by(hemi, Age_interval10, Diagnostic) %>%
  do(fit_hemi_age = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients by group GENERO e IDADE----
hemi_age_Coef = tidy(dados_lm_hemi_age,
                     fit_hemi_age,
                     conf.int = TRUE,
                     conf.level = 0.95)
hemi_age_Coef

hemi_age_Pred = augment(dados_lm_hemi_age, fit_hemi_age)
hemi_age_Pred

hemi_age_R2 = glance(dados_lm_hemi_age, fit_hemi_age)
hemi_age_R2

# grafico coeficientes GENERO e IDADE----

hemi_age_intercept <-
  filter(hemi_age_Coef, term == "log(ExposedArea)")

hemi_age_log <-
  filter(hemi_age_Coef, term == "(Intercept)")

coef_hemi_age_a <-
  ggplot(
    hemi_age_intercept,
    aes(
      x = Age_interval10,
      y = estimate,
      color = Diagnostic,
      group = Diagnostic
    )
  ) +
  geom_point() + geom_line(aes(group = Diagnostic)) +
  geom_errorbar(aes(ymax = hemi_age_intercept$conf.high, ymin = hemi_age_intercept$conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Slope",
       colour = "Diagnostic",
       tag = "A") + geom_hline(aes(yintercept = 1.25)) +
  theme_pubclean() +  facet_grid(Diagnostic ~ hemi)


coef_hemi_age_b <-
  ggplot(hemi_age_log,
         aes(
           x = Age_interval10,
           y = log(estimate),
           color = Diagnostic
         )) +
  geom_point() + geom_line(aes(group = Diagnostic)) +
  geom_errorbar(aes(ymax = hemi_age_log$conf.high, ymin = hemi_age_log$conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Offset (k)",
       colour = "Diagnostic",
       tag = "B") +
  theme_pubclean() + facet_grid(Diagnostic ~ hemi)

#   estatistica ----

dados_lm_hemi_age_t <-
  dados_hemi_v1 %>%
  group_by(Age_interval10) %>%
  do(fit_hemi_age_t = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea) + hemi + log(ExposedArea)*hemi, data = .))

dados_lm_hemi_age_t$fit_hemi_age_t

anova(dados_lm_hemi_age_t$fit_hemi_age_t[[1]])
anova(dados_lm_hemi_age_t$fit_hemi_age_t[[2]])