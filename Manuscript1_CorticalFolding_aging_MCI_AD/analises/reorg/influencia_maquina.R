# INFLUENCIA DA LATERIDADE ----
dados_all <- filter(dados_all, !is.na(RM_Maquina), ROI == "hemisphere")

# COMPARA AS MEDIDAS ESTRUTURAIS ----

ttest_AreaT_maquina <-
t.test(TotalArea ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"), var.equal = FALSE)
Cohend_AreaT_maquina <- cohen.d(TotalArea ~ RM_Maquina, data = filter(dados_all))
ttest_AreaT_maquina
Cohend_AreaT_maquina


ttest_AreaE_maquina <-
  t.test(ExposedArea ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"), var.equal = FALSE)
Cohend_AreaE_maquina <- cohen.d(ExposedArea ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"))
ttest_AreaE_maquina
Cohend_AreaE_maquina


ttest_AvgThick_maquina <-
  t.test(AvgThickness ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"), var.equal = FALSE)
Cohend_AvgThick_maquina <-
  cohen.d(AvgThickness ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"))
ttest_AvgThick_maquina
Cohend_AvgThick_maquina


ttest_lGI_maquina <-
  t.test(localGI ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"), var.equal = FALSE)
Cohend_lGI_maquina <- cohen.d(localGI ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"))
ttest_lGI_maquina
Cohend_lGI_maquina

ttest_logk_maquina <-
  t.test(K ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"), var.equal = FALSE)
Cohend_logk_maquina <- cohen.d(K ~ RM_Maquina, data = filter(dados_all, ROI == "hemisphere"))
ttest_logk_maquina
Cohend_logk_maquina


# plots de comparacao - histogramas ----

hist_AreaT_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(TotalArea, fill = RM_Maquina)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Total grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaT_maquina$estimate, 5),
      ", p = ",
      signif(ttest_AreaT_maquina$p.value, 2)
    ),
    x = "Total Area",
    y = "Count"
  ) +
  theme_pubclean()
hist_AreaT_maquina

hist_AreaE_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(ExposedArea, fill = RM_Maquina)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Exposed grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaE_maquina$estimate, 5),
      ", p = ",
      signif(ttest_AreaE_maquina$p.value, 2)
    ),
    x = "Smooth Area",
    y = "Count"
  ) +
  theme_pubclean()
hist_AreaE_maquina

hist_AvgThick_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(AvgThickness, fill = RM_Maquina)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Average Cortical Thickness",
    subtitle = paste(
      "d = ",
      signif(Cohend_AvgThick_maquina$estimate, 5),
      ", p = ",
      signif(ttest_AvgThick_maquina$p.value, 2)
    ),
    x = "AvgThick",
    y = "Count"
  ) +
  theme_pubclean()

hist_AvgThick_maquina

hist_lGI_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(localGI, fill = RM_Maquina)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Local Gyrification Index",
    subtitle = paste(
      "d = ",
      signif(Cohend_lGI_maquina$estimate, 5),
      ", p = ",
      signif(ttest_lGI_maquina$p.value, 2)
    ),
    x = "Local Gyrification Index",
    y = "Count"
  ) +
  theme_pubclean()

hist_lGI_maquina

hist_logk_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(K, fill = RM_Maquina)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "log Offset k",
    subtitle = paste(
      "d = ",
      signif(Cohend_logk_maquina$estimate, 5),
      ", p = ",
      signif(ttest_logk_maquina$p.value, 2)
    ),
    x = "Total Area",
    y = "Count",
    tag = "E"
  ) +
  theme_pubclean()
hist_logk_maquina

# plots de comparacao - boxplot ----

bxplt_AreaT_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(RM_Maquina, TotalArea, color = RM_Maquina)) +
  geom_boxplot() +
  labs(
    title = "Total grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaT_maquina$estimate, 5),
      ", p = ",
      signif(ttest_AreaT_maquina$p.value, 2)
    ),
    x = "RM_Maquina",
    y = "Total Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AreaT_maquina

bxplt_AreaE_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(RM_Maquina, ExposedArea, color = RM_Maquina)) +
  geom_boxplot() +
  labs(
    title = "Exposed grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaE_maquina$estimate, 5),
      ", p = ",
      signif(ttest_AreaE_maquina$p.value, 2)
    ),
    x = "RM_Maquina",
    y = "Smooth Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AreaE_maquina

bxplt_AvgThick_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(RM_Maquina, AvgThickness, color = RM_Maquina)) +
  geom_boxplot() +
  labs(
    title = "Average Cortical Thickness",
    subtitle = paste(
      "d = ",
      signif(Cohend_AvgThick_maquina$estimate, 5),
      ", p = ",
      signif(ttest_AvgThick_maquina$p.value, 2)
    ),
    x = "RM_Maquina",
    y = "AvgThickness (mm)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AvgThick_maquina

bxplt_lGI_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(RM_Maquina, localGI, color = RM_Maquina)) +
  geom_boxplot() +
  labs(
    title = "Local Gyrification Index",
    subtitle = paste(
      "d = ",
      signif(Cohend_lGI_maquina$estimate, 5),
      ", p = ",
      signif(ttest_lGI_maquina$p.value, 2)
    ),
    x = "RM_Maquina",
    y = "local Gyrification Index"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_lGI_maquina

bxplt_logk_maquina <-
  ggplot(data = filter(dados_all, ROI == "hemisphere"), aes(RM_Maquina, K, color = RM_Maquina)) +
  geom_boxplot() +
  labs(
    title = "log Offset k",
    subtitle = paste(
      "d = ",
      signif(Cohend_logk_maquina$estimate, 5),
      ", p = ",
      signif(ttest_logk_maquina$p.value, 2)
    ),
    x = "RM_Maquina",
    y = "log Offset k",
    tag = "E"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_logk_maquina

# regr linear para k vs RM_Maquina ----

LM_maquina <-
  lm(formula = K ~ RM_Maquina * Diagnostic,
     data = filter(filter(dados_all, ROI == "hemisphere")))

summary(LM_maquina)

# regr linear para modelo GENERO ----

dados_lm_maquina <- filter(dados_all, ROI == "hemisphere")  %>%
  group_by(RM_Maquina) %>%
  do(fit_maquina = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients by group GENERO----
maquina_Coef = tidy(dados_lm_maquina,
                 fit_maquina,
                 conf.int = TRUE,
                 conf.level = 0.95)
maquina_Coef

maquina_Pred = augment(dados_lm_maquina, fit_maquina)
maquina_Pred

maquina_R2 = glance(dados_lm_maquina, fit_maquina)
maquina_R2

# grafico de aplicacao do modelo  ----

model_maquina <-
  ggplot(filter(dados_all, ROI == "hemisphere"),  aes(
    x = (log(ExposedArea)),
    y = (log(sqrt(AvgThickness) * TotalArea)),
    color = RM_Maquina
  )) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Girificacao por RM_Maquinao",
    subtitle = paste(
      "Intercept (Philips) = ",
      signif(maquina_Coef$estimate[2], 5),
      ", Intercept (Siemens) = ",
      signif(maquina_Coef$estimate[4], 5)
    ),
    x = "Log Area exposta",
    y = "Log T^(1/2)*At",
    colour = "RM_Maquina"
  ) +
  theme_pubclean() + ggsave("model_maquina.png", width = 7 , height = 4)

model_facet_maquina <-
  ggplot(filter(dados_all, ROI == "hemisphere"),  aes(
    x = (log(ExposedArea)),
    y = (log(sqrt(AvgThickness) * TotalArea)),
    color = RM_Maquina
  )) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(    x = "Log Ae",
           y = "Log T^(1/2)*At",
           colour = "RM_Maquina"
  ) +
  theme_pubclean() + facet_grid(Diagnostic ~ .) + ggsave("model_maquina_facet.png", width = 7 , height = 4)

figure_model_maquina <-
  ggarrange(model_maquina,
            model_facet_maquina,
            nrow = 2,
            labels = c("E", "F"), heights = c(2,3), common.legend = TRUE
  ) + ggsave("model_maquina_facet.png", width = 7 , height = 7)

# junta os graficos ----

figure2_maquina <-
  ggarrange(
    hist_AreaT_maquina,
    hist_AreaE_maquina,
    hist_AvgThick_maquina,
    hist_lGI_maquina,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D"), common.legend = TRUE
  )
figure2_maquina

figure2bxplt_maquina <-
  ggarrange(
    bxplt_AreaT_maquina,
    bxplt_AreaE_maquina,
    bxplt_AvgThick_maquina,
    bxplt_lGI_maquina,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D"), common.legend = TRUE)
figure2bxplt_maquina + ggsave("figure2bxplt.png", width = 7, height = 5)

# regr linear para modelo GENERO ----

dados_lm_maquina_age <-
  filter(dados_all, ROI == "hemisphere") %>%
  group_by(RM_Maquina, Age_interval10, Diagnostic) %>%
  do(fit_maquina_age = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients by group GENERO e IDADE----
maquina_age_Coef = tidy(dados_lm_maquina_age,
                     fit_maquina_age,
                     conf.int = TRUE,
                     conf.level = 0.95)
maquina_age_Coef

maquina_age_Pred = augment(dados_lm_maquina_age, fit_maquina_age)
maquina_age_Pred

maquina_age_R2 = glance(dados_lm_maquina_age, fit_maquina_age)
maquina_age_R2

# grafico coeficientes GENERO e IDADE----

maquina_age_intercept <-
  filter(maquina_age_Coef, term == "log(ExposedArea)")

maquina_age_log <-
  filter(maquina_age_Coef, term == "(Intercept)")

coef_maquina_age_a <- ggplot(
  maquina_age_intercept,
  aes(
    x = Age_interval10,
    y = estimate,
    color = Diagnostic,
    group = Diagnostic
  )
) +
  geom_point() + geom_line(aes(group = Diagnostic)) +
  geom_errorbar(aes(ymax = maquina_age_intercept$conf.high, ymin = maquina_age_intercept$conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Slope",
       colour = "Diagnostic",
       tag = "A") + geom_hline(aes(yintercept = 1.25)) +
  theme_pubclean() +  facet_grid(Diagnostic ~ RM_Maquina)


coef_maquina_age_b <- ggplot(maquina_age_log,
                          aes(
                            x = Age_interval10,
                            y = log(estimate),
                            color = Diagnostic
                          )) +
  geom_point() + geom_line(aes(group = Diagnostic)) +
  geom_errorbar(aes(ymax = maquina_age_log$conf.high, ymin = maquina_age_log$conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Offset (k)",
       colour = "Diagnostic",
       tag = "B") +
  theme_pubclean() + facet_grid(Diagnostic ~ RM_Maquina)