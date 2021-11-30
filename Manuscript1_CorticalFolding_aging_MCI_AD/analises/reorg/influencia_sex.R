# INFLUENCIA DO GENERO NOS DADOS ----

dados_hemi_v1_v1_CTL <-
  filter(dados_hemi_v1,
         Diagnostic == "CONTROLE")

aov_age_Gender_groups <- aov(Age ~ Gender, data = dados_hemi_v1)


# COMPARA AS MEDIDAS ESTRUTURAIS ----
ttest_AreaT <-
  t.test(TotalArea ~ Gender, data = dados_hemi_v1_v1_CTL, var.equal = FALSE)
Cohend_AreaT <- cohen.d(TotalArea ~ Gender, data = dados_hemi_v1_v1_CTL)
ttest_AreaT
Cohend_AreaT


ttest_AreaE <-
  t.test(ExposedArea ~ Gender, data = dados_hemi_v1_v1_CTL, var.equal = FALSE)
Cohend_AreaE <- cohen.d(ExposedArea ~ Gender, data = dados_hemi_v1_v1_CTL)
ttest_AreaE
Cohend_AreaE


ttest_AvgThick <-
  t.test(AvgThickness ~ Gender, data = dados_hemi_v1_v1_CTL, var.equal = FALSE)
Cohend_AvgThick <-
  cohen.d(AvgThickness ~ Gender, data = dados_hemi_v1_v1_CTL)
ttest_AvgThick
Cohend_AvgThick


ttest_lGI <-
  t.test(localGI ~ Gender, data = dados_hemi_v1_v1_CTL, var.equal = FALSE)
Cohend_lGI <- cohen.d(localGI ~ Gender, data = dados_hemi_v1_v1_CTL)
ttest_lGI
Cohend_lGI

ttest_logk <-
  t.test(K ~ Gender, data = dados_hemi_v1_v1_CTL, var.equal = FALSE)
Cohend_logk <- cohen.d(K ~ Gender, data = dados_hemi_v1_v1_CTL)
ttest_logk
Cohend_logk


# plots de comparacao - histogramas ----

hist_AreaT <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(TotalArea, fill = Gender)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Total grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaT$estimate, 5),
      ", p = ",
      signif(ttest_AreaT$p.value, 2)
    ),
    x = "Total Area",
    y = "Count"
  ) +
  theme_pubclean()
hist_AreaT

hist_AreaE <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(ExposedArea, fill = Gender)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Exposed grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaE$estimate, 5),
      ", p = ",
      signif(ttest_AreaE$p.value, 2)
    ),
    x = "Smooth Area",
    y = "Count"
  ) +
  theme_pubclean()
hist_AreaE

hist_AvgThick <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(AvgThickness, fill = Gender)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Average Cortical Thickness",
    subtitle = paste(
      "d = ",
      signif(Cohend_AvgThick$estimate, 5),
      ", p = ",
      signif(ttest_AvgThick$p.value, 2)
    ),
    x = "AvgThick",
    y = "Count"
  ) +
  theme_pubclean()

hist_AvgThick

hist_lGI <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(localGI, fill = Gender)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "Local Gyrification Index",
    subtitle = paste(
      "d = ",
      signif(Cohend_lGI$estimate, 5),
      ", p = ",
      signif(ttest_lGI$p.value, 2)
    ),
    x = "Local Gyrification Index",
    y = "Count"
  ) +
  theme_pubclean()

hist_lGI

hist_logk <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(K, fill = Gender)) +
  geom_histogram(alpha = .6, position = "identity") +
  labs(
    title = "log Offset k",
    subtitle = paste(
      "d = ",
      signif(Cohend_logk$estimate, 5),
      ", p = ",
      signif(ttest_logk$p.value, 2)
    ),
    x = "Total Area",
    y = "Count",
    tag = "E"
  ) +
  theme_pubclean()
hist_logk

# plots de comparacao - boxplot ----

bxplt_AreaT <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(Gender, TotalArea, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "Total grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaT$estimate, 5),
      ", p = ",
      signif(ttest_AreaT$p.value, 2)
    ),
    x = "Gendero",
    y = "Total Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AreaT

bxplt_AreaE <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(Gender, ExposedArea, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "Exposed grey matter surface area",
    subtitle = paste(
      "d = ",
      signif(Cohend_AreaE$estimate, 5),
      ", p = ",
      signif(ttest_AreaE$p.value, 2)
    ),
    x = "Gendero",
    y = "Smooth Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AreaE

bxplt_AvgThick <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(Gender, AvgThickness, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "Average Cortical Thickness",
    subtitle = paste(
      "d = ",
      signif(Cohend_AvgThick$estimate, 5),
      ", p = ",
      signif(ttest_AvgThick$p.value, 2)
    ),
    x = "Gendero",
    y = "AvgThickness (mm)"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_AvgThick

bxplt_lGI <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(Gender, localGI, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "Local Gyrification Index",
    subtitle = paste(
      "d = ",
      signif(Cohend_lGI$estimate, 5),
      ", p = ",
      signif(ttest_lGI$p.value, 2)
    ),
    x = "Gendero",
    y = "local Gyrification Index"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_lGI

bxplt_logk <-
  ggplot(data = dados_hemi_v1_v1_CTL, aes(Gender, K, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "log Offset k",
    subtitle = paste(
      "d = ",
      signif(Cohend_logk$estimate, 5),
      ", p = ",
      signif(ttest_logk$p.value, 2)
    ),
    x = "Gendero",
    y = "log Offset k",
    tag = "E"
  ) +
  theme_pubclean() + stat_compare_means(label = "p.signif", method = "t.test")

bxplt_logk

# regr linear para k vs gender ----

LM_Gender <-
  lm(formula = K ~ Gender * Diagnostic,
     data = filter(dados_hemi_v1))

summary(LM_Gender)

# regr linear para modelo GENERO ----

dados_lm_gender <- dados_hemi_v1  %>%
  group_by(Gender) %>%
  do(fit_gender = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients by group GENERO----
gender_Coef = tidy(dados_lm_gender,
                   fit_gender,
                   conf.int = TRUE,
                   conf.level = 0.95)
gender_Coef

gender_Pred = augment(dados_lm_gender, fit_gender)
gender_Pred

gender_R2 = glance(dados_lm_gender, fit_gender)
gender_R2

# grafico de aplicacao do modelo  ----

model <-
  ggplot(dados_hemi_v1,  aes(
    x = (log(ExposedArea)),
    y = (log(sqrt(AvgThickness) * TotalArea)),
    color = Gender
  )) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Girificacao por Gendero",
    subtitle = paste(
      "slope alpha (female) = ",
      signif(gender_Coef$estimate[2], 5),
      ", slope alpha (male) = ",
      signif(gender_Coef$estimate[4], 5)
    ),
    x = "Log Area exposta",
    y = "Log T^(1/2)*At",
    colour = "Gendero"
  ) +
  theme_pubclean() + ggsave("model_Gender.png", width = 7 , height = 4)

model_facet <-
  ggplot(dados_hemi_v1,  aes(
    x = (log(ExposedArea)),
    y = (log(sqrt(AvgThickness) * TotalArea)),
    color = Gender
  )) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(    x = "Log Ae",
    y = "Log T^(1/2)*At",
    colour = "Gendero"
  ) +
  theme_pubclean() + facet_grid(Diagnostic ~ .) + ggsave("model_Gender_facet.png", width = 7 , height = 4)

figure_model_ <-
  ggarrange(model,
    model_facet,
    nrow = 2,
    labels = c("E", "F"), heights = c(2,3), common.legend = TRUE
  ) + ggsave("model_Gender_facet.png", width = 7 , height = 7)

# junta os graficos ----

figure2 <-
  ggarrange(
    hist_AreaT,
    hist_AreaE,
    hist_AvgThick,
    hist_lGI,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D"), common.legend = TRUE
  )
figure2

figure2bxplt <-
  ggarrange(
    bxplt_AreaT,
    bxplt_AreaE,
    bxplt_AvgThick,
    bxplt_lGI,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D"), common.legend = TRUE)
figure2bxplt + ggsave("figure2bxplt.png", width = 7, height = 5)

# regr linear para modelo GENERO ----

dados_lm_gender_age <-
  dados_hemi_v1 %>%
  group_by(Gender, Age_interval10, Diagnostic) %>%
  do(fit_gender_age = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients by group GENERO e IDADE----
gender_age_Coef = tidy(dados_lm_gender_age,
                       fit_gender_age,
                       conf.int = TRUE,
                       conf.level = 0.95)
gender_age_Coef

gender_age_Pred = augment(dados_lm_gender_age, fit_gender_age)
gender_age_Pred

gender_age_R2 = glance(dados_lm_gender_age, fit_gender_age)
gender_age_R2

# grafico coeficientes GENERO e IDADE----

gender_age_intercept <-
  filter(gender_age_Coef, term == "log(ExposedArea)")

gender_age_log <-
  filter(gender_age_Coef, term == "(Intercept)")

coef_gender_age_a <- ggplot(
  gender_age_intercept,
  aes(
    x = Age_interval10,
    y = estimate,
    color = Diagnostic,
    group = Diagnostic
  )
) +
  geom_point() + geom_line(aes(group = Diagnostic)) +
  geom_errorbar(aes(ymax = gender_age_intercept$conf.high, ymin = gender_age_intercept$conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Slope",
       colour = "Diagnostic",
       tag = "A") + geom_hline(aes(yintercept = 1.25)) +
  theme_pubclean() +  facet_grid(Diagnostic ~ Gender)


coef_gender_age_b <- ggplot(gender_age_log,
                            aes(
                              x = Age_interval10,
                              y = log(estimate),
                              color = Diagnostic
                            )) +
  geom_point() + geom_line(aes(group = Diagnostic)) +
  geom_errorbar(aes(ymax = gender_age_log$conf.high, ymin = gender_age_log$conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Offset (k)",
       colour = "Diagnostic",
       tag = "B") +
  theme_pubclean() + facet_grid(Diagnostic ~ Gender)

#   estatistica ----

dados_lm_Gender_age_t <-
  dados_hemi_v1 %>%
  group_by(Age_interval10) %>%
  do(fit_Gender_age_t = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea) + Gender + log(ExposedArea)*Gender, data = .))

anova(dados_lm_Gender_age_t$fit_Gender_age_t[[1]])
anova(dados_lm_Gender_age_t$fit_Gender_age_t[[2]])
anova(dados_lm_Gender_age_t$fit_Gender_age_t[[3]])
anova(dados_lm_Gender_age_t$fit_Gender_age_t[[4]])
anova(dados_lm_Gender_age_t$fit_Gender_age_t[[5]])

