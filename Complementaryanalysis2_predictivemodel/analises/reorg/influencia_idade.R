# INFLUENCIA DA IDADE NOS DADOS ----


# verifica se existe diferenca na idade entre os grupos de diagnostcio

lm_age_diag_groups <- lm(Age ~ Diagnostic, data = dados_hemi_v1)

tidy(lm_age_diag_groups)
glance(lm_age_diag_groups)

# regr linear k vs intervalo de idade ----

LM_CTL_V1_Ageinterval10 <-
  lm(
    formula = K ~ Age_interval10,
    data = filter(dados_hemi_v1, Diagnostic == "CONTROLE")
  )

summary(LM_CTL_V1_Ageinterval10)

# regr linear do modelo por intervalo de idade ----
dados_lm_age <- filter(dados_hemi_v1, Age >=60 )  %>%
  group_by(Age_interval, Diagnostic) %>%
  do(fit_age = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients por intervalo de idade ----
age_Coef = tidy(dados_lm_age,
                fit_age,
                conf.int = TRUE,
                conf.level = 0.95)
age_Coef

age_Pred = augment(dados_lm_age, fit_age)
age_Pred

age_R2 = glance(dados_lm_age, fit_age)
age_R2

# plot do modelo ----

model_age <-
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
    subtitle = "controles",
    x = "Log Area exposta",
    y = "Log T^(1/2)*At",
    colour = "Idade em decadas",
    tag = "A"
  ) +
  theme_pubclean()
model_age

# plot dos coeficientes ----

age_intercept <- filter(age_Coef, term == "(Intercept)")

age_log <- filter(age_Coef, term == "log(ExposedArea)")

coef_age_a <- ggplot(age_intercept,
                     aes(x = Age_interval,
                         y = estimate, color = Diagnostic)) +
  geom_point() + geom_line(group = 1) +
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Offset (k)") +
  theme_pubclean() + facet_grid(Diagnostic ~ .)


coef_age_b <- ggplot(age_log,
                     aes(x = Diagnostic,
                         y = estimate, color = Age_interval)) +
  geom_point() + 
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low)) + geom_hline(aes(yintercept = 1.25)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Slope") +
  theme_pubclean() + stat_compare_means(method = "t.test", comparisons = my_comparisons, label = "p.signif") + facet_grid(Diagnostic ~
                                                                                                      .)


coef_age_b <- ggplot(age_intercept,
                     aes(x = Diagnostic,
                         y = estimate, color = Diagnostic)) +
  geom_point() +
  geom_errorbar(aes(ymax =
                      conf.high, ymin = conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Slope") +
  theme_pubclean() + stat_compare_means(comparisons = my_comparisons, label = "p.signif") + facet_grid(. ~ Age_interval) + geom_hline(aes(yintercept = 1.25))

figure4 <-
  ggarrange(coef_age_a,
            coef_age_b,
            nrow = 2,
            labels = c("B", "C"))
figure4


# plot R2 por idade -----

ggplot(data = age_R2,
       aes(x = Age_interval10,
           y = p.value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Comparando o offset log(k) para os diagnosticos por idade",
    subtitle = "Sessiona 1",
    x = "Intervalo de idade",
    y = "Offset log(k)",
    colour = "Diagnostico"
  ) +
  theme_pubclean()

# LOG OFFSET K PELA IDADE - ENVELEHECIMENTO SAUDADAVEL (SO CONTROLES) ----

ggplot(
  data = filter(dados_hemi_v1, Diagnostic == "CONTROLE"),
  aes(x = Age, y = K, color = Age_interval10)
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Comparando o log Offset (k) pela idade",
    subtitle = "Sessiona 1",
    x = "Idade",
    y = "Offset log(k)",
    colour = "Intervalo de idade (decadas)",
    caption = "Variacao na girificacao com a idade."
  ) +
  theme_pubclean() + stat_summary(aes(group = SUBJ_ses), fun.data = "mean_cl_boot")

ggplot(data = filter(dados_hemi_v1, Diagnostic == "CONTROLE"),
       aes(x = Age, y = K)) +
  geom_point(aes(color = Age_interval10)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Comparando o log Offset (k) pela idade",
    subtitle = "Sessiona 1",
    x = "Idade",
    y = "Offset log(k)",
    colour = "Intervalo de idade (decadas)",
    caption = "Variacao na girificacao com a idade."
  ) +
  theme_pubclean() + stat_summary(aes(group = SUBJ_ses), fun.data = "mean_cl_boot")