# LIPOXINA - EDA

lipoxina1 <- ggplot(
  data = filter(
    dados_hemi_v1,
    Session == 1),
  aes(Lipoxina, K, color = Diagnostic)
) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Comparando o offset log(k) por medida de Lipoxina",
    subtitle = "Sessiona 1",
    x = "Lipoxina",
    y = "log Offset (k)",
    colour = "Diagnostico"
  ) +
  theme_pubclean() + facet_grid(. ~Age_interval10)

lipoxina2 <- ggplot(
  data = dados_hemi_v1,
  mapping = aes(color = Diagnostic)
) +
  geom_density(mapping = aes(x = Lipoxina)) +
  labs(
    title = "Comparando Lipoxina para os diagnosticos",
    subtitle = "Sessiona 1",
    x = "Lipoxina",
    y = "Densidade",
    colour = "Diagnostico"
  ) +
  theme_pubclean() + facet_grid(. ~Age_interval10)

lipoxinalm <- dados_hemi_v1  %>%
  do(fit_lipoxina = lm(Lipoxina ~ K + Age + Diagnostic, data = .))

lipoxina_Coef = tidy(lipoxinalm,
                fit_lipoxina,
                conf.int = TRUE,
                conf.level = 0.95)

lipoxina_Pred = augment(lipoxinalm, fit_lipoxina)

lipoxina_R2 = glance(lipoxinalm, fit_lipoxina)


fit_lipoxina_age_diag <- aov(Lipoxina ~ K * Age * Diagnostic, data = dados_hemi_v1)
