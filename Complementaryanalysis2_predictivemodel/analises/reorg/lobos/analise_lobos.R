# ANALISE LOBOS ----

# regr linear para modelo GENERO ----

dados_lm_lobes <-
  dados_lobos_v1 %>%
  group_by(Age_interval10, Diagnostic) %>%
  do(fit_lobes = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = .))

# get the coefficients by group GENERO e IDADE----
lobes_Coef = tidy(dados_lm_lobes,
                  fit_lobes,
                  conf.int = TRUE,
                  conf.level = 0.95)
lobes_Coef

lobes_Pred = augment(dados_lm_lobes, fit_lobes)
lobes_Pred

lobes_R2 = glance(dados_lm_lobes, fit_lobes)
lobes_R2

# grafico coeficientes GENERO e IDADE----

lobes_intercept <- filter(lobes_Coef, term == "(Intercept)")

lobes_log <- filter(lobes_Coef, term == "log(ExposedArea)")

coef_lobes_b <- ggplot(lobes_log,
                       aes(
                         x = Age_interval10,
                         y = estimate,
                         color = Diagnostic,
                         group = Diagnostic
                       )) +
  geom_point() + geom_line(aes(group = Diagnostic)) +
  geom_errorbar(aes(ymax = lobes_log$conf.high, ymin = lobes_log$conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Slope",
       colour = "Diagnostic") + geom_hline(aes(yintercept = 1.25)) +
  theme_pubclean() +  facet_grid(Diagnostic ~ .)

coef_lobes_a <- ggplot(
  lobes_intercept,
  aes(
    x = Age_interval10,
    y = estimate,
    color = Diagnostic,
    group = Diagnostic
  )
) +
  geom_point() + geom_line(aes(group = Diagnostic)) +
  geom_errorbar(aes(ymax = lobes_intercept$conf.high, ymin = lobes_intercept$conf.low)) +
  labs(x = "Intervalo idade (decadas)",
       y = "Offset k",
       colour = "Diagnostic") +
  theme_pubclean() +  facet_grid(Diagnostic ~ .)

# FIT LOBES ----

fit_lobes <-
  lm(log(sqrt(AvgThickness) * TotalArea) ~ log(ExposedArea), data = dados_lobos_v1)

lobos1 <- ggplot() +
  geom_point(data = dados_lobos_v1,
             aes(
               y = log(sqrt(AvgThickness) * TotalArea),
               x = log(ExposedArea),
               color = ROI
             )) +
  geom_point(data = dados_hemi_v1,
             aes(
               y = log(sqrt(AvgThickness) * TotalArea),
               x = log(ExposedArea),
               color = "hemisphere"
             ),
             alpha = 0.8) +
  labs(
    title = "Aplicacao do modelo de girificacao",
    subtitle = paste(
      "LOBOS: Adj R2 = ",
      signif(summary(fit_lobes)$adj.r.squared, 5),
      ", Intercept (log(k)) = ",
      signif(tidy(fit_lobes)[1, 2], 5),
      ",\nSlope ",
      expression(alpha),
      "= ",
      signif(tidy(fit_lobes)[2, 2], 5),
      ",  p = ",
      signif(glance(fit_lobes)[1, 5], 5)
    ),
    y = "log 10 At T1/2",
    x = "log Ae",
    color = "ROI"
  ) + geom_smooth(
    data = dados_hemi_v1,
    aes(
      y = log(sqrt(AvgThickness) * TotalArea),
      x = log(ExposedArea),
      color = "hemisphere"
    ),
    alpha = 0.8,
    method = "lm",
    fullrange = TRUE,
    se = FALSE
  ) +
  geom_smooth(
    data = dados_lobos_v1,
    aes(
      y = log(sqrt(AvgThickness) * TotalArea),
      x = log(ExposedArea),
      color = "lobes"
    ),
    method = "lm",
    fullrange = TRUE,
    se = FALSE
  ) +
  theme_pubclean() + stat_cor()
# CORRIGIDO ----

fit_lobes_corrected <-
  lm(
    log(sqrt(AvgThickness) * TotalArea_corrected) ~ log(SmoothArea_corrected),
    data = filter(
      dados_lobos_v1,
      Session == 1,
      Diagnostic == "CONTROLE",
      ROI == "F"| ROI == "T"| ROI == "O"| ROI == "P"
    )
  )


lobos1_corrected <- ggplot() +
  geom_point(data = dados_lobos_v1,
             aes(
               y = log(sqrt(AvgThickness) * TotalArea_corrected),
               x = log(SmoothArea_corrected),
               color = ROI
             )) +
  geom_point(data = dados_hemi_v1,
             aes(
               y = log(sqrt(AvgThickness) * TotalArea),
               x = log(ExposedArea),
               color = "hemisphere"
             ),
             alpha = 0.8) +
  labs(
    title = "Aplicacao do modelo de girificacao",
    subtitle = paste(
      "LOBOS: Adj R2 = ",
      signif(summary(fit_lobes_corrected)$adj.r.squared, 5),
      ", Intercept (log(k)) = ",
      signif(tidy(fit_lobes_corrected)[1, 2], 5),
      ",\nSlope ",
      expression(alpha),
      "= ",
      signif(tidy(fit_lobes_corrected)[2, 2], 5),
      ",  p = ",
      signif(glance(fit_lobes_corrected)[1, 5], 5)
    ),
    y = "log 10 At T1/2",
    x = "log Ae",
    color = "ROI"
  ) + geom_smooth(
    data = dados_hemi_v1,
    aes(
      y = log(sqrt(AvgThickness) * TotalArea),
      x = log(ExposedArea),
      color = "hemisphere"
    ),
    alpha = 0.8,
    method = "lm",
    fullrange = TRUE,
    se = FALSE
  ) +
  geom_smooth(
    data = dados_lobos_v1,
    aes(
      y = log(sqrt(AvgThickness) * TotalArea_corrected),
      x = log(SmoothArea_corrected),
      color = "lobes"
    ),
    method = "lm",
    fullrange = TRUE,
    se = FALSE
  ) +
  theme_pubclean()

lobos2 <-
  ggplot(
    filter(
      dados,
      Session == 1,
      Diagnostic == "CONTROLE",
      ROI == "F"|
        ROI == "T"|
        ROI == "O"|
        ROI == "P"|
        ROI == "hemisphere"
    ),
    aes(
      y = log(sqrt(AvgThickness) * TotalArea_corrected),
      x = log(SmoothArea_corrected)
    )
  ) +
  geom_point(aes(color = ROI)) +
  labs(
    title = "Aplicacao do modelo de girificacao - corrigido",
    subtitle = paste(
      "Adj R2 = ",
      signif(summary(fit_lobes_corrected)$adj.r.squared, 5),
      ", Intercept (log(k)) = ",
      signif(tidy(fit_lobes_corrected)[1, 2], 5),
      ",\n Slope ",
      expression(alpha),
      "= ",
      signif(tidy(fit_lobes_corrected)[2, 2], 5),
      ",  p = ",
      signif(glance(fit_lobes_corrected)[1, 5], 3)
    ),
    colour = "ROI",
    y = "log 10 At T1/2",
    x = "log Ae",
    caption = "Somente controles, visita 1, com correcao e ignorando a insula e o corpo caloso"
  ) + geom_smooth(method = "lm") +
  theme_pubclean()


#  TAMANHO DE EFEITO ----
dados_lm_lobes_K <- dados_lobos_v1 %>%
  group_by(Age_interval10, Diagnostic) %>%
  do(fit_lobes_K = aov(K ~ Age * ROI, data = .))

fit_lobes_K <-
  aov(K ~ Age * ROI,
      data = dados_lobos_v1)

# get the coefficients by group GENERO e IDADE----
lobes_K_Coef = tidy(dados_lm_lobes_K,
                    fit_lobes_K ,
                    conf.int = TRUE,
                    conf.level = 0.95)
lobes_K_Coef

lobes_K_Pred = augment(dados_lm_lobes_K, fit_lobes_K)
lobes_K_Pred

lobes_K_R2 = glance(dados_lm_lobes_K, fit_lobes_K)
lobes_K_R2