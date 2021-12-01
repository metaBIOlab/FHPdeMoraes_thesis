# analise da variancia

source("analises/variancia_test.R")

p_yellow1 <- pnorm(dados$z_K_corr_decay,mean(dados$kteorico_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"]), sd(dados$kteorico_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"]))    


ggplot(dados_hemi_v1, aes(x = z_K_corr_decay, fill = Diagnostic)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black") +
  stat_function(fun = dnorm, args = list(
    mean = mean(dados$kteorico_corr_decay[dados$Diagnostic == "CONTROLE"&
                                                dados$ROI == "hemisphere"]),
    sd = sd(dados$kteorico_corr_decay[dados$Diagnostic == "CONTROLE"&
                                            dados$ROI == "hemisphere"])
  ))

ggplot(
  filter(
    dados,
    ROI == "hemisphere"|
      ROI == "O"| ROI == "P"| ROI == "T"| ROI == "F"
  ),
  mapping = aes(x = Diagnostic,
                y = z,
                color = Diagnostic)
) +
  geom_boxplot() +
  labs(title = "Comparando o offset log(k) para cada diagnostico",
       x = "Diagnostico",
       y = "z") +
  theme_pubclean() + facet_grid(. ~ ROI) +
  stat_compare_means(comparisons = my_comparisons)


ggplot(
  filter(
    dados,
    ROI == "hemisphere"|
      ROI == "O"| ROI == "P"| ROI == "T"| ROI == "F"
  ),
  mapping = aes(x = z,
                color = Diagnostic)
) +
  geom_density() +
  labs(title = "Comparando o offset log(k) para cada diagnostico",
       x = "Diagnostico",
       y = "z") +
  theme_pubclean() + facet_grid(ROI ~ .)



ggplot(
  filter(dados,
         ROI == "hemisphere"),
  mapping = aes(
    x = z,
    fill = Diagnostic,
    alpha = 0.5,
    color = Diagnostic
  )
) +
  geom_histogram(binwidth = 0.05) +
  theme_pubclean()