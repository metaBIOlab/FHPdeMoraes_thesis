# ANALISE LOBOS DIAGNOSTICO ----


# LOBOS IDADE ----

aov_diag_ROI <-
  aov(
    K ~ Age * Diagnostic * ROI,
    data = dados_lobos_v1
  )
tidy(aov_diag_ROI) %>% kable %>% kable_styling()
glance(aov_diag_ROI)

lobos3 <- ggplot(
  data = dados_lobos_v1,
  aes(Diagnostic, log_kteorico_corrected, color = Diagnostic)
) +
  geom_boxplot() +
  labs(x = "Lobos",
       y = "log Offset (k)",
       colour = "Diagnostico") + theme_pubclean() + facet_wrap(~ ROI, nrow = 2, ncol = 2) +
  stat_compare_means(
    comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
    label = "p.signif",
    hide.ns = TRUE,
    label.y = c(-0.28,-0.2,-0.12)
  ) + ylim(-0.6, -0.1)

lobos4 <- ggplot(
  data = filter(dados_lobos_v1,  ROI == "F"|
                  ROI == "T"| ROI == "O"| ROI == "P"),
  aes(x = Diagnostic, y = log_kteorico_corrected, color = Diagnostic)
) +
  geom_boxplot() +
  labs(
    title = "Comparando o Offset k por ROI",
    x = "Diagnostico",
    y = "log Offset (k)",
    colour = "Diagnostico"
  ) + theme_pubclean() +
  stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                     label = "p.signif") + facet_grid(. ~ ROI)  
#+ ggsave("lobos_diag_pvalor.png", width = 13, height = 5.3)

lobos4_age_corr <- ggplot(
  data = filter(dados_lobos_v1,  ROI == "F"| ROI == "T"| ROI == "O"| ROI == "P"),
  aes(Diagnostic, K_corr_decay, color = Diagnostic)
) +
  geom_boxplot() +
  labs(
    title = "Comparando o Offset k por ROI - DEAGED 25 yo",
    x = "Diagnostic",
    y = "log Offset (k)",
    colour = "Diagnostic"
  ) + theme_pubclean()  +
  stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                     label = "p.signif") + facet_grid(. ~ ROI)

# mesma coisa do de cima mas com Gender ----

ggplot(
  data = filter(
    dados,
    RM_Maquina == "Philips",
    Session == 1,
    ROI == "F"|
      ROI == "T"| ROI == "O"| ROI == "P"| ROI == "hemisphere"
  ),
  aes(Diagnostic, log_kteorico_corrected, color = Diagnostic)
) +
  geom_boxplot() +
  labs(
    title = "Comparando o Offset k por ROI",
    subtitle = paste("Diagnostic:ROI ANOVA p = ",
                     signif(tidy(aov_diag_ROI)$p.value[6], 5)),
    x = "Lobos",
    y = "log Offset (k)",
    colour = "Diagnostico"
  ) + theme_pubclean()  +
  stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                     label = "p.signif") + facet_grid(Gender ~ ROI)

# outras medidas ----

# + ggsave("lobos_diag.png", width = 7, height = 5)

aov_diag_ROI_thick <-
  aov(
    AvgThickness ~ Age * Diagnostic * ROI,
    data = dados_lobos_v1)

tidy(aov_diag_ROI_thick)
glance(aov_diag_ROI_thick)

lobos4_thick <- ggplot(
  data = dados_lobos_v1,
  aes(Diagnostic, AvgThickness, color = Diagnostic)
) +
  geom_boxplot() +
  labs(
    title = "Comparando a espessura pelo diagnostico",
    x = "Lobes",
    y = "Avg Thickness",
    colour = "Diagnostic"
  ) + theme_pubclean() +
  stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                     label = "p.signif") + facet_grid(. ~ ROI)

aov_diag_ROI_areat <-
  aov(
    TotalArea_corrected ~ Age * Diagnostic * ROI,
    data = dados_lobos_v1
  )
tidy(aov_diag_ROI_areat)
glance(aov_diag_ROI_areat)

lobos4_areat <- ggplot(
  data = dados_lobos_v1,
  aes(Diagnostic, TotalArea_corrected, color = Diagnostic)
) +
  geom_boxplot() +
  labs(
    title = "Comparando a area total pelo diagnostico",
    x = "Lobes",
    y = "Total Area",
    colour = "Diagnostic"
  ) + theme_pubclean() +
  stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                     label = "p.signif") + facet_grid(. ~ ROI) 

aov_diag_ROI_areae <-
  aov(
    SmoothArea_corrected ~ Age * Diagnostic * ROI,
    data = dados_lobos_v1)

tidy(aov_diag_ROI_areae)
glance(aov_diag_ROI_areae)

lobos4_areae <- ggplot(
  data = dados_lobos_v1,
  aes(Diagnostic, SmoothArea_corrected, color = Diagnostic)
) +
  geom_boxplot() +
  labs(
    title = "Comparando a area exposta pelo diagnostico",
    x = "Lobes",
    y = "Area exposta",
    colour = "Diagnostic"
  ) + theme_pubclean() +
  stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                     label = "p.signif") + facet_grid(. ~ ROI)

aov_diag_ROI_lGI <-
  aov(
    localGI ~ Age * Diagnostic * ROI,
    data = dados_lobos_v1)

tidy(aov_diag_ROI_lGI)
glance(aov_diag_ROI_lGI)

lobos4_lGI <- ggplot(
  data = dados_lobos_v1,
  aes(Diagnostic, localGI, color = Diagnostic)
) +
  geom_boxplot() +
  labs(
    title = "Comparando o indice de girifcacao pelo diagnostico",
    x = "Lobes",
    y = "lGI",
    colour = "Diagnostic"
  ) + theme_pubclean() +
  stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                     label = "p.signif") + facet_grid(. ~ ROI) 