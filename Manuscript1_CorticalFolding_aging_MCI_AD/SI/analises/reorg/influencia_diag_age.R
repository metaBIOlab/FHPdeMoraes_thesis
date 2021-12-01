# COMPARA AS MEDIDAS ESTRUTURAIS ----
aov_AreaT_diag <-
  aov(TotalArea ~ Diagnostic * Age, data = dados_hemi_v1)

aov_AreaE_diag <-
  aov(ExposedArea ~ Diagnostic * Age, data = dados_hemi_v1)

aov_AvgThick_diag <-
  aov(AvgThickness ~ Diagnostic * Age, dados_hemi_v1)

aov_lGI_diag <-
  aov(localGI ~ Diagnostic * Age, dados_hemi_v1)

aov_age_diag <-
  aov(K ~ Diagnostic * Age, dados_hemi_v1)

anova_stats(aov_age_diag)

lm_age_diag <- lm(K ~ Diagnostic * Age, dados_hemi_v1)
anova(lm_age_diag)
# plots de comparacao - boxplot ----

bxplt_AreaT_diagnostic <-
  ggplot(data = dados_hemi_v1,
         aes(Diagnostic, TotalArea, color = Diagnostic)) +
  geom_boxplot() +
  labs(
    title = "Total grey matter surface area",
    subtitle = paste("ANOVA p = ",
                     signif(tidy(aov_AreaT_diag)$p.value[3], 5)),
    x = "Diagnostic",
    y = "Total Area (mm^2)"
  ) +
  theme_pubclean() + facet_grid(. ~ Age_interval10) + stat_compare_means(method = "t.test", label = "p.signif", comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")))

bxplt_AreaE_diagnostic <-
  ggplot(data = dados_hemi_v1, aes(Diagnostic, ExposedArea, color = Diagnostic)) +
  geom_boxplot() +
  labs(
    title = "Exposed grey matter surface area",
    subtitle = paste("ANOVA p = ",
                     signif(tidy(aov_AreaE_diag)$p.value[3], 5)),
    x = "Diagnostic",
    y = "Smooth Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")), label = "p.signif") + facet_grid(. ~ Age_interval10)


bxplt_AvgThick_diagnostic <-
  ggplot(data = dados_hemi_v1,
         aes(Diagnostic, AvgThickness, color = Diagnostic)) +
  geom_boxplot() +
  labs(
    title = "Average Cortical Thickness",
    subtitle = paste("ANOVA p = ",
                     signif(tidy(aov_AvgThick_diag)$p.value[3], 5)),
    x = "Diagnostic",
    y = "AvgThickness",
    tag = "C"
  ) +
  theme_pubclean() + stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")), label = "p.signif") + facet_grid(. ~ Age_interval10)

bxplt_lGI_diagnostic <-
  ggplot(data = dados_hemi_v1, aes(Diagnostic, localGI, color = Diagnostic)) +
  geom_boxplot() +
  labs(
    title = "Local Gyrification Index",
    subtitle = paste("ANOVA p = ",
                     signif(tidy(aov_lGI_diag)$p.value[3], 5)),
    x = "Diagnostic",
    y = "local Gyrification Index",
    tag = "D"
  ) +
  theme_pubclean() + stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                                     label = "p.signif") + facet_grid(. ~ Age_interval10)

bxplt_logk_diagnostic <-
  ggplot(data = dados_hemi_v1, aes(Diagnostic, K, color = Diagnostic)) +
  geom_boxplot() +
  labs(
    title = "Log k teorico",
    subtitle = paste("ANOVA p = ",
                     signif(tidy(aov_age_diag)$p.value[3], 5)),
    x = "Diagnostic",
    y = "loog k teorico",
    tag = "E"
  ) +
  theme_pubclean() + stat_compare_means(comparisons = list(c("CONTROLE", "CCL"), c("CONTROLE", "ALZ"), c("CCL", "ALZ")),
                                     label = "p.signif") + facet_grid(. ~ Age_interval10)
