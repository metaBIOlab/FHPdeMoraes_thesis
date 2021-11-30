# COMPARA AS MEDIDAS ESTRUTURAIS ----
aov_AreaT_diag <-
  aov(TotalArea ~ Diagnostic * Gender, data = dados_hemi_v1)

aov_AreaE_diag <-
  aov(SmoothArea ~ Diagnostic * Gender, data = dados_hemi_v1)

aov_AvgThick_diag <-
  aov(AvgThickness ~ Diagnostic * Gender, data = dados_hemi_v1)

aov_lGI_diag <-
  aov(localGI ~ Diagnostic * Gender, data = dados_hemi_v1)

aov_gen_diag <-
  aov(log_kteorico ~ Diagnostic * Gender, dados_hemi_v1)

anova_stats(aov_gen_diag)
# plots de comparacao - boxplot ----

bxplt_AreaT_diagnostic_Gender <-
  ggplot(data = dados_hemi_v1, aes(Diagnostic, TotalArea, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "Total grey matter surface area",
    subtitle = paste("diagnostic:Gender ANOVA p = ",
                     signif(tidy(aov_AreaT_diag)$p.value[3], 5)),
    x = "Gender",
    y = "Total Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(method = "t.test", label = "p.signif")

bxplt_AreaE_diagnostic_Gender <-
  ggplot(data = dados_hemi_v1, aes(Diagnostic, SmoothArea, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "Exposed grey matter surface area",
    subtitle = paste("diagnostic:Gender ANOVA p = ",
                     signif(tidy(aov_AreaE_diag)$p.value[3], 5)),
    x = "Gender",
    y = "Smooth Area (mm^2)"
  ) +
  theme_pubclean() + stat_compare_means(method = "t.test", label = "p.signif")


bxplt_AvgThick_diagnostic_Gender <-
  ggplot(data = dados_hemi_v1,
         aes(Diagnostic, AvgThickness, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "Average Cortical Thickness",
    subtitle = paste("diagnostic:Gender ANOVA p = ",
                     signif(tidy(aov_AvgThick_diag)$p.value[3], 5)),
    x = "Gender",
    y = "Average Thickness (mm)"
  ) +
  theme_pubclean() + stat_compare_means(method = "t.test", label = "p.signif")

bxplt_lGI_diagnostic_Gender <-
  ggplot(data = dados_hemi_v1, aes(Diagnostic, localGI, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "Local Gyrification Index",
    subtitle = paste("diagnostic:Gender ANOVA p = ",
                     signif(tidy(aov_lGI_diag)$p.value[3], 5)),
    x = "Gender",
    y = "local Gyrification Index"
  ) +
  theme_pubclean() + stat_compare_means(method = "t.test", label = "p.signif")


bxplt_logk_diagnostic_Gender <-
  ggplot(data = dados_hemi_v1, aes(Diagnostic, log_kteorico, color = Gender)) +
  geom_boxplot() +
  labs(
    title = "log Offset k",
    subtitle = paste("diagnostic:Gender ANOVA p = ",
                     signif(tidy(aov_gen_diag)$p.value[3], 5)),
    x = "Gender",
    y = "log Offset k"
  ) +
  theme_pubclean() + stat_compare_means(method = "t.test", label = "p.signif")

figure_Gender_diag <-
  ggarrange(
    bxplt_AreaT_diagnostic_Gender,
    bxplt_AreaE_diagnostic_Gender,
    bxplt_AvgThick_diagnostic_Gender,
    bxplt_lGI_diagnostic_Gender,
    labels = c("A", "B", "C", "D"),
    common.legend = TRUE
  ) + ggsave("Gender_diag_boxplot.png", width = 7 , height = 8.6)

figure_Gender_diag2 <-  ggarrange(
  bxplt_logk_diagnostic_Gender,
  model_facet,
  labels = c("E", "F"),
  common.legend = TRUE
) + ggsave("Gender_diag_boxplot_logk_model_facet.png",
           width = 7,
           height = 4.3)
