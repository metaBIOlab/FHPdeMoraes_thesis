### YUJIANG DESCRIPTION

#dados_datasetscomp_ADNI_IDOR$Sample[dados_datasetscomp_ADNI_IDOR$Sample == "ADNIAD" | dados_datasetscomp_ADNI_IDOR$Sample == "ADNIControl" ] <- "ADNI"

filter(dados_datasetscomp, Sample != "Mota&Houzel2015") %>%
  group_by(Sample, Diagnostic) %>%
  summarise(
    N = n_distinct(SUBJ),
    Mean_age = mean(Age),
    Std_age = sd(Age),
    min_age = min(Age),
    max_age = max(Age),
    mean_T= mean(AvgThickness),
    SD_T = sd(AvgThickness),
    Mean_AT = mean(TotalArea),
    SD_AT = sd(TotalArea),
    Mean_AE = mean(ExposedArea),
    SD_AE = sd(ExposedArea),
    mean_K = mean(K),
    sd_K = sd(K),
    mean_S = mean(S),
    sd_S = sd(S),
    mean_I = mean(I),
    sd_I = sd(I)
) %>% t() %>% kable(digits = 2) %>% kable_styling()

### -COMPARACAO ---


lm_amostras <- filter(dados_datasetscomp, Sample != "Mota&Houzel2015") %>%
  group_by(Sample) %>%
  do(fit_amostras = lm(1/2 * logAvgThickness + logTotalArea ~ logExposedArea, data = ., na.action = na.omit))

amostras_Coef = tidy(lm_amostras,
                     fit_amostras,
                     conf.int = TRUE,
                     conf.level = 0.95)

amostras_Coef %>% kable(digits = 4) %>% kable_styling()

ggplot(
  data = filter(amostras_Coef, term == "logExposedArea"),
  aes(
    x = reorder(Sample, estimate),
    y = estimate,
    color = Sample
  )
) +
  geom_point() + geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) + geom_hline(yintercept = 1.25, linetype = "dashed")  + stat_compare_means(method = "kruskal.test") + theme_pubr() + labs(y = "slope")


