# K, S and I  TRAJECTORIES INCLUDING ALL DATASETS

dados_datasetscomp$Sample[dados_datasetscomp$Sample == "ADNIControl"] <- "ADNI"
dados_datasetscomp$Sample[dados_datasetscomp$Sample == "ADNIAD"] <- "ADNI"
dados_datasetscomp$Sample[dados_datasetscomp$Sample == "OASIS_healthy"] <- "OASIS"


dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "CONTROLE"] <- "CTL"
dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "ALZ"] <- "AD"



dados_datasetscomp_lobos <- filter(dados_datasetscomp, ROI == "F" | ROI == "O" | ROI == "P" | ROI == "T")
dados_datasetscomp_hemi <- filter(dados_datasetscomp, ROI != "F" | ROI != "O" | ROI != "P" | ROI != "T")


lm_diagnostic <- dados_datasetscomp_hemi %>%
  group_by(Sample, Diagnostic) %>%
  do(
    fit_diagnostic = lm(
      1 / 2 * logAvgThickness + logTotalArea ~ logExposedArea,
      data = .,
      na.action = na.omit
    )
  )

diagnostic_Coef = tidy(lm_diagnostic,
                            fit_diagnostic,
                            conf.int = TRUE,
                            conf.level = 0.95)

diagnostic_Coef

ggplot(
  data = filter(diagnostic_Coef, term == "logExposedArea"),
  aes(
    y = estimate, x = Sample,
    color = Diagnostic,
    fill = Diagnostic,
    alpha = 0.4
  )
) + geom_boxplot() + geom_hline(yintercept = 1.25) + theme_pubr()



ggplot(filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
       aes(x = Age, y = K,  color = Sample)) + geom_point() + geom_smooth() +
  theme_pubr() + facet_grid(Diagnostic ~ .)

ggplot(filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
       aes(x = Age, y = S,  color = Sample)) + geom_point() + geom_smooth() +
  theme_pubr() + facet_grid(Diagnostic ~ .)

ggplot(filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
       aes(x = Age, y = I,  color = Sample)) + geom_point() + geom_smooth() +
  theme_pubr() + facet_grid(Diagnostic ~ .)

ggplot(
  filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
  aes(x = Age, y = K,  color = Sample)
) + stat_summary(fun.data = "mean_cl_normal",
                 geom = "errorbar",
                 width = .4) +
  stat_summary(fun.y = "mean", geom = "point") + geom_smooth() +
  theme_pubr() + facet_grid(Gender ~ Diagnostic) 

ggplot(
  filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
  aes(x = Age, y = S,  color = Sample)
)  + stat_summary(fun.data = "mean_cl_normal",
                  geom = "errorbar",
                  width = .4) +
  stat_summary(fun.y = "mean", geom = "point") + geom_smooth() +
  theme_pubr() + facet_grid(Gender ~ Diagnostic)

ggplot(
  filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
  aes(x = Age, y = I,  color = Sample)
)  + stat_summary(fun.data = "mean_cl_normal",
                  geom = "errorbar",
                  width = .4) +
  stat_summary(fun.y = "mean", geom = "point") + geom_smooth() +
  theme_pubr() + facet_grid(Gender ~ Diagnostic)

ggplot(filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
       aes(x = K,  color = Age_interval10)) + geom_density() +
  theme_pubr() + facet_grid(Diagnostic ~ .)

ggplot(filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
       aes(x = S,  color = Age_interval10)) + geom_density() +
  theme_pubr() + facet_grid(Diagnostic ~ .)

ggplot(filter(dados_datasetscomp, Sample != "Mota&Houzel2015"),
       aes(x = I,  color = Age_interval10)) + geom_density() +
  theme_pubr() + facet_grid(Diagnostic ~ .)
