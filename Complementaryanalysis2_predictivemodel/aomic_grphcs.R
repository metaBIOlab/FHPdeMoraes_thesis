#### HEMI ----

n_distinct(filter(dados_AOMIC, ROI == "hemisphere")$SUBJ)


lm_AOMIC <- lm(1 / 2 * logAvgThickness + logTotalArea_corrected  ~ logExposedArea_corrected ,
               data = filter(dados_AOMIC, ROI == "hemisphere"),
               na.action = na.omit)
tidy(lm_AOMIC, conf.int = TRUE)

ggplot(
  filter(dados_AOMIC, ROI == "hemisphere"),
  aes(
    x = logExposedArea_corrected,
    y = (1 / 2 * logAvgThickness + logTotalArea_corrected)
  )
) + geom_point() + geom_smooth(method = "lm") + stat_cor()


#### LOBOS ----

n_distinct(filter(dados_AOMIC, ROI != "hemisphere")$SUBJ)

ggplot(
  filter(dados_AOMIC, ROI != "hemisphere"),
  aes(
    x = logExposedArea_corrected,
    y = (1 / 2 * logAvgThickness + logTotalArea_corrected)
  )
) + geom_point(aes(color=ROI)) + geom_smooth(method = "lm") + stat_cor()


lm_AOMIC_lobes <- lm(1 / 2 * logAvgThickness + logTotalArea_corrected  ~ logExposedArea_corrected ,
                     data = filter(dados_AOMIC, ROI != "hemisphere"),
                     na.action = na.omit)
tidy(lm_AOMIC_lobes, conf.int = TRUE)

lm_diagnostic_ROI <- filter(dados_AOMIC, ROI != "hemisphere") %>%
  group_by(ROI) %>%
  do(
    fit_diagnostic = lm(
      1 / 2 * logAvgThickness + logTotalArea_corrected  ~ logExposedArea_corrected ,
      data = .,
      na.action = na.omit
    )
  )

diagnostic_Coef_ROI = tidy(lm_diagnostic_ROI,
                           fit_diagnostic,
                           conf.int = TRUE,
                           conf.level = 0.95)

lm_diagnostic <- filter(dados_AOMIC, ROI != "hemisphere") %>%
  group_by(SUBJ) %>%
  do(
    fit_diagnostic = lm(
      1 / 2 * logAvgThickness + logTotalArea_corrected  ~ logExposedArea_corrected ,
      data = .,
      na.action = na.omit
    )
  )

diagnostic_Coef = tidy(lm_diagnostic,
                       fit_diagnostic,
                       conf.int = TRUE,
                       conf.level = 0.95)

ggplot(
  filter(diagnostic_Coef, term == "logExposedArea_corrected"),
  aes(x = estimate, alpha = 0.4)
) + geom_histogram() + geom_vline(xintercept = 1.25, color = "red") + stat_central_tendency(type = "mean") + theme_pubr()

lm_diagnostic <- dados_AOMIC %>%
  group_by(SUBJ) %>%
  do(
    fit_diagnostic = lm(
      1 / 2 * logAvgThickness + logTotalArea_corrected  ~ logExposedArea_corrected ,
      data = .,
      na.action = na.omit
    )
  )

diagnostic_Coef = tidy(lm_diagnostic,
                       fit_diagnostic,
                       conf.int = TRUE,
                       conf.level = 0.95)

ggplot(filter(diagnostic_Coef, term == "logExposedArea_corrected"),
       aes(x = estimate, alpha = 0.4)) + geom_histogram() + geom_vline(xintercept = 1.25, color = "red") + stat_central_tendency(type = "mean") + theme_pubr()

