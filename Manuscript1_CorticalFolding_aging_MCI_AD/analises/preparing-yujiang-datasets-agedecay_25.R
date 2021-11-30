# logTotalArea

datasets_yujiang <- filter(datasets_yujiang, TotalArea != 0)

decay_YW_TotalArea <- datasets_yujiang  %>%
  group_by(Sample) %>%
  do(fit_decay_YW_TotalArea = rlm(logTotalArea ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_YW_TotalArea_Coef = tidy(
  decay_YW_TotalArea,
  fit_decay_YW_TotalArea,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda----

lambda_At_HCP500r <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "HCP500r" &
                                     decay_YW_TotalArea_Coef$term == "Age"]

lambda_At_NKI <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "NKI" &
                                     decay_YW_TotalArea_Coef$term == "Age"]

lambda_At_OASIS_healthy <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "OASIS_healthy" &
                                     decay_YW_TotalArea_Coef$term == "Age"]

lambda_At_ADNIAD <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "ADNIAD" &
                                     decay_YW_TotalArea_Coef$term == "Age"]

lambda_At_ADNIControl <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "ADNIControl" &
                                     decay_YW_TotalArea_Coef$term == "Age"]


b_At_HCP500r <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "HCP500r" &
                                     decay_YW_TotalArea_Coef$term == "(Intercept)"]

b_At_NKI <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "NKI" &
                                     decay_YW_TotalArea_Coef$term == "(Intercept)"]

b_At_OASIS_healthy <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "OASIS_healthy" &
                                     decay_YW_TotalArea_Coef$term == "(Intercept)"]

b_At_ADNIAD <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "ADNIAD" &
                                     decay_YW_TotalArea_Coef$term == "(Intercept)"]

b_At_ADNIControl <-
  decay_YW_TotalArea_Coef$estimate[decay_YW_TotalArea_Coef$Sample == "ADNIControl" &
                                     decay_YW_TotalArea_Coef$term == "(Intercept)"]

# logAvgThickness

decay_YW_logAvgThickness <- datasets_yujiang  %>%
  group_by(Sample) %>%
  do(fit_decay_YW_logAvgThickness = rlm(logAvgThickness ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_YW_logAvgThickness_Coef = tidy(
  decay_YW_logAvgThickness,
  fit_decay_YW_logAvgThickness,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda----

lambda_t_HCP500r <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "HCP500r" &
                                           decay_YW_logAvgThickness_Coef$term == "Age"]

lambda_t_NKI <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "NKI" &
                                           decay_YW_logAvgThickness_Coef$term == "Age"]

lambda_t_OASIS_healthy <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "OASIS_healthy" &
                                           decay_YW_logAvgThickness_Coef$term == "Age"]

lambda_t_ADNIAD <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "ADNIAD" &
                                           decay_YW_logAvgThickness_Coef$term == "Age"]

lambda_t_ADNIControl <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "ADNIControl" &
                                           decay_YW_logAvgThickness_Coef$term == "Age"]

b_t_HCP500r <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "HCP500r" &
                                           decay_YW_logAvgThickness_Coef$term == "(Intercept)"]

b_t_NKI <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "NKI" &
                                           decay_YW_logAvgThickness_Coef$term == "(Intercept)"]

b_t_OASIS_healthy <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "OASIS_healthy" &
                                           decay_YW_logAvgThickness_Coef$term == "(Intercept)"]

b_t_ADNIAD <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "ADNIAD" &
                                           decay_YW_logAvgThickness_Coef$term == "(Intercept)"]

b_t_ADNIControl <-
  decay_YW_logAvgThickness_Coef$estimate[decay_YW_logAvgThickness_Coef$Sample == "ADNIControl" &
                                           decay_YW_logAvgThickness_Coef$term == "(Intercept)"]

# logExposedArea

decay_YW_logExposedArea <- datasets_yujiang  %>%
  group_by(Sample) %>%
  do(fit_decay_YW_logExposedArea = rlm(logExposedArea ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_YW_logExposedArea_Coef = tidy(
  decay_YW_logExposedArea,
  fit_decay_YW_logExposedArea,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda----

lambda_Ae_HCP500r <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "HCP500r" &
                                          decay_YW_logExposedArea_Coef$term == "Age"]

lambda_Ae_NKI <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "NKI" &
                                          decay_YW_logExposedArea_Coef$term == "Age"]

lambda_Ae_OASIS_healthy <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "OASIS_healthy" &
                                          decay_YW_logExposedArea_Coef$term == "Age"]

lambda_Ae_ADNIAD <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "ADNIAD" &
                                          decay_YW_logExposedArea_Coef$term == "Age"]

lambda_Ae_ADNIControl <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "ADNIControl" &
                                          decay_YW_logExposedArea_Coef$term == "Age"]

b_Ae_HCP500r <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "HCP500r" &
                                          decay_YW_logExposedArea_Coef$term == "(Intercept)"]

b_Ae_NKI <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "NKI" &
                                          decay_YW_logExposedArea_Coef$term == "(Intercept)"]

b_Ae_OASIS_healthy <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "OASIS_healthy" &
                                          decay_YW_logExposedArea_Coef$term == "(Intercept)"]

b_Ae_ADNIAD <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "ADNIAD" &
                                          decay_YW_logExposedArea_Coef$term == "(Intercept)"]

b_Ae_ADNIControl <-
  decay_YW_logExposedArea_Coef$estimate[decay_YW_logExposedArea_Coef$Sample == "ADNIControl" &
                                          decay_YW_logExposedArea_Coef$term == "(Intercept)"]

# mutate new decayed variables ----

datasets_yujiang <-
  datasets_yujiang %>% mutate(logAvgThickness_age_decay = as.numeric(
    ifelse(
      Sample == "HCP500r",
      logAvgThickness - lambda_t_HCP500r * (Age - Age.cor),
      ifelse(
        Sample == "NKI",
        logAvgThickness - lambda_t_NKI * (Age - Age.cor),
        ifelse(
          Sample == "OASIS_healthy",
          logAvgThickness - lambda_t_OASIS_healthy * (Age - Age.cor),
            ifelse(
              Sample == "ADNIAD" | Sample == "ADNIControl",
              logAvgThickness - lambda_t_ADNIControl * (Age - Age.cor),
              ""
            )
          )
        )
      )
    ),
    logTotalArea_age_decay = as.numeric(
      ifelse(
        Sample == "HCP500r",
        logTotalArea - lambda_At_HCP500r * (Age - Age.cor),
        ifelse(
          Sample == "NKI",
          logTotalArea - lambda_At_NKI * (Age - Age.cor),
          ifelse(
            Sample == "OASIS_healthy",
            logTotalArea - lambda_At_OASIS_healthy * (Age - Age.cor),
              ifelse(
                Sample == "ADNIAD" | Sample == "ADNIControl",
                logTotalArea - lambda_At_ADNIControl * (Age - Age.cor),
                ""
              )
            )
          )
        )
      ),
    logExposedArea_age_decay = as.numeric(
      ifelse(
        Sample == "HCP500r",
        logExposedArea - lambda_Ae_HCP500r * (Age - Age.cor),
        ifelse(
          Sample == "NKI",
          logExposedArea - lambda_Ae_NKI * (Age - Age.cor),
          ifelse(
            Sample == "OASIS_healthy",
            logExposedArea - lambda_Ae_OASIS_healthy * (Age - Age.cor),
            ifelse(
              Sample == "ADNIAD",
              logExposedArea - lambda_Ae_ADNIAD * (Age - Age.cor),
              ifelse(
                Sample == "ADNIAD" | Sample == "ADNIControl",
                logExposedArea - lambda_Ae_ADNIControl * (Age - Age.cor),
                ""
              )
            )
          )
        )
      )
    ))

# K ----

datasets_yujiang <- datasets_yujiang %>%
  mutate(
    K_age_decay = (
      logTotalArea_age_decay + (1 / 2) * logAvgThickness_age_decay -
        ((5 / 4) * logExposedArea_age_decay)
    ),
    I_age_decay = logTotalArea_age_decay + logExposedArea_age_decay + logAvgThickness_age_decay ^
      2,
    S_age_decay = 3 / 2 * logTotalArea_age_decay + 3 / 4 * logExposedArea_age_decay - 9 /
      4 * logAvgThickness_age_decay ^ 2
  )
