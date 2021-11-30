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
      logAvgThickness - b_t_HCP500r - lambda_t_HCP500r * Age,
      ifelse(
        Sample == "NKI",
        logAvgThickness - b_t_NKI - lambda_t_NKI * Age,
        ifelse(
          Sample == "OASIS_healthy",
          logAvgThickness - b_t_OASIS_healthy - lambda_t_OASIS_healthy * Age,
            ifelse(
              Sample == "ADNIAD" | Sample == "ADNIControl",
              logAvgThickness - b_t_ADNIControl - lambda_t_ADNIControl * Age,
              ""
            )
          )
        )
      )
    ),
    logTotalArea_age_decay = as.numeric(
      ifelse(
        Sample == "HCP500r",
        logTotalArea - b_At_HCP500r - lambda_At_HCP500r * Age,
        ifelse(
          Sample == "NKI",
          logTotalArea - b_At_NKI - lambda_At_NKI * Age,
          ifelse(
            Sample == "OASIS_healthy",
            logTotalArea - b_At_OASIS_healthy - lambda_At_OASIS_healthy * Age,
              ifelse(
                Sample == "ADNIAD" | Sample == "ADNIControl",
                logTotalArea - b_At_ADNIControl - lambda_At_ADNIControl * Age,
                ""
              )
            )
          )
        )
      ),
    logExposedArea_age_decay = as.numeric(
      ifelse(
        Sample == "HCP500r",
        logExposedArea - b_Ae_HCP500r - lambda_Ae_HCP500r * Age,
        ifelse(
          Sample == "NKI",
          logExposedArea - b_Ae_NKI - lambda_Ae_NKI * Age,
          ifelse(
            Sample == "OASIS_healthy",
            logExposedArea - b_Ae_OASIS_healthy - lambda_Ae_OASIS_healthy * Age,
              ifelse(
                Sample == "ADNIAD" | Sample == "ADNIControl",
                logExposedArea - b_Ae_ADNIControl - lambda_Ae_ADNIControl * Age,
                ""
              )
            )
          )
        )
      )
    )

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
