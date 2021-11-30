## calculo Z score

datasets_yujiang_means_sd <- datasets_yujiang %>%
  pivot_longer(
    c(
      K,
      K_age_decay,
      logAvgThickness,
      logAvgThickness_age_decay,
      S,
      S_age_decay,
      I,
      I_age_decay
    ),
    names_to = "morphological_parameter",
    values_to = "morphological_parameter_value"
  ) %>% filter(!is.na(morphological_parameter_value),
               !is.nan(morphological_parameter_value)) %>%
  group_by(Sample, Diagnostic, morphological_parameter) %>%
  summarise(
    mean.morphological_parameter_value = mean(morphological_parameter_value),
    sd.morphological_parameter_value = sd(morphological_parameter_value)
  )


datasets_yujiang <-
  datasets_yujiang %>% mutate(
    K.zscore = as.numeric(
      ifelse(
        datasets_yujiang$Sample == "ADNIAD" |
          datasets_yujiang$Sample == "ADNIControl",
        (
          K - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                             datasets_yujiang_means_sd$morphological_parameter == "K"]
        ) /
          datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                       datasets_yujiang_means_sd$morphological_parameter == "K"],
        ifelse(
          datasets_yujiang$Sample == "HCP500r",
          (
            K - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "K"]
          ) /
            datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                         datasets_yujiang_means_sd$morphological_parameter == "K"],
          ifelse(
            datasets_yujiang$Sample == "NKI",
            (
              K - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NKI" &
                                                                                 datasets_yujiang_means_sd$morphological_parameter == "K"]
            ) /
              datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NKI" &
                                                                           datasets_yujiang_means_sd$morphological_parameter == "K"],
            ifelse(
              ifelse(
                datasets_yujiang$Sample == "OASIS_healthy",
                (
                  K - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                                     datasets_yujiang_means_sd$morphological_parameter == "K"]
                ) /
                  datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "K"],
                ""
              )
            )
          )
        )
      )
    ),
    K_age_decay.zscore = as.numeric(
      ifelse(
        datasets_yujiang$Sample == "ADNIAD" |
          datasets_yujiang$Sample == "ADNIControl",
        (
          K - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                             datasets_yujiang_means_sd$morphological_parameter == "K_age_decay"]
        ) /
          datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                       datasets_yujiang_means_sd$morphological_parameter == "K_age_decay"],
        ifelse(
          datasets_yujiang$Sample == "HCP500r",
          (
            K - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "K_age_decay"]
          ) /
            datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                         datasets_yujiang_means_sd$morphological_parameter == "K_age_decay"],
          ifelse(
            datasets_yujiang$Sample == "NKI",
            (
              K - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NKI" &
                                                                                 datasets_yujiang_means_sd$morphological_parameter == "K_age_decay"]
            ) /
              datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NKI" &
                                                                           datasets_yujiang_means_sd$morphological_parameter == "K_age_decay"],
            ifelse(
              ifelse(
                datasets_yujiang$Sample == "OASIS_healthy",
                (
                  K - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                                     datasets_yujiang_means_sd$morphological_parameter == "K_age_decay"]
                ) /
                  datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "K_age_decay"],
                ""
              )
            )
          )
        )
      )
    ),
    logAvgThickness.zscore = as.numeric(
      ifelse(
        datasets_yujiang$Sample == "ADNIAD" |
          datasets_yujiang$Sample == "ADNIControl",
        (
          logAvgThickness - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                                           datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness"]
        ) /
          datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                       datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness"],
        ifelse(
          datasets_yujiang$Sample == "HCP500r",
          (
            logAvgThickness - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                                             datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness"]
          ) /
            datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                         datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness"],
          ifelse(
            datasets_yujiang$Sample == "NlogAvgThicknessI",
            (
              logAvgThickness - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NlogAvgThicknessI" &
                                                                                               datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness"]
            ) /
              datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NlogAvgThicknessI" &
                                                                           datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness"],
            ifelse(
              ifelse(
                datasets_yujiang$Sample == "OASIS_healthy",
                (
                  logAvgThickness - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                                                   datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness"]
                ) /
                  datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness"],
                ""
              )
            )
          )
        )
      )
    ),
    logAvgThickness_age_decay.zscore = as.numeric(
      ifelse(
        datasets_yujiang$Sample == "ADNIAD" |
          datasets_yujiang$Sample == "ADNIControl",
        (
          logAvgThickness - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                                           datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
        ) /
          datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                       datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
        ifelse(
          datasets_yujiang$Sample == "HCP500r",
          (
            logAvgThickness - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                                             datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
          ) /
            datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                         datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
          ifelse(
            datasets_yujiang$Sample == "NlogAvgThicknessI",
            (
              logAvgThickness - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NlogAvgThicknessI" &
                                                                                               datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
            ) /
              datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NlogAvgThicknessI" &
                                                                           datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
            ifelse(
              ifelse(
                datasets_yujiang$Sample == "OASIS_healthy",
                (
                  logAvgThickness - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                                                   datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                ) /
                  datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                ""
              )
            )
          )
        )
      )
    ),
    I.zscore = as.numeric(
      ifelse(
        datasets_yujiang$Sample == "ADNIAD" |
          datasets_yujiang$Sample == "ADNIControl",
        (
          I - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                             datasets_yujiang_means_sd$morphological_parameter == "I"]
        ) /
          datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                       datasets_yujiang_means_sd$morphological_parameter == "I"],
        ifelse(
          datasets_yujiang$Sample == "HCP500r",
          (
            I - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "I"]
          ) /
            datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                         datasets_yujiang_means_sd$morphological_parameter == "I"],
          ifelse(
            datasets_yujiang$Sample == "NII",
            (
              I - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NII" &
                                                                                 datasets_yujiang_means_sd$morphological_parameter == "I"]
            ) /
              datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NII" &
                                                                           datasets_yujiang_means_sd$morphological_parameter == "I"],
            ifelse(
              ifelse(
                datasets_yujiang$Sample == "OASIS_healthy",
                (
                  I - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                                     datasets_yujiang_means_sd$morphological_parameter == "I"]
                ) /
                  datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "I"],
                ""
              )
            )
          )
        )
      )
    ),
    I_age_decay.zscore = as.numeric(
      ifelse(
        datasets_yujiang$Sample == "ADNIAD" |
          datasets_yujiang$Sample == "ADNIControl",
        (
          I - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                             datasets_yujiang_means_sd$morphological_parameter == "I_age_decay"]
        ) /
          datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                       datasets_yujiang_means_sd$morphological_parameter == "I_age_decay"],
        ifelse(
          datasets_yujiang$Sample == "HCP500r",
          (
            I - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "I_age_decay"]
          ) /
            datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                         datasets_yujiang_means_sd$morphological_parameter == "I_age_decay"],
          ifelse(
            datasets_yujiang$Sample == "NII",
            (
              I - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NII" &
                                                                                 datasets_yujiang_means_sd$morphological_parameter == "I_age_decay"]
            ) /
              datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NII" &
                                                                           datasets_yujiang_means_sd$morphological_parameter == "I_age_decay"],
            ifelse(
              ifelse(
                datasets_yujiang$Sample == "OASIS_healthy",
                (
                  I - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                                     datasets_yujiang_means_sd$morphological_parameter == "I_age_decay"]
                ) /
                  datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "I_age_decay"],
                ""
              )
            )
          )
        )
      )
    ),
    S.zscore = as.numeric(
      ifelse(
        datasets_yujiang$Sample == "ADNIAD" |
          datasets_yujiang$Sample == "ADNIControl",
        (
          S - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                             datasets_yujiang_means_sd$morphological_parameter == "S"]
        ) /
          datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                       datasets_yujiang_means_sd$morphological_parameter == "S"],
        ifelse(
          datasets_yujiang$Sample == "HCP500r",
          (
            S - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "S"]
          ) /
            datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                         datasets_yujiang_means_sd$morphological_parameter == "S"],
          ifelse(
            datasets_yujiang$Sample == "NSI",
            (
              S - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NSI" &
                                                                                 datasets_yujiang_means_sd$morphological_parameter == "S"]
            ) /
              datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NSI" &
                                                                           datasets_yujiang_means_sd$morphological_parameter == "S"],
            ifelse(
              ifelse(
                datasets_yujiang$Sample == "OASIS_healthy",
                (
                  S - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                                     datasets_yujiang_means_sd$morphological_parameter == "S"]
                ) /
                  datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "S"],
                ""
              )
            )
          )
        )
      )
    ),
    S_age_decay.zscore = as.numeric(
      ifelse(
        datasets_yujiang$Sample == "ADNIAD" |
          datasets_yujiang$Sample == "ADNIControl",
        (
          S - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                             datasets_yujiang_means_sd$morphological_parameter == "S_age_decay"]
        ) /
          datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "ADNIControl" &
                                                                       datasets_yujiang_means_sd$morphological_parameter == "S_age_decay"],
        ifelse(
          datasets_yujiang$Sample == "HCP500r",
          (
            S - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "S_age_decay"]
          ) /
            datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "HCP500r" &
                                                                         datasets_yujiang_means_sd$morphological_parameter == "S_age_decay"],
          ifelse(
            datasets_yujiang$Sample == "NSI",
            (
              S - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NSI" &
                                                                                 datasets_yujiang_means_sd$morphological_parameter == "S_age_decay"]
            ) /
              datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "NSI" &
                                                                           datasets_yujiang_means_sd$morphological_parameter == "S_age_decay"],
            ifelse(
              ifelse(
                datasets_yujiang$Sample == "OASIS_healthy",
                (
                  S - datasets_yujiang_means_sd$mean.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                                     datasets_yujiang_means_sd$morphological_parameter == "S_age_decay"]
                ) /
                  datasets_yujiang_means_sd$sd.morphological_parameter_value[datasets_yujiang_means_sd$Sample == "OASIS_healthy" &
                                                                               datasets_yujiang_means_sd$morphological_parameter == "S_age_decay"],
                ""
              )
            )
          )
        )
      )
    )
  )
