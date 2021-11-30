## calculo Z score

dados_hemi_v1_CH_means_sd <- dados_hemi_v1_CH_DACTL %>%
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
  ) %>% filter(!is.na(morphological_parameter_value),!is.nan(morphological_parameter_value)) %>%
  group_by(Diagnostic, morphological_parameter, Age_interval10) %>%
  summarise(
    mean.morphological_parameter_value = mean(morphological_parameter_value),
    sd.morphological_parameter_value = sd(morphological_parameter_value)
  )


dados_hemi_v1_CH_DACTL <-
  dados_hemi_v1_CH_DACTL %>% dplyr::mutate(
    K.zscore = as.numeric((
      K - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                         dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                         dados_hemi_v1_CH_means_sd$morphological_parameter == "K"]
    ) /
      dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                   dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                   dados_hemi_v1_CH_means_sd$morphological_parameter == "K"]
    ),
    K_age_decay.zscore = as.numeric((
      K_age_decay - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                                   dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                                   dados_hemi_v1_CH_means_sd$morphological_parameter == "K_age_decay"]
    ) /
      dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                   dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                   dados_hemi_v1_CH_means_sd$morphological_parameter == "K_age_decay"]
    ),
    logAvgThickness.zscore =
      as.numeric((
        logAvgThickness - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                                         dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                                         dados_hemi_v1_CH_means_sd$morphological_parameter == "logAvgThickness"]
      ) /
        dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                     dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                     dados_hemi_v1_CH_means_sd$morphological_parameter == "logAvgThickness"]
      ),
    logAvgThickness_age_decay.zscore =
      as.numeric(
        (
          logAvgThickness_age_decay - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                                                     dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                                                     dados_hemi_v1_CH_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
        ) /
          dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                       dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                       dados_hemi_v1_CH_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
      ),
    I.zscore =
      as.numeric(
          (
            I - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                               dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                               dados_hemi_v1_CH_means_sd$morphological_parameter == "I"]
          ) /
            dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                         dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                         dados_hemi_v1_CH_means_sd$morphological_parameter == "I"]
        )
    ,
    I_age_decay.zscore =
      as.numeric(
        (
          I_age_decay - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                                       dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                                       dados_hemi_v1_CH_means_sd$morphological_parameter == "I_age_decay"]
        ) /
          dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                       dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                       dados_hemi_v1_CH_means_sd$morphological_parameter == "I_age_decay"]
      )
    ,
    S.zscore =
      as.numeric(
        (
          S - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                             dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                             dados_hemi_v1_CH_means_sd$morphological_parameter == "S"]
        ) /
          dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                       dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                       dados_hemi_v1_CH_means_sd$morphological_parameter == "S"]
      )
    ,
    S_age_decay.zscore =
      as.numeric(
        (
          S_age_decay - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                                       dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                                       dados_hemi_v1_CH_means_sd$morphological_parameter == "S_age_decay"]
        ) /
          dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Age_interval10 == "60" &
                                                                       dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
                                                                       dados_hemi_v1_CH_means_sd$morphological_parameter == "S_age_decay"]
      )
  )


#######

## calculo Z score

# dados_hemi_v1_CH_means_sd <- dados_hemi_v1_CH %>%
#   pivot_longer(
#     c(
#       K,
#       K_age_decay,
#       logAvgThickness,
#       logAvgThickness_age_decay,
#       S,
#       S_age_decay,
#       I,
#       I_age_decay
#     ),
#     names_to = "morphological_parameter",
#     values_to = "morphological_parameter_value"
#   ) %>% filter(!is.na(morphological_parameter_value),!is.nan(morphological_parameter_value)) %>%
#   group_by(Diagnostic, morphological_parameter) %>%
#   summarise(
#     mean.morphological_parameter_value = mean(morphological_parameter_value),
#     sd.morphological_parameter_value = sd(morphological_parameter_value)
#   )
# 
# 
# dados_hemi_v1_CH_DACTL <-
#   dados_hemi_v1_CH %>% mutate(
#     K.zscore = as.numeric((
#       K - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                          dados_hemi_v1_CH_means_sd$morphological_parameter == "K"]
#     ) /
#       dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                    dados_hemi_v1_CH_means_sd$morphological_parameter == "K"]
#     ),
#     K_age_decay.zscore = as.numeric((
#       K_age_decay - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                                    dados_hemi_v1_CH_means_sd$morphological_parameter == "K_age_decay"]
#     ) /
#       dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                    dados_hemi_v1_CH_means_sd$morphological_parameter == "K_age_decay"]
#     ),
#     logAvgThickness.zscore =
#       as.numeric((
#         logAvgThickness - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                                          dados_hemi_v1_CH_means_sd$morphological_parameter == "logAvgThickness"]
#       ) /
#         dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                      dados_hemi_v1_CH_means_sd$morphological_parameter == "logAvgThickness"]
#       ),
#     logAvgThickness_age_decay.zscore =
#       as.numeric(
#         (
#           logAvgThickness_age_decay - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                                                      dados_hemi_v1_CH_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
#         ) /
#           dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                        dados_hemi_v1_CH_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
#       ),
#     I.zscore =
#       as.numeric(
#         ifelse(
#           dados_hemi_v1_CH$Session == "1" &
#             dados_hemi_v1_CH$ROI == "hemisphere",
#           (
#             I - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                                dados_hemi_v1_CH_means_sd$morphological_parameter == "I"]
#           ) /
#             dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                          dados_hemi_v1_CH_means_sd$morphological_parameter == "I"],
#           ""
#         )
#       )
#     ,
#     I_age_decay.zscore =
#       as.numeric(
#         (
#           I_age_decay - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                                        dados_hemi_v1_CH_means_sd$morphological_parameter == "I_age_decay"]
#         ) /
#           dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                        dados_hemi_v1_CH_means_sd$morphological_parameter == "I_age_decay"]
#       )
#     ,
#     S.zscore =
#       as.numeric(
#         (
#           S - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                              dados_hemi_v1_CH_means_sd$morphological_parameter == "S"]
#         ) /
#           dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                        dados_hemi_v1_CH_means_sd$morphological_parameter == "S"]
#       )
#     ,
#     S_age_decay.zscore =
#       as.numeric(
#         (
#           S_age_decay - dados_hemi_v1_CH_means_sd$mean.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                                        dados_hemi_v1_CH_means_sd$morphological_parameter == "S_age_decay"]
#         ) /
#           dados_hemi_v1_CH_means_sd$sd.morphological_parameter_value[dados_hemi_v1_CH_means_sd$Diagnostic == "CTL" &
#                                                                        dados_hemi_v1_CH_means_sd$morphological_parameter == "S_age_decay"]
#       )
#   )