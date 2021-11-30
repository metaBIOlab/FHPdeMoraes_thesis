## calculo Z score

dados_means_sd <- dados %>%
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
  group_by(Session, ROI, Diagnostic, morphological_parameter) %>%
  summarise(
    mean.morphological_parameter_value = mean(morphological_parameter_value),
    sd.morphological_parameter_value = sd(morphological_parameter_value)
  )


dados <-
  dados %>% mutate(
    K.zscore = as.numeric(ifelse(
      dados$Session == "1" & dados$ROI == "hemisphere",
      (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                               dados_means_sd$ROI == "hemisphere" &
                                                               dados_means_sd$Diagnostic == "CONTROLE" &
                                                               dados_means_sd$morphological_parameter == "K"]) /
        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                          dados_means_sd$ROI == "hemisphere" &
                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                          dados_means_sd$morphological_parameter == "K"],
      ifelse(
        dados$Session == "2" & dados$ROI == "hemisphere",
        (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                 dados_means_sd$ROI == "hemisphere" &
                                                                 dados_means_sd$Diagnostic == "CONTROLE" &
                                                                 dados_means_sd$morphological_parameter == "K"]) /
          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                            dados_means_sd$ROI == "hemisphere" &
                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                            dados_means_sd$morphological_parameter == "K"],
        ifelse(
          dados$Session == "3" & dados$ROI == "hemisphere",
          (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                   dados_means_sd$ROI == "hemisphere" &
                                                                   dados_means_sd$Diagnostic == "CONTROLE" &
                                                                   dados_means_sd$morphological_parameter == "K"]) /
            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                              dados_means_sd$ROI == "hemisphere" &
                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                              dados_means_sd$morphological_parameter == "K"],
          ifelse(
            dados$Session == "1" & dados$ROI == "F",
            (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                     dados_means_sd$ROI == "F" &
                                                                     dados_means_sd$Diagnostic == "CONTROLE" &
                                                                     dados_means_sd$morphological_parameter == "K"]) /
              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                dados_means_sd$ROI == "F" &
                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                dados_means_sd$morphological_parameter == "K"],
            ifelse(
              dados$Session == "2" & dados$ROI == "F",
              (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                       dados_means_sd$ROI == "F" &
                                                                       dados_means_sd$Diagnostic == "CONTROLE" &
                                                                       dados_means_sd$morphological_parameter == "K"]) /
                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                  dados_means_sd$ROI == "F" &
                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                  dados_means_sd$morphological_parameter == "K"],
              ifelse(
                dados$Session == "3" & dados$ROI == "F",
                (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                         dados_means_sd$ROI == "F" &
                                                                         dados_means_sd$Diagnostic == "CONTROLE" &
                                                                         dados_means_sd$morphological_parameter == "K"]) /
                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                    dados_means_sd$ROI == "F" &
                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                    dados_means_sd$morphological_parameter == "K"],
                ifelse(
                  dados$Session == "1" & dados$ROI == "P",
                  (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                           dados_means_sd$ROI == "P" &
                                                                           dados_means_sd$Diagnostic == "CONTROLE" &
                                                                           dados_means_sd$morphological_parameter == "K"]) /
                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                      dados_means_sd$ROI == "P" &
                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                      dados_means_sd$morphological_parameter == "K"],
                  ifelse(
                    dados$Session == "2" & dados$ROI == "P",
                    (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                             dados_means_sd$ROI == "P" &
                                                                             dados_means_sd$Diagnostic == "CONTROLE" &
                                                                             dados_means_sd$morphological_parameter == "K"]) /
                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                        dados_means_sd$ROI == "P" &
                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                        dados_means_sd$morphological_parameter == "K"],
                    ifelse(
                      dados$Session == "3" & dados$ROI == "P",
                      (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                               dados_means_sd$ROI == "P" &
                                                                               dados_means_sd$Diagnostic == "CONTROLE" &
                                                                               dados_means_sd$morphological_parameter == "K"]) /
                        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                          dados_means_sd$ROI == "P" &
                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                          dados_means_sd$morphological_parameter == "K"],
                      ifelse(
                        dados$Session == "1" & dados$ROI == "T",
                        (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                 dados_means_sd$ROI == "T" &
                                                                                 dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                 dados_means_sd$morphological_parameter == "K"]) /
                          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                            dados_means_sd$ROI == "T" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "K"],
                        ifelse(
                          dados$Session == "2" & dados$ROI == "T",
                          (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                   dados_means_sd$ROI == "T" &
                                                                                   dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                   dados_means_sd$morphological_parameter == "K"]) /
                            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                              dados_means_sd$ROI == "T" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "K"],
                          ifelse(
                            dados$Session == "3" & dados$ROI == "T",
                            (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                     dados_means_sd$ROI == "T" &
                                                                                     dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                     dados_means_sd$morphological_parameter == "K"]) /
                              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                dados_means_sd$ROI == "T" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "K"],
                            ifelse(
                              dados$Session == "1" & dados$ROI == "O",
                              (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                       dados_means_sd$ROI == "O" &
                                                                                       dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                       dados_means_sd$morphological_parameter == "K"]) /
                                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                  dados_means_sd$ROI == "O" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "K"],
                              ifelse(
                                dados$Session == "2" & dados$ROI == "O",
                                (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                         dados_means_sd$ROI == "O" &
                                                                                         dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                         dados_means_sd$morphological_parameter == "K"]) /
                                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                    dados_means_sd$ROI == "O" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "K"],
                                ifelse(
                                  dados$Session == "3" & dados$ROI == "O",
                                  (K - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                           dados_means_sd$ROI == "O" &
                                                                                           dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                           dados_means_sd$morphological_parameter == "K"]) /
                                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                      dados_means_sd$ROI == "O" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "K"],
                                  ""
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )),
    K_age_decay.zscore = as.numeric(
      ifelse(
        dados$Session == "1" & dados$ROI == "hemisphere",
        (
          K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                            dados_means_sd$ROI == "hemisphere" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "K_age_decay"]
        ) /
          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                            dados_means_sd$ROI == "hemisphere" &
                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                            dados_means_sd$morphological_parameter == "K_age_decay"],
        ifelse(
          dados$Session == "2" & dados$ROI == "hemisphere",
          (
            K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                              dados_means_sd$ROI == "hemisphere" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "K_age_decay"]
          ) /
            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                              dados_means_sd$ROI == "hemisphere" &
                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                              dados_means_sd$morphological_parameter == "K_age_decay"],
          ifelse(
            dados$Session == "3" & dados$ROI == "hemisphere",
            (
              K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                dados_means_sd$ROI == "hemisphere" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "K_age_decay"]
            ) /
              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                dados_means_sd$ROI == "hemisphere" &
                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                dados_means_sd$morphological_parameter == "K_age_decay"],
            ifelse(
              dados$Session == "1" & dados$ROI == "F",
              (
                K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                  dados_means_sd$ROI == "F" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "K_age_decay"]
              ) /
                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                  dados_means_sd$ROI == "F" &
                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                  dados_means_sd$morphological_parameter == "K_age_decay"],
              ifelse(
                dados$Session == "2" & dados$ROI == "F",
                (
                  K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                    dados_means_sd$ROI == "F" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "K_age_decay"]
                ) /
                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                    dados_means_sd$ROI == "F" &
                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                    dados_means_sd$morphological_parameter == "K_age_decay"],
                ifelse(
                  dados$Session == "3" & dados$ROI == "F",
                  (
                    K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                      dados_means_sd$ROI == "F" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "K_age_decay"]
                  ) /
                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                      dados_means_sd$ROI == "F" &
                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                      dados_means_sd$morphological_parameter == "K_age_decay"],
                  ifelse(
                    dados$Session == "1" & dados$ROI == "P",
                    (
                      K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                        dados_means_sd$ROI == "P" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "K_age_decay"]
                    ) /
                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                        dados_means_sd$ROI == "P" &
                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                        dados_means_sd$morphological_parameter == "K_age_decay"],
                    ifelse(
                      dados$Session == "2" & dados$ROI == "P",
                      (
                        K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                          dados_means_sd$ROI == "P" &
                                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                          dados_means_sd$morphological_parameter == "K_age_decay"]
                      ) /
                        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                          dados_means_sd$ROI == "P" &
                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                          dados_means_sd$morphological_parameter == "K_age_decay"],
                      ifelse(
                        dados$Session == "3" & dados$ROI == "P",
                        (
                          K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                            dados_means_sd$ROI == "P" &
                                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                            dados_means_sd$morphological_parameter == "K_age_decay"]
                        ) /
                          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                            dados_means_sd$ROI == "P" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "K_age_decay"],
                        ifelse(
                          dados$Session == "1" & dados$ROI == "T",
                          (
                            K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                              dados_means_sd$ROI == "T" &
                                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                              dados_means_sd$morphological_parameter == "K_age_decay"]
                          ) /
                            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                              dados_means_sd$ROI == "T" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "K_age_decay"],
                          ifelse(
                            dados$Session == "2" & dados$ROI == "T",
                            (
                              K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                dados_means_sd$ROI == "T" &
                                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                dados_means_sd$morphological_parameter == "K_age_decay"]
                            ) /
                              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                dados_means_sd$ROI == "T" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "K_age_decay"],
                            ifelse(
                              dados$Session == "3" & dados$ROI == "T",
                              (
                                K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                  dados_means_sd$ROI == "T" &
                                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                  dados_means_sd$morphological_parameter == "K_age_decay"]
                              ) /
                                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                  dados_means_sd$ROI == "T" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "K_age_decay"],
                              ifelse(
                                dados$Session == "1" & dados$ROI == "O",
                                (
                                  K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                    dados_means_sd$ROI == "O" &
                                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                    dados_means_sd$morphological_parameter == "K_age_decay"]
                                ) /
                                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                    dados_means_sd$ROI == "O" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "K_age_decay"],
                                ifelse(
                                  dados$Session == "2" & dados$ROI == "O",
                                  (
                                    K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                      dados_means_sd$ROI == "O" &
                                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                      dados_means_sd$morphological_parameter == "K_age_decay"]
                                  ) /
                                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                      dados_means_sd$ROI == "O" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "K_age_decay"],
                                  ifelse(
                                    dados$Session == "3" & dados$ROI == "O",
                                    (
                                      K_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                        dados_means_sd$ROI == "O" &
                                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                        dados_means_sd$morphological_parameter == "K_age_decay"]
                                    ) /
                                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                        dados_means_sd$ROI == "O" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "K_age_decay"],
                                    ""
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )),
    logAvgThickness.zscore =
      as.numeric(ifelse(
        dados$Session == "1" & dados$ROI == "hemisphere",
        (
          logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                dados_means_sd$ROI == "hemisphere" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "logAvgThickness"]
        ) /
          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                            dados_means_sd$ROI == "hemisphere" &
                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                            dados_means_sd$morphological_parameter == "logAvgThickness"],
        ifelse(
          dados$Session == "2" & dados$ROI == "hemisphere",
          (
            logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                  dados_means_sd$ROI == "hemisphere" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "logAvgThickness"]
          ) /
            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                              dados_means_sd$ROI == "hemisphere" &
                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                              dados_means_sd$morphological_parameter == "logAvgThickness"],
          ifelse(
            dados$Session == "3" & dados$ROI == "hemisphere",
            (
              logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                    dados_means_sd$ROI == "hemisphere" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "logAvgThickness"]
            ) /
              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                dados_means_sd$ROI == "hemisphere" &
                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                dados_means_sd$morphological_parameter == "logAvgThickness"],
            ifelse(
              dados$Session == "1" & dados$ROI == "F",
              (
                logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                      dados_means_sd$ROI == "F" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "logAvgThickness"]
              ) /
                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                  dados_means_sd$ROI == "F" &
                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                  dados_means_sd$morphological_parameter == "logAvgThickness"],
              ifelse(
                dados$Session == "2" & dados$ROI == "F",
                (
                  logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                        dados_means_sd$ROI == "F" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "logAvgThickness"]
                ) /
                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                    dados_means_sd$ROI == "F" &
                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                    dados_means_sd$morphological_parameter == "logAvgThickness"],
                ifelse(
                  dados$Session == "3" & dados$ROI == "F",
                  (
                    logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                          dados_means_sd$ROI == "F" &
                                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                          dados_means_sd$morphological_parameter == "logAvgThickness"]
                  ) /
                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                      dados_means_sd$ROI == "F" &
                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                      dados_means_sd$morphological_parameter == "logAvgThickness"],
                  ifelse(
                    dados$Session == "1" & dados$ROI == "P",
                    (
                      logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                            dados_means_sd$ROI == "P" &
                                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                            dados_means_sd$morphological_parameter == "logAvgThickness"]
                    ) /
                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                        dados_means_sd$ROI == "P" &
                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                        dados_means_sd$morphological_parameter == "logAvgThickness"],
                    ifelse(
                      dados$Session == "2" & dados$ROI == "P",
                      (
                        logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                              dados_means_sd$ROI == "P" &
                                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                              dados_means_sd$morphological_parameter == "logAvgThickness"]
                      ) /
                        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                          dados_means_sd$ROI == "P" &
                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                          dados_means_sd$morphological_parameter == "logAvgThickness"],
                      ifelse(
                        dados$Session == "3" & dados$ROI == "P",
                        (
                          logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                dados_means_sd$ROI == "P" &
                                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                dados_means_sd$morphological_parameter == "logAvgThickness"]
                        ) /
                          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                            dados_means_sd$ROI == "P" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "logAvgThickness"],
                        ifelse(
                          dados$Session == "1" & dados$ROI == "T",
                          (
                            logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                  dados_means_sd$ROI == "T" &
                                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                  dados_means_sd$morphological_parameter == "logAvgThickness"]
                          ) /
                            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                              dados_means_sd$ROI == "T" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "logAvgThickness"],
                          ifelse(
                            dados$Session == "2" & dados$ROI == "T",
                            (
                              logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                    dados_means_sd$ROI == "T" &
                                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                    dados_means_sd$morphological_parameter == "logAvgThickness"]
                            ) /
                              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                dados_means_sd$ROI == "T" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "logAvgThickness"],
                            ifelse(
                              dados$Session == "3" & dados$ROI == "T",
                              (
                                logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                      dados_means_sd$ROI == "T" &
                                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                      dados_means_sd$morphological_parameter == "logAvgThickness"]
                              ) /
                                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                  dados_means_sd$ROI == "T" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "logAvgThickness"],
                              ifelse(
                                dados$Session == "1" & dados$ROI == "O",
                                (
                                  logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                        dados_means_sd$ROI == "O" &
                                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                        dados_means_sd$morphological_parameter == "logAvgThickness"]
                                ) /
                                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                    dados_means_sd$ROI == "O" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "logAvgThickness"],
                                ifelse(
                                  dados$Session == "2" & dados$ROI == "O",
                                  (
                                    logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                          dados_means_sd$ROI == "O" &
                                                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                          dados_means_sd$morphological_parameter == "logAvgThickness"]
                                  ) /
                                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                      dados_means_sd$ROI == "O" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "logAvgThickness"],
                                  ifelse(
                                    dados$Session == "3" & dados$ROI == "O",
                                    (
                                      logAvgThickness - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                            dados_means_sd$ROI == "O" &
                                                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                            dados_means_sd$morphological_parameter == "logAvgThickness"]
                                    ) /
                                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                        dados_means_sd$ROI == "O" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "logAvgThickness"],
                                    ""
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )),
    logAvgThickness_age_decay.zscore =
      as.numeric(ifelse(
        dados$Session == "1" & dados$ROI == "hemisphere",
        (
          logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                          dados_means_sd$ROI == "hemisphere" &
                                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                          dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
        ) /
          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                            dados_means_sd$ROI == "hemisphere" &
                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                            dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
        ifelse(
          dados$Session == "2" & dados$ROI == "hemisphere",
          (
            logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                            dados_means_sd$ROI == "hemisphere" &
                                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                            dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
          ) /
            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                              dados_means_sd$ROI == "hemisphere" &
                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                              dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
          ifelse(
            dados$Session == "3" & dados$ROI == "hemisphere",
            (
              logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                              dados_means_sd$ROI == "hemisphere" &
                                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                              dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
            ) /
              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                dados_means_sd$ROI == "hemisphere" &
                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
            ifelse(
              dados$Session == "1" & dados$ROI == "F",
              (
                logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                dados_means_sd$ROI == "F" &
                                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
              ) /
                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                  dados_means_sd$ROI == "F" &
                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                  dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
              ifelse(
                dados$Session == "2" & dados$ROI == "F",
                (
                  logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                  dados_means_sd$ROI == "F" &
                                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                  dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                ) /
                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                    dados_means_sd$ROI == "F" &
                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                    dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                ifelse(
                  dados$Session == "3" & dados$ROI == "F",
                  (
                    logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                    dados_means_sd$ROI == "F" &
                                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                    dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                  ) /
                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                      dados_means_sd$ROI == "F" &
                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                      dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                  ifelse(
                    dados$Session == "1" & dados$ROI == "P",
                    (
                      logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                      dados_means_sd$ROI == "P" &
                                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                      dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                    ) /
                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                        dados_means_sd$ROI == "P" &
                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                        dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                    ifelse(
                      dados$Session == "2" & dados$ROI == "P",
                      (
                        logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                        dados_means_sd$ROI == "P" &
                                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                        dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                      ) /
                        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                          dados_means_sd$ROI == "P" &
                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                          dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                      ifelse(
                        dados$Session == "3" & dados$ROI == "P",
                        (
                          logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                          dados_means_sd$ROI == "P" &
                                                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                          dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                        ) /
                          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                            dados_means_sd$ROI == "P" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                        ifelse(
                          dados$Session == "1" & dados$ROI == "T",
                          (
                            logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                            dados_means_sd$ROI == "T" &
                                                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                            dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                          ) /
                            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                              dados_means_sd$ROI == "T" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                          ifelse(
                            dados$Session == "2" & dados$ROI == "T",
                            (
                              logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                              dados_means_sd$ROI == "T" &
                                                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                              dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                            ) /
                              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                dados_means_sd$ROI == "T" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                            ifelse(
                              dados$Session == "3" & dados$ROI == "T",
                              (
                                logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                                dados_means_sd$ROI == "T" &
                                                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                                dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                              ) /
                                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                  dados_means_sd$ROI == "T" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                              ifelse(
                                dados$Session == "1" & dados$ROI == "O",
                                (
                                  logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                                  dados_means_sd$ROI == "O" &
                                                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                                  dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                                ) /
                                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                    dados_means_sd$ROI == "O" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                                ifelse(
                                  dados$Session == "2" & dados$ROI == "O",
                                  (
                                    logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                                    dados_means_sd$ROI == "O" &
                                                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                                    dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                                  ) /
                                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                      dados_means_sd$ROI == "O" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                                  ifelse(
                                    dados$Session == "3" & dados$ROI == "O",
                                    (
                                      logAvgThickness_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                                      dados_means_sd$ROI == "O" &
                                                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                                      dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"]
                                    ) /
                                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                        dados_means_sd$ROI == "O" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "logAvgThickness_age_decay"],
                                    ""
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )),
    I.zscore =
      as.numeric(ifelse(
        dados$Session == "1" & dados$ROI == "hemisphere",
        (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                 dados_means_sd$ROI == "hemisphere" &
                                                                 dados_means_sd$Diagnostic == "CONTROLE" &
                                                                 dados_means_sd$morphological_parameter == "I"]) /
          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                            dados_means_sd$ROI == "hemisphere" &
                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                            dados_means_sd$morphological_parameter == "I"],
        ifelse(
          dados$Session == "2" & dados$ROI == "hemisphere",
          (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                   dados_means_sd$ROI == "hemisphere" &
                                                                   dados_means_sd$Diagnostic == "CONTROLE" &
                                                                   dados_means_sd$morphological_parameter == "I"]) /
            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                              dados_means_sd$ROI == "hemisphere" &
                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                              dados_means_sd$morphological_parameter == "I"],
          ifelse(
            dados$Session == "3" & dados$ROI == "hemisphere",
            (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                     dados_means_sd$ROI == "hemisphere" &
                                                                     dados_means_sd$Diagnostic == "CONTROLE" &
                                                                     dados_means_sd$morphological_parameter == "I"]) /
              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                dados_means_sd$ROI == "hemisphere" &
                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                dados_means_sd$morphological_parameter == "I"],
            ifelse(
              dados$Session == "1" & dados$ROI == "F",
              (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                       dados_means_sd$ROI == "F" &
                                                                       dados_means_sd$Diagnostic == "CONTROLE" &
                                                                       dados_means_sd$morphological_parameter == "I"]) /
                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                  dados_means_sd$ROI == "F" &
                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                  dados_means_sd$morphological_parameter == "I"],
              ifelse(
                dados$Session == "2" & dados$ROI == "F",
                (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                         dados_means_sd$ROI == "F" &
                                                                         dados_means_sd$Diagnostic == "CONTROLE" &
                                                                         dados_means_sd$morphological_parameter == "I"]) /
                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                    dados_means_sd$ROI == "F" &
                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                    dados_means_sd$morphological_parameter == "I"],
                ifelse(
                  dados$Session == "3" & dados$ROI == "F",
                  (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                           dados_means_sd$ROI == "F" &
                                                                           dados_means_sd$Diagnostic == "CONTROLE" &
                                                                           dados_means_sd$morphological_parameter == "I"]) /
                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                      dados_means_sd$ROI == "F" &
                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                      dados_means_sd$morphological_parameter == "I"],
                  ifelse(
                    dados$Session == "1" & dados$ROI == "P",
                    (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                             dados_means_sd$ROI == "P" &
                                                                             dados_means_sd$Diagnostic == "CONTROLE" &
                                                                             dados_means_sd$morphological_parameter == "I"]) /
                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                        dados_means_sd$ROI == "P" &
                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                        dados_means_sd$morphological_parameter == "I"],
                    ifelse(
                      dados$Session == "2" & dados$ROI == "P",
                      (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                               dados_means_sd$ROI == "P" &
                                                                               dados_means_sd$Diagnostic == "CONTROLE" &
                                                                               dados_means_sd$morphological_parameter == "I"]) /
                        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                          dados_means_sd$ROI == "P" &
                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                          dados_means_sd$morphological_parameter == "I"],
                      ifelse(
                        dados$Session == "3" & dados$ROI == "P",
                        (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                 dados_means_sd$ROI == "P" &
                                                                                 dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                 dados_means_sd$morphological_parameter == "I"]) /
                          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                            dados_means_sd$ROI == "P" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "I"],
                        ifelse(
                          dados$Session == "1" & dados$ROI == "T",
                          (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                   dados_means_sd$ROI == "T" &
                                                                                   dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                   dados_means_sd$morphological_parameter == "I"]) /
                            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                              dados_means_sd$ROI == "T" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "I"],
                          ifelse(
                            dados$Session == "2" & dados$ROI == "T",
                            (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                     dados_means_sd$ROI == "T" &
                                                                                     dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                     dados_means_sd$morphological_parameter == "I"]) /
                              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                dados_means_sd$ROI == "T" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "I"],
                            ifelse(
                              dados$Session == "3" & dados$ROI == "T",
                              (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                       dados_means_sd$ROI == "T" &
                                                                                       dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                       dados_means_sd$morphological_parameter == "I"]) /
                                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                  dados_means_sd$ROI == "T" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "I"],
                              ifelse(
                                dados$Session == "1" & dados$ROI == "O",
                                (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                         dados_means_sd$ROI == "O" &
                                                                                         dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                         dados_means_sd$morphological_parameter == "I"]) /
                                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                    dados_means_sd$ROI == "O" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "I"],
                                ifelse(
                                  dados$Session == "2" & dados$ROI == "O",
                                  (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                           dados_means_sd$ROI == "O" &
                                                                                           dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                           dados_means_sd$morphological_parameter == "I"]) /
                                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                      dados_means_sd$ROI == "O" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "I"],
                                  ifelse(
                                    dados$Session == "3" & dados$ROI == "O",
                                    (I - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                             dados_means_sd$ROI == "O" &
                                                                                             dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                             dados_means_sd$morphological_parameter == "I"]) /
                                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                        dados_means_sd$ROI == "O" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "I"],
                                    ""
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )),
    I_age_decay.zscore =
      as.numeric(ifelse(
        dados$Session == "1" & dados$ROI == "hemisphere",
        (
          I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                            dados_means_sd$ROI == "hemisphere" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "I_age_decay"]
        ) /
          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                            dados_means_sd$ROI == "hemisphere" &
                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                            dados_means_sd$morphological_parameter == "I_age_decay"],
        ifelse(
          dados$Session == "2" & dados$ROI == "hemisphere",
          (
            I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                              dados_means_sd$ROI == "hemisphere" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "I_age_decay"]
          ) /
            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                              dados_means_sd$ROI == "hemisphere" &
                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                              dados_means_sd$morphological_parameter == "I_age_decay"],
          ifelse(
            dados$Session == "3" & dados$ROI == "hemisphere",
            (
              I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                dados_means_sd$ROI == "hemisphere" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "I_age_decay"]
            ) /
              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                dados_means_sd$ROI == "hemisphere" &
                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                dados_means_sd$morphological_parameter == "I_age_decay"],
            ifelse(
              dados$Session == "1" & dados$ROI == "F",
              (
                I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                  dados_means_sd$ROI == "F" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "I_age_decay"]
              ) /
                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                  dados_means_sd$ROI == "F" &
                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                  dados_means_sd$morphological_parameter == "I_age_decay"],
              ifelse(
                dados$Session == "2" & dados$ROI == "F",
                (
                  I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                    dados_means_sd$ROI == "F" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "I_age_decay"]
                ) /
                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                    dados_means_sd$ROI == "F" &
                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                    dados_means_sd$morphological_parameter == "I_age_decay"],
                ifelse(
                  dados$Session == "3" & dados$ROI == "F",
                  (
                    I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                      dados_means_sd$ROI == "F" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "I_age_decay"]
                  ) /
                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                      dados_means_sd$ROI == "F" &
                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                      dados_means_sd$morphological_parameter == "I_age_decay"],
                  ifelse(
                    dados$Session == "1" & dados$ROI == "P",
                    (
                      I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                        dados_means_sd$ROI == "P" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "I_age_decay"]
                    ) /
                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                        dados_means_sd$ROI == "P" &
                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                        dados_means_sd$morphological_parameter == "I_age_decay"],
                    ifelse(
                      dados$Session == "2" & dados$ROI == "P",
                      (
                        I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                          dados_means_sd$ROI == "P" &
                                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                          dados_means_sd$morphological_parameter == "I_age_decay"]
                      ) /
                        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                          dados_means_sd$ROI == "P" &
                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                          dados_means_sd$morphological_parameter == "I_age_decay"],
                      ifelse(
                        dados$Session == "3" & dados$ROI == "P",
                        (
                          I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                            dados_means_sd$ROI == "P" &
                                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                            dados_means_sd$morphological_parameter == "I_age_decay"]
                        ) /
                          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                            dados_means_sd$ROI == "P" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "I_age_decay"],
                        ifelse(
                          dados$Session == "1" & dados$ROI == "T",
                          (
                            I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                              dados_means_sd$ROI == "T" &
                                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                              dados_means_sd$morphological_parameter == "I_age_decay"]
                          ) /
                            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                              dados_means_sd$ROI == "T" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "I_age_decay"],
                          ifelse(
                            dados$Session == "2" & dados$ROI == "T",
                            (
                              I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                dados_means_sd$ROI == "T" &
                                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                dados_means_sd$morphological_parameter == "I_age_decay"]
                            ) /
                              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                dados_means_sd$ROI == "T" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "I_age_decay"],
                            ifelse(
                              dados$Session == "3" & dados$ROI == "T",
                              (
                                I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                  dados_means_sd$ROI == "T" &
                                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                  dados_means_sd$morphological_parameter == "I_age_decay"]
                              ) /
                                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                  dados_means_sd$ROI == "T" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "I_age_decay"],
                              ifelse(
                                dados$Session == "1" & dados$ROI == "O",
                                (
                                  I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                    dados_means_sd$ROI == "O" &
                                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                    dados_means_sd$morphological_parameter == "I_age_decay"]
                                ) /
                                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                    dados_means_sd$ROI == "O" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "I_age_decay"],
                                ifelse(
                                  dados$Session == "2" & dados$ROI == "O",
                                  (
                                    I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                      dados_means_sd$ROI == "O" &
                                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                      dados_means_sd$morphological_parameter == "I_age_decay"]
                                  ) /
                                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                      dados_means_sd$ROI == "O" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "I_age_decay"],
                                  ifelse(
                                    dados$Session == "3" & dados$ROI == "O",
                                    (
                                      I_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                        dados_means_sd$ROI == "O" &
                                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                        dados_means_sd$morphological_parameter == "I_age_decay"]
                                    ) /
                                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                        dados_means_sd$ROI == "O" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "I_age_decay"],
                                    ""
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )),
    S.zscore =
      as.numeric(ifelse(
        dados$Session == "1" & dados$ROI == "hemisphere",
        (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                 dados_means_sd$ROI == "hemisphere" &
                                                                 dados_means_sd$Diagnostic == "CONTROLE" &
                                                                 dados_means_sd$morphological_parameter == "S"]) /
          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                            dados_means_sd$ROI == "hemisphere" &
                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                            dados_means_sd$morphological_parameter == "S"],
        ifelse(
          dados$Session == "2" & dados$ROI == "hemisphere",
          (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                   dados_means_sd$ROI == "hemisphere" &
                                                                   dados_means_sd$Diagnostic == "CONTROLE" &
                                                                   dados_means_sd$morphological_parameter == "S"]) /
            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                              dados_means_sd$ROI == "hemisphere" &
                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                              dados_means_sd$morphological_parameter == "S"],
          ifelse(
            dados$Session == "3" & dados$ROI == "hemisphere",
            (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                     dados_means_sd$ROI == "hemisphere" &
                                                                     dados_means_sd$Diagnostic == "CONTROLE" &
                                                                     dados_means_sd$morphological_parameter == "S"]) /
              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                dados_means_sd$ROI == "hemisphere" &
                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                dados_means_sd$morphological_parameter == "S"],
            ifelse(
              dados$Session == "1" & dados$ROI == "F",
              (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                       dados_means_sd$ROI == "F" &
                                                                       dados_means_sd$Diagnostic == "CONTROLE" &
                                                                       dados_means_sd$morphological_parameter == "S"]) /
                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                  dados_means_sd$ROI == "F" &
                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                  dados_means_sd$morphological_parameter == "S"],
              ifelse(
                dados$Session == "2" & dados$ROI == "F",
                (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                         dados_means_sd$ROI == "F" &
                                                                         dados_means_sd$Diagnostic == "CONTROLE" &
                                                                         dados_means_sd$morphological_parameter == "S"]) /
                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                    dados_means_sd$ROI == "F" &
                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                    dados_means_sd$morphological_parameter == "S"],
                ifelse(
                  dados$Session == "3" & dados$ROI == "F",
                  (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                           dados_means_sd$ROI == "F" &
                                                                           dados_means_sd$Diagnostic == "CONTROLE" &
                                                                           dados_means_sd$morphological_parameter == "S"]) /
                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                      dados_means_sd$ROI == "F" &
                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                      dados_means_sd$morphological_parameter == "S"],
                  ifelse(
                    dados$Session == "1" & dados$ROI == "P",
                    (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                             dados_means_sd$ROI == "P" &
                                                                             dados_means_sd$Diagnostic == "CONTROLE" &
                                                                             dados_means_sd$morphological_parameter == "S"]) /
                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                        dados_means_sd$ROI == "P" &
                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                        dados_means_sd$morphological_parameter == "S"],
                    ifelse(
                      dados$Session == "2" & dados$ROI == "P",
                      (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                               dados_means_sd$ROI == "P" &
                                                                               dados_means_sd$Diagnostic == "CONTROLE" &
                                                                               dados_means_sd$morphological_parameter == "S"]) /
                        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                          dados_means_sd$ROI == "P" &
                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                          dados_means_sd$morphological_parameter == "S"],
                      ifelse(
                        dados$Session == "3" & dados$ROI == "P",
                        (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                 dados_means_sd$ROI == "P" &
                                                                                 dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                 dados_means_sd$morphological_parameter == "S"]) /
                          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                            dados_means_sd$ROI == "P" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "S"],
                        ifelse(
                          dados$Session == "1" & dados$ROI == "T",
                          (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                   dados_means_sd$ROI == "T" &
                                                                                   dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                   dados_means_sd$morphological_parameter == "S"]) /
                            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                              dados_means_sd$ROI == "T" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "S"],
                          ifelse(
                            dados$Session == "2" & dados$ROI == "T",
                            (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                     dados_means_sd$ROI == "T" &
                                                                                     dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                     dados_means_sd$morphological_parameter == "S"]) /
                              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                dados_means_sd$ROI == "T" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "S"],
                            ifelse(
                              dados$Session == "3" & dados$ROI == "T",
                              (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                       dados_means_sd$ROI == "T" &
                                                                                       dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                       dados_means_sd$morphological_parameter == "S"]) /
                                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                  dados_means_sd$ROI == "T" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "S"],
                              ifelse(
                                dados$Session == "1" & dados$ROI == "O",
                                (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                         dados_means_sd$ROI == "O" &
                                                                                         dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                         dados_means_sd$morphological_parameter == "S"]) /
                                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                    dados_means_sd$ROI == "O" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "S"],
                                ifelse(
                                  dados$Session == "2" & dados$ROI == "O",
                                  (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                           dados_means_sd$ROI == "O" &
                                                                                           dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                           dados_means_sd$morphological_parameter == "S"]) /
                                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                      dados_means_sd$ROI == "O" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "S"],
                                  ifelse(
                                    dados$Session == "3" & dados$ROI == "O",
                                    (S - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                             dados_means_sd$ROI == "O" &
                                                                                             dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                             dados_means_sd$morphological_parameter == "S"]) /
                                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                        dados_means_sd$ROI == "O" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "S"],
                                    ""
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )),
    S_age_decay.zscore =
      as.numeric(ifelse(
        dados$Session == "1" & dados$ROI == "hemisphere",
        (
          S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                            dados_means_sd$ROI == "hemisphere" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "S_age_decay"]
        ) /
          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                            dados_means_sd$ROI == "hemisphere" &
                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                            dados_means_sd$morphological_parameter == "S_age_decay"],
        ifelse(
          dados$Session == "2" & dados$ROI == "hemisphere",
          (
            S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                              dados_means_sd$ROI == "hemisphere" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "S_age_decay"]
          ) /
            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                              dados_means_sd$ROI == "hemisphere" &
                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                              dados_means_sd$morphological_parameter == "S_age_decay"],
          ifelse(
            dados$Session == "3" & dados$ROI == "hemisphere",
            (
              S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                dados_means_sd$ROI == "hemisphere" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "S_age_decay"]
            ) /
              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                dados_means_sd$ROI == "hemisphere" &
                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                dados_means_sd$morphological_parameter == "S_age_decay"],
            ifelse(
              dados$Session == "1" & dados$ROI == "F",
              (
                S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                  dados_means_sd$ROI == "F" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "S_age_decay"]
              ) /
                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                  dados_means_sd$ROI == "F" &
                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                  dados_means_sd$morphological_parameter == "S_age_decay"],
              ifelse(
                dados$Session == "2" & dados$ROI == "F",
                (
                  S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                    dados_means_sd$ROI == "F" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "S_age_decay"]
                ) /
                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                    dados_means_sd$ROI == "F" &
                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                    dados_means_sd$morphological_parameter == "S_age_decay"],
                ifelse(
                  dados$Session == "3" & dados$ROI == "F",
                  (
                    S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                      dados_means_sd$ROI == "F" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "S_age_decay"]
                  ) /
                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                      dados_means_sd$ROI == "F" &
                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                      dados_means_sd$morphological_parameter == "S_age_decay"],
                  ifelse(
                    dados$Session == "1" & dados$ROI == "P",
                    (
                      S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                        dados_means_sd$ROI == "P" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "S_age_decay"]
                    ) /
                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                        dados_means_sd$ROI == "P" &
                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                        dados_means_sd$morphological_parameter == "S_age_decay"],
                    ifelse(
                      dados$Session == "2" & dados$ROI == "P",
                      (
                        S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                          dados_means_sd$ROI == "P" &
                                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                          dados_means_sd$morphological_parameter == "S_age_decay"]
                      ) /
                        dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                          dados_means_sd$ROI == "P" &
                                                                          dados_means_sd$Diagnostic == "CONTROLE" &
                                                                          dados_means_sd$morphological_parameter == "S_age_decay"],
                      ifelse(
                        dados$Session == "3" & dados$ROI == "P",
                        (
                          S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                            dados_means_sd$ROI == "P" &
                                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                            dados_means_sd$morphological_parameter == "S_age_decay"]
                        ) /
                          dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                            dados_means_sd$ROI == "P" &
                                                                            dados_means_sd$Diagnostic == "CONTROLE" &
                                                                            dados_means_sd$morphological_parameter == "S_age_decay"],
                        ifelse(
                          dados$Session == "1" & dados$ROI == "T",
                          (
                            S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                              dados_means_sd$ROI == "T" &
                                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                              dados_means_sd$morphological_parameter == "S_age_decay"]
                          ) /
                            dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                              dados_means_sd$ROI == "T" &
                                                                              dados_means_sd$Diagnostic == "CONTROLE" &
                                                                              dados_means_sd$morphological_parameter == "S_age_decay"],
                          ifelse(
                            dados$Session == "2" & dados$ROI == "T",
                            (
                              S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                dados_means_sd$ROI == "T" &
                                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                dados_means_sd$morphological_parameter == "S_age_decay"]
                            ) /
                              dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                dados_means_sd$ROI == "T" &
                                                                                dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                dados_means_sd$morphological_parameter == "S_age_decay"],
                            ifelse(
                              dados$Session == "3" & dados$ROI == "T",
                              (
                                S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                  dados_means_sd$ROI == "T" &
                                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                  dados_means_sd$morphological_parameter == "S_age_decay"]
                              ) /
                                dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                  dados_means_sd$ROI == "T" &
                                                                                  dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                  dados_means_sd$morphological_parameter == "S_age_decay"],
                              ifelse(
                                dados$Session == "1" & dados$ROI == "O",
                                (
                                  S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                                    dados_means_sd$ROI == "O" &
                                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                    dados_means_sd$morphological_parameter == "S_age_decay"]
                                ) /
                                  dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "1" &
                                                                                    dados_means_sd$ROI == "O" &
                                                                                    dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                    dados_means_sd$morphological_parameter == "S_age_decay"],
                                ifelse(
                                  dados$Session == "2" & dados$ROI == "O",
                                  (
                                    S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                                      dados_means_sd$ROI == "O" &
                                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                      dados_means_sd$morphological_parameter == "S_age_decay"]
                                  ) /
                                    dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "2" &
                                                                                      dados_means_sd$ROI == "O" &
                                                                                      dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                      dados_means_sd$morphological_parameter == "S_age_decay"],
                                  ifelse(
                                    dados$Session == "3" & dados$ROI == "O",
                                    (
                                      S_age_decay - dados_means_sd$mean.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                                        dados_means_sd$ROI == "O" &
                                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                                        dados_means_sd$morphological_parameter == "S_age_decay"]
                                    ) /
                                      dados_means_sd$sd.morphological_parameter_value[dados_means_sd$Session == "3" &
                                                                                        dados_means_sd$ROI == "O" &
                                                                                        dados_means_sd$Diagnostic == "CONTROLE" &
                                                                                        dados_means_sd$morphological_parameter == "S_age_decay"],
                                    ""
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
  ))
