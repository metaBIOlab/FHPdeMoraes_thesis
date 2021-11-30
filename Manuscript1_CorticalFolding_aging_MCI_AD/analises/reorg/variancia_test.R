dados <- dados %>% mutate(
  z_log_AvgThickness_corr_decay = case_when(
    dados$ROI == "hemisphere"~ (
      dados$log_AvgThickness_corr_decay - mean(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"])
    ) / sd(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"]),
    dados$ROI == "F"~ (
      dados$log_AvgThickness_corr_decay - mean(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"])
    ) / sd(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"]),
    dados$ROI == "P"~ (
      dados$log_AvgThickness_corr_decay - mean(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"])
    ) / sd(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"]),
    dados$ROI == "T"~ (
      dados$log_AvgThickness_corr_decay - mean(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"])
    ) / sd(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"]),
    dados$ROI == "O"~ (
      dados$log_AvgThickness_corr_decay - mean(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
    ) / sd(dados$log_AvgThickness_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
  )
) %>% mutate(
  z_log_TotalArea_corr_decay = case_when(
    dados$ROI == "hemisphere"~ (
      dados$log_TotalArea_corr_decay - mean(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"])
    ) / sd(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"]),
    dados$ROI == "F"~  (
      dados$log_TotalArea_corr_decay - mean(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"])
    ) / sd(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"]),
    dados$ROI == "P"~ (
      dados$log_TotalArea_corr_decay - mean(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"])
    ) / sd(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"]),
    dados$ROI == "T"~ (
      dados$log_TotalArea_corr_decay - mean(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"])
    ) / sd(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"]),
    dados$ROI == "O"~ (
      dados$log_TotalArea_corr_decay - mean(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
    ) / sd(dados$log_TotalArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
  )
) %>% mutate(
  z_log_SmoothArea_corr_decay = case_when(
    dados$ROI == "hemisphere"~ (
      dados$log_SmoothArea_corr_decay - mean(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"])
    ) / sd(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"]),
    dados$ROI == "F"~ (
      dados$log_SmoothArea_corr_decay - mean(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"])
    ) / sd(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"]),
    dados$ROI == "P"~ (
      dados$log_SmoothArea_corr_decay - mean(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"])
    ) / sd(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"]),
    dados$ROI == "T"~ (
      dados$log_SmoothArea_corr_decay - mean(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"])
    ) / sd(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"]),
    dados$ROI == "O"~ (
     dados$log_SmoothArea_corr_decay - mean(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
    ) / sd(dados$log_SmoothArea_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
  )
) %>% mutate(
  z_localGI_corr_decay = case_when(
    dados$ROI == "hemisphere"~ (
      dados$localGI_corr_decay - mean(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"])
      )/ sd(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"]),
    dados$ROI == "F"~ (
      dados$localGI_corr_decay - mean(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"])
      )/ sd(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"]),
    dados$ROI == "P"~ (
      dados$localGI_corr_decay - mean(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"])
      ) / sd(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"]),
    dados$ROI == "T"~ (
      dados$localGI_corr_decay - mean(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"])
      ) / sd(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"]),
    dados$ROI == "O"~ (
      dados$localGI_corr_decay - mean(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
      ) / sd(dados$localGI_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
  )
) %>% mutate(
  z_K_corr_decay = case_when(
    dados$ROI == "hemisphere"~ (
      dados$K_corr_decay - mean(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"])
    ) / sd(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "hemisphere"]),
    dados$ROI == "F"~ (
      dados$K_corr_decay - mean(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"])
    ) / sd(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "F"]),
    dados$ROI == "P"~ (
      dados$K_corr_decay - mean(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"])
    ) / sd(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "P"]),
    dados$ROI == "T"~ (
      dados$K_corr_decay - mean(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"])
    ) / sd(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "T"]),
    dados$ROI == "O"~ (
      dados$K_corr_decay - mean(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
    ) / sd(dados$K_corr_decay[dados$Diagnostic == "CONTROLE"& dados$ROI == "O"])
  )
)
