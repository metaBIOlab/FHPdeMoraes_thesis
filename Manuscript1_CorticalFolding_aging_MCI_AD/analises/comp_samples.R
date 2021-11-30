## DATA JOIN ####

dados_datasetscomp <-
  dados %>% filter(
    machine == "Philips-Achieva",
    Diagnostic == "CONTROLE" |
      Diagnostic == "CCL" |
      Diagnostic == "ALZ",
    Session == "1",
    !is.na(logAvgThickness),
    localGI != 0 |
      !is.na(localGI),
    !is.infinite(logExposedArea),
      ROI == "hemisphere"
  ) %>% dplyr::select(
    SUBJ,
    Gender,
    Age,
    ROI,
    AvgThickness,
    TotalArea,
    ExposedArea,
    localGI,
    K,
    I,
    S,
    K_corrected,
    I_corrected,
    S_corrected,
    hemi,
    Age_interval,
    Diagnostic,
    logAvgThickness,
    logTotalArea,
    logExposedArea,
    logAvgThickness_age_decay,
    logTotalArea_age_decay,
    logExposedArea_age_decay,
    K_age_decay,
    I_age_decay,
    S_age_decay
  ) %>% mutate(Sample = "IDOR") %>% unique()

dados_datasetscomp$Gender <- as.factor(dados_datasetscomp$Gender)
dados_datasetscomp$Age <- as.character(dados_datasetscomp$Age)

dados_datasetscomp$Age <- as.double(dados_datasetscomp$Age)

### WITH MOTA&HOUZEL DATA ####
dados_datasetscomp <- full_join(dados_datasetscomp, dados_MH2015)
dados_datasetscomp <- filter(dados_datasetscomp, localGI != 0)

