# PREPARACAO ----
#source("analises/preparing-yujiang-datasets.R")
#source("analises/preparing-yujiang-datasets-agedecay_25.R")
source("analises/preparing-Motahouzel-dataset.R")
# source("Wang2016_OASIS.R")
# source("Wang2016_HCP.R")
# source("Wang2019_ADNI2.R")
# source("Wang2019_HCP2.R")
# source("Wang2019_IXI2.R")
# source("Wang2019_NKI.R")
# source("IDOR.R")
# source("AOMIC.R")
source("ACC.R")
source("ACC_caltechucsf.R")
# source("AHEAD.R")
source("ZK.R")
source("Micro.R")
# source("AOMIC-PIOP2.R")
# source("HCPINFANT.R")

# junta as amostras para comparacao ----
dados_datasetscomp <-
 full_join(dados_ACC, dados_caltechucsf) %>%
  full_join(dados_micro) %>%
  full_join(dados_zk)

dados_datasetscomp <- dados_datasetscomp %>%
  mutate(
    hemi = Hemisphere,
    AvgThickness = AvgCortThickness,
    TotalArea = PialArea,
    ExposedArea = SmoothPialArea
  ) 

source("analises/variables.R")

dados_datasetscomp$ROI <- as.factor(dados_datasetscomp$ROI)
dados_datasetscomp$Diagnostic <- as.factor(dados_datasetscomp$Diagnostic)
dados_datasetscomp$Sample <- as.factor(dados_datasetscomp$Sample)
dados_datasetscomp$Gender <- as.factor(dados_datasetscomp$Gender)

dados_datasetscomp <- dados_datasetscomp %>%
  dplyr::select(
    -c(Lobe,
      SubjectID,
      Lobe,
      AvgCortThickness,
      PialArea,
      WhiteArea,
      SmoothPialArea,
      GreymatterVol,
      Hemisphere
    )
  )  %>%
  filter(
    !is.na(TotalArea),
    ExposedArea != 0,!is.na(localGI),!is.infinite(AvgThickness),!is.na(Diagnostic), AvgThickness !=0,
    localGI != 0,!is.infinite(AvgThickness),!is.na(AvgThickness)
    ) %>%
  droplevels() %>%
  unique()

dados_datasetscomp$Diagnostic <- as.character(dados_datasetscomp$Diagnostic)

dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "CONTROLE"] <- "CTL"
dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "Control"] <- "CTL"
dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "AgCC"] <- "CCD"

dados_datasetscomp$Diagnostic <- factor(dados_datasetscomp$Diagnostic)

dados_datasetscomp$Sample <- as.character(dados_datasetscomp$Sample)

dados_datasetscomp$Sample[dados_datasetscomp$Sample == "IDOR-CCD"] <- "IDOR"

dados_datasetscomp$Sample <- factor(dados_datasetscomp$Sample)

# dados_datasetscomp$Gender[dados_datasetscomp$Gender == "F"] <- "FEM"
# dados_datasetscomp$Gender[dados_datasetscomp$Gender == "M"] <- "MASC"

dados_datasetscomp_excluded <- filter(dados_datasetscomp, is.nan(Age), is.na(Age), is.na(Gender)) %>%
  droplevels()

dados_datasetscomp <- filter(dados_datasetscomp, !is.nan(Age), !is.na(Age), !is.na(Gender)) %>%
  droplevels()

# DEAGING + HARMONIZATION FULL SAMPLE ---- , Age > 18, Sample == "NKI"

dados_datasetscomp_rate <-
  filter(dados_datasetscomp, Diagnostic == "CTL")

#dados_datasetscomp_rate$Diagnostic <- factor(dados_datasetscomp_rate$Diagnostic, levels = c("CTL", "MCI","AD"))
dados_datasetscomp_rate$Sample <-
  as.factor(dados_datasetscomp_rate$Sample)
dados_datasetscomp_rate$ROI <-
  factor(dados_datasetscomp_rate$ROI,
         levels = c("hemisphere", "F", "O", "P", "T"))


## AvgThickness ---
m.1 <-
  lme4::lmer(AvgThickness ~ Age * ROI + (1 | Sample:ROI), data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%
  filter(grpvar == "Sample:ROI") %>%
  mutate(
    T_shift = condval,
    sd_T_shift = condsd,
    Sample = str_split(grp, pattern = ":", simplify = TRUE)[, 1],
    ROI = str_split(grp, pattern = ":", simplify = TRUE)[, 2]
  ) %>%
  dplyr::select(-c(condval, grpvar, term, condsd, grp))

Age.trend <- as_tibble(lstrends(m.1, ~ ROI, var = "Age")) %>%
  mutate(Age.trend_T = Age.trend) %>%
  dplyr::select(c(ROI, Age.trend_T))

dados_datasetscomp <- full_join(dados_datasetscomp, Age.trend) %>%
  full_join(re) %>%
  mutate(
    AvgThickness_shiftc = AvgThickness - T_shift,
    AvgThickness_age_decay = AvgThickness - Age.trend_T * (Age - Age.cor),
    AvgThickness_age_decay_shiftc = AvgThickness - T_shift - Age.trend_T *
      (Age - Age.cor),
    logAvgThickness_shiftc = log10(AvgThickness_shiftc),
    logAvgThickness_age_decay = log10(AvgThickness_age_decay),
    logAvgThickness_age_decay_shiftc = log10(AvgThickness_age_decay_shiftc)
  )

## TotalArea ---
m.1 <-
  lme4::lmer(TotalArea ~ Age * ROI + (1 | Sample:ROI), data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%
  filter(grpvar == "Sample:ROI") %>%
  mutate(
    AT_shift = condval,
    sd_AT_shift = condsd,
    Sample = str_split(grp, pattern = ":", simplify = TRUE)[, 1],
    ROI = str_split(grp, pattern = ":", simplify = TRUE)[, 2]
  ) %>%
  dplyr::select(-c(condval, grpvar, term, condsd, grp))

Age.trend <-
  as_tibble(lstrends(m.1, ~ ROI, var = "Age")) %>%
  mutate(Age.trend_AT = Age.trend) %>%
  dplyr::select(c(ROI, Age.trend_AT))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>%
  full_join(Age.trend) %>%
  mutate(
    TotalArea_shiftc = TotalArea - AT_shift,
    TotalArea_age_decay = TotalArea - Age.trend_AT * (Age - Age.cor),
    TotalArea_age_decay_shiftc = TotalArea - AT_shift - Age.trend_AT *
      (Age - Age.cor),
    logTotalArea_shiftc = log10(TotalArea_shiftc),
    logTotalArea_age_decay = log10(TotalArea_age_decay),
    logTotalArea_age_decay_shiftc = log10(TotalArea_age_decay_shiftc)
  )

## ExposedArea ---
m.1 <-
  lme4::lmer(
    ExposedArea  ~ Age * ROI + (1 | Sample:ROI), data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%
  filter(grpvar == "Sample:ROI") %>%
  mutate(
    AE_shift = condval,
    sd_AE_shift = condsd,
    Sample = str_split(grp, pattern = ":", simplify = TRUE)[, 1],
    ROI = str_split(grp, pattern = ":", simplify = TRUE)[, 2]
  ) %>%
  dplyr::select(-c(condval, grpvar, term, condsd, grp))

Age.trend <-
  as_tibble(lstrends(m.1, ~ ROI, var = "Age")) %>%
  mutate(Age.trend_AE = Age.trend) %>%
  dplyr::select(c(ROI, Age.trend_AE))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>%
  full_join(Age.trend) %>%
  mutate(
    ExposedArea_shiftc = ExposedArea - AE_shift,
    ExposedArea_age_decay = ExposedArea - Age.trend_AE * (Age - Age.cor),
    ExposedArea_age_decay_shiftc = ExposedArea - AE_shift - Age.trend_AE * (Age - Age.cor),
    logExposedArea_shiftc = log10(ExposedArea_shiftc),
    logExposedArea_age_decay = log10(ExposedArea_age_decay),
    logExposedArea_age_decay_shiftc = log10(ExposedArea_age_decay_shiftc)
  )

# ----
dados_datasetscomp <- dados_datasetscomp %>%
  mutate(
    K_age_decay = log10(TotalArea_age_decay) + 1/4*log10(AvgThickness_age_decay^2) - 5/4*log10(ExposedArea_age_decay),
    K_shiftc = log10(TotalArea_shiftc) + 1/4*log10(AvgThickness_shiftc^2) - 5/4*log10(ExposedArea_shiftc),
    K_age_decay_shiftc = log10(TotalArea_age_decay_shiftc) + 1/4*log10(AvgThickness_age_decay_shiftc^2) - 5/4*log10(ExposedArea_age_decay_shiftc),
    I_age_decay = log10(TotalArea_age_decay) + log10(ExposedArea_age_decay) + log10(AvgThickness_age_decay^2),
    I_shiftc = log10(TotalArea_shiftc) + log10(ExposedArea_shiftc) + log10(AvgThickness_shiftc^2),
    I_age_decay_shiftc = log10(TotalArea_age_decay_shiftc) + log10(ExposedArea_age_decay_shiftc) + log10(AvgThickness_age_decay_shiftc^2),
    S_age_decay = 3/2*log10(TotalArea_age_decay) + 3/4*log10(ExposedArea_age_decay) - 9/4*log10(AvgThickness_age_decay^2),
    S_shiftc = 3/2*log10(TotalArea_shiftc) + 3/4*log10(ExposedArea_shiftc) - 9/4*log10(AvgThickness_shiftc^2),
    S_age_decay_shiftc = 3/2*log10(TotalArea_age_decay_shiftc) + 3/4*log10(ExposedArea_age_decay_shiftc) - 9/4*log10(AvgThickness_age_decay_shiftc^2),
    Knorm_shiftc = K_shiftc / sqrt(1 + (1 / 4) ^ 2 + (5 / 2) ^ 2),
    Snorm_shiftc = S_shiftc / sqrt((3 / 2) ^ 2 + (3 / 4) ^ 2 + (9 / 4) ^ 2),
    Inorm_shiftc = I_shiftc / sqrt(1 ^ 2 + 1 ^ 2 + 1 ^ 2),
    Knorm_age_decay = K_age_decay / sqrt(1 + (1 / 4) ^ 2 + (5 / 2) ^ 2),
    Snorm_age_decay = S_age_decay / sqrt((3 / 2) ^ 2 + (3 / 4) ^ 2 + (9 / 4) ^ 2),
    Inorm_age_decay = I_age_decay / sqrt(1 ^ 2 + 1 ^ 2 + 1 ^ 2),
    Knorm_age_decay_shiftc = K_age_decay_shiftc / sqrt(1 + (1 / 4) ^ 2 + (5 / 2) ^ 2),
    Snorm_age_decay_shiftc = S_age_decay_shiftc / sqrt((3 / 2) ^ 2 +  (3 / 4) ^ 2 + (9 / 4) ^ 2),
    Inorm_age_decay_shiftc = I_age_decay_shiftc / sqrt(1 ^ 2 + 1 ^ 2 + 1 ^ 2)
  )

