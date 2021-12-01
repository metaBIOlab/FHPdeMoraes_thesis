# DEAGING FULL SAMPLE ---- , Age > 18, Sample == "NKI"

## AvgThickness ---
decay_AvgThickness <-
  filter(dados_datasetscomp, Diagnostic == "CTL", !is.na(logAvgThickness), !is.nan(logAvgThickness), !is.infinite(logAvgThickness)) %>%
  droplevels() %>%
  group_by(ROI, Sample) %>%
  do(fit_decay_AvgThickness = tidy(rlm(logAvgThickness ~ Age, data = .), conf.int=TRUE)) %>% unnest(cols = c(fit_decay_AvgThickness))

decay_AvgThickness <- filter(decay_AvgThickness,term == "Age") %>%
  mutate(c_AvgThickness = estimate,
         std_error_c_AvgThickness = std.error) %>%
  dplyr::select(c(ROI, Sample, c_AvgThickness, std_error_c_AvgThickness))

## TotalArea ---
decay_logTotalArea <- filter(dados_datasetscomp, Diagnostic == "CTL", !is.na(log10(TotalArea_corrected)), !is.nan(log10(TotalArea_corrected)), !is.infinite(log10(TotalArea_corrected))) %>%
  droplevels() %>%
  group_by(ROI, Sample) %>%
  do(fit_decay_logTotalArea = tidy(rlm(log10(TotalArea_corrected) ~ Age, data = .),conf.int=TRUE)) %>%
  unnest(cols = c(fit_decay_logTotalArea))

decay_logTotalArea <- filter(decay_logTotalArea, term == "Age") %>%
  mutate(c_logTotalArea = estimate,
         std_error_c_logTotalArea = std.error) %>%
  dplyr::select(c(ROI, Sample, c_logTotalArea, std_error_c_logTotalArea))

## ExposedArea ---
decay_logExposedArea <- filter(dados_datasetscomp, Diagnostic == "CTL", !is.na(logExposedArea_corrected), !is.nan(logExposedArea_corrected), !is.infinite(logExposedArea_corrected)) %>%
  droplevels() %>%
  group_by(ROI, Sample) %>%
  do(fit_decay_logExposedArea = tidy(rlm(logExposedArea_corrected ~ Age, data = .), conf.int = TRUE)) %>%
  unnest(cols = c(fit_decay_logExposedArea))

decay_logExposedArea <- filter(decay_logExposedArea, term == "Age")  %>%
  mutate(c_logExposedArea = estimate,
         std_error_c_logExposedArea = std.error) %>%
  dplyr::select(c(ROI, Sample, c_logExposedArea, std_error_c_logExposedArea))

## K ---
decay_K <- filter(dados_datasetscomp, Diagnostic == "CTL", !is.na(K), !is.nan(K), !is.infinite(K)) %>%
  droplevels() %>%
  group_by(ROI, Sample) %>%
  do(fit_decay_K = tidy(rlm(K ~ Age, data = .), conf.int = TRUE))  %>%
  unnest(cols = c(fit_decay_K))

decay_K <- filter(decay_K, term == "Age")  %>%
  mutate(c_K = estimate,
         std_error_c_K = std.error) %>%
  dplyr::select(c(ROI, Sample, c_K, std_error_c_K))

dados_datasetscomp <- full_join(dados_datasetscomp, decay_AvgThickness) %>%
  full_join(decay_logTotalArea) %>%
  full_join(decay_logExposedArea) %>%
  full_join(decay_K) %>%
  mutate(
    logAvgThickness_age_decay = logAvgThickness - c_AvgThickness * (Age - Age.cor),
    logTotalArea_age_decay = logTotalArea_corrected - c_logExposedArea * (Age - Age.cor),
    logExposedArea_age_decay = logExposedArea_corrected - c_logExposedArea * (Age - Age.cor),
    K_age_decay = logTotalArea_age_decay + 1/2*logAvgThickness_age_decay - 5/4*logExposedArea_age_decay,
    K_age_decay2 = K_corrected - c_K * (Age - Age.cor),
    I_age_decay = logTotalArea_age_decay + logExposedArea_age_decay + logAvgThickness_age_decay^2,
    S_age_decay = 3/2*logTotalArea_age_decay + 3/4*logExposedArea_age_decay - 9/4*logAvgThickness_age_decay^2,
    Knorm_age_decay = K_age_decay/sqrt(1 + (1/4)^2 + (5/4)^2),
    Snorm_age_decay = S_age_decay/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
    Inorm_age_decay = I_age_decay/sqrt(1^2+1^2+1^2))
