# Decreasing rate ----

dados_datasetscomp_rate <-
  filter(
    dados_datasetscomp,
    ROI == "hemisphere")

#dados_datasetscomp_rate$Diagnostic <- factor(dados_datasetscomp_rate$Diagnostic, levels = c("CTL", "MCI","AD"))
dados_datasetscomp_rate$Sample <- as.factor(dados_datasetscomp_rate$Sample)

## AvgThickness \~ Age ----

m.1 <- lme4::lmer(log10(AvgThickness) ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic) , data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%
  filter(grpvar == "Sample") %>%
  mutate(T_shift = condval, Sample = grp) %>%
  dplyr::select(-c(condval, grpvar, term, condsd, grp))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>%
  mutate(logAvgThickness_shiftc = log10(AvgThickness) - T_shift,
         logAvgThickness_age_decay = log10(AvgThickness) - as_tibble(fixef(m.1))$value[2]*(Age-Age.cor),
         logAvgThickness_age_decay_shiftc = log10(AvgThickness) - T_shift - as_tibble(fixef(m.1))$value[2]*(Age-Age.cor))
         # logAvgThickness_shiftc = log10(AvgThickness_shiftc),
         # logAvgThickness_age_decay = log10(AvgThickness_age_decay),
         # logAvgThickness_age_decay_shiftc = log10(AvgThickness_age_decay_shiftc))

## TotalArea \~ Age ----

m.1 <- lme4::lmer(log10(TotalArea) ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic) , data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%
  filter(grpvar == "Sample") %>%
  mutate(AT_shift = condval, Sample = grp) %>%
  dplyr::select(-c(condval, grpvar, term, condsd, grp))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>%
  mutate(logTotalArea_shiftc = log10(TotalArea) - AT_shift,
         logTotalArea_age_decay = log10(TotalArea) - as_tibble(fixef(m.1))$value[2]*(Age-Age.cor),
         logTotalArea_age_decay_shiftc = log10(TotalArea) - AT_shift - as_tibble(fixef(m.1))$value[2]*(Age-Age.cor))
         # logTotalArea_shiftc = log10(TotalArea_shiftc),
         # logTotalArea_age_decay = log10(TotalArea_age_decay),
         # logTotalArea_age_decay_shiftc = log10(TotalArea_age_decay_shiftc))

## ExposedArea \~ Age ----

m.1 <- lme4::lmer(log10(ExposedArea) ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic) , data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%
  filter(grpvar == "Sample") %>%
  mutate(AE_shift = condval, Sample = grp) %>%
  dplyr::select(-c(condval, grpvar, term, condsd, grp))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>%
  mutate(logExposedArea_shiftc = log10(ExposedArea) - AE_shift,
         logExposedArea_age_decay = log10(ExposedArea) - as_tibble(fixef(m.1))$value[2]*(Age-Age.cor),
         logExposedArea_age_decay_shiftc = log10(ExposedArea) - AE_shift - as_tibble(fixef(m.1))$value[2]*(Age-Age.cor))
         # logExposedArea_shiftc = log10(ExposedArea_shiftc),
         # logExposedArea_age_decay = log10(ExposedArea_age_decay),
         # logExposedArea_age_decay_shiftc = log10(ExposedArea_age_decay_shiftc))

## GI \~ Age ----

m.1 <- lme4::lmer(localGI ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic), data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%
  filter(grpvar == "Sample") %>%
  mutate(GI_shift = condval, Sample = grp) %>%
  dplyr::select(-c(condval, grpvar, term, condsd, grp))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>%
  mutate(GI_shiftc = localGI - GI_shift,
         GI_age_decay = localGI - as_tibble(fixef(m.1))$value[2]*(Age-Age.cor),
         GI_age_decay_shiftc = localGI - GI_shift - as_tibble(fixef(m.1))$value[2]*(Age-Age.cor))

# K, S e I ----

dados_datasetscomp <- dados_datasetscomp %>%
  mutate(K_age_decay = logTotalArea_age_decay + 1/2 * logAvgThickness_age_decay - 5/4 * logExposedArea_age_decay,
         K_shiftc = logTotalArea_shiftc + 1/2 * logAvgThickness_shiftc - 5/4 * logExposedArea_shiftc,
         K_age_decay_shiftc = logTotalArea_age_decay_shiftc + 1/2 * logAvgThickness_age_decay_shiftc - 5/4 * logExposedArea_age_decay_shiftc,
         S_age_decay = 3 / 2 * logTotalArea_age_decay + 3 / 4 * logExposedArea_age_decay - 9 / 2 * logAvgThickness_shiftc,
         S_shiftc = 3 / 2 * logTotalArea_shiftc + 3 / 4 * logExposedArea_shiftc - 9 / 2 * logAvgThickness_shiftc,
         S_age_decay_shiftc = 3 / 2 * logTotalArea_age_decay_shiftc + 3 / 4 * logExposedArea_age_decay_shiftc - 9 / 2 * logAvgThickness_shiftc,
         I_age_decay = 2*logAvgThickness_age_decay + logTotalArea_age_decay + logExposedArea_age_decay,
         I_shiftc = 2*logAvgThickness_shiftc + logTotalArea_shiftc + logExposedArea_shiftc,
         I_age_decay_shiftc = 2*logAvgThickness_age_decay_shiftc + logTotalArea_age_decay_shiftc + logExposedArea_age_decay_shiftc,
         Knorm_shiftc = K_shiftc/sqrt(1 + (1/4)^2 + (5/2)^2),
         Snorm_shiftc = S_shiftc/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
         Inorm_shiftc = I_shiftc/sqrt(1^2 + 1^2 + 1^2),
         Knorm_age_decay = K_age_decay/sqrt(1 + (1/4)^2 + (5/2)^2),
         Snorm_age_decay = S_age_decay/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
         Inorm_age_decay = I_age_decay/sqrt(1^2 + 1^2 + 1^2),
         Knorm_age_decay_shiftc = K_age_decay_shiftc/sqrt(1 + (1/4)^2 + (5/2)^2),
         Snorm_age_decay_shiftc = S_age_decay_shiftc/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
         Inorm_age_decay_shiftc = I_age_decay_shiftc/sqrt(1^2 + 1^2 + 1^2))