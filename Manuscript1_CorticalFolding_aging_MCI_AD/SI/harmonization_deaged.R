# Decreasing rate ----

dados_datasetscomp_rate <-
  filter(
    dados_datasetscomp,
    ROI == "hemisphere",
    Diagnostic == "CTL" |
      Diagnostic == "MCI" |
      Diagnostic == "AD",
    Sample != "IDOR-CCD-Control"
  )
#dados_datasetscomp_rate$Diagnostic <- factor(dados_datasetscomp_rate$Diagnostic, levels = c("CTL", "MCI","AD"))
dados_datasetscomp_rate$Sample <- as.factor(dados_datasetscomp_rate$Sample)

## AvgThickness \~ Age ----

m.1 <- lme4::lmer(logAvgThickness_age_decay ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic) , data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%  filter(grpvar == "Sample") %>% mutate(T_shift = condval, Sample = grp) %>% dplyr::select(-c(condval, grpvar, term, condsd, grp))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>% mutate(logAvgThickness_age_decay_shiftc = logAvgThickness_age_decay - T_shift)

## TotalArea \~ Age ----

m.1 <- lme4::lmer(logTotalArea_age_decay ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic) , data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>%  filter(grpvar == "Sample") %>% mutate(AT_shift = condval, Sample = grp) %>% dplyr::select(-c(condval, grpvar, term, condsd, grp))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>% mutate(logTotalArea_age_decay_shiftc = logTotalArea_age_decay - AT_shift)

## ExposedArea \~ Age ----

m.1 <- lme4::lmer(logExposedArea_age_decay ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic) , data = dados_datasetscomp_rate)

re <- as_tibble(ranef(m.1)) %>% filter(grpvar == "Sample") %>% mutate(AE_shift = condval, Sample = grp) %>% dplyr::select(-c(condval, grpvar, term, condsd, grp))

dados_datasetscomp <- full_join(dados_datasetscomp, re) %>% mutate(logExposedArea_age_decay_shiftc = logExposedArea_age_decay - AE_shift)

## GI \~ Age ----

# m.1 <- lme4::lmer(localGI ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic), data = dados_datasetscomp_rate)
# 
# re <- as_tibble(ranef(m.1)) %>%  filter(grpvar == "Sample") %>% mutate(GI_shift = condval, Sample = grp) %>% dplyr::select(-c(condval, grpvar, term, condsd, grp))
# 
# dados_datasetscomp <- full_join(dados_datasetscomp, re) %>% mutate(localGI_shiftc = localGI - GI_shift)

## K \~ Age ----

# m.1 <- lme4::lmer(K ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic), data = dados_datasetscomp_rate)
# 
# re <- as_tibble(ranef(m.1)) %>% filter(grpvar == "Sample") %>% mutate(K_shift = condval, Sample = grp) %>% dplyr::select(-c(condval, grpvar, term, condsd, grp))
# 
# dados_datasetscomp <- full_join(dados_datasetscomp, re) %>% mutate(K_shiftc = K - K_shift)

## S \~ Age ----

# m.1 <- lme4::lmer(S ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic) , data = dados_datasetscomp_rate)
# 
# re <- as_tibble(ranef(m.1)) %>%  filter(grpvar == "Sample") %>% mutate(S_shift = condval, Sample = grp) %>% dplyr::select(-c(condval, grpvar, term, condsd, grp))
# 
# dados_datasetscomp <- full_join(dados_datasetscomp, re) %>% mutate(S_shiftc = S - S_shift)

## I \~ Age ----

# m.1 <- lme4::lmer(I ~ Age * Diagnostic + (1|Sample) + (1|Sample:Diagnostic) , data = dados_datasetscomp_rate)
# 
# re <- as_tibble(ranef(m.1)) %>% filter(grpvar == "Sample") %>% mutate(I_shift = condval, Sample = grp) %>% dplyr::select(-c(condval, grpvar, term, condsd, grp))
# 
# dados_datasetscomp <- full_join(dados_datasetscomp, re) %>% mutate(I_shiftc = I - I_shift)
