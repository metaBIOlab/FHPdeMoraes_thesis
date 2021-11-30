
# deaging

decay_logMMSE <-
  filter(dados, Session == "1") %>%
  group_by(Diagnostic) %>%
  do(fit_decay_logMMSE = rlm(log10(MMSE) ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_logMMSE_Coef = tidy(
  decay_logMMSE,
  fit_decay_logMMSE,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda, b e Ko CTL - sem correcao longitudinal ----

lambda_logMMSE <-
  decay_logMMSE_Coef$estimate[decay_logMMSE_Coef$Diagnostic == "CONTROLE" &
                                decay_logMMSE_Coef$term == "Age"]

b_logMMSE <-
  decay_logMMSE_Coef$estimate[decay_logMMSE_Coef$Diagnostic == "CONTROLE" &
                                decay_logMMSE_Coef$term == "(Intercept)"]


decay_logLipoxina <-
  filter(dados, Session == "1") %>%
  group_by(Diagnostic) %>%
  do(fit_decay_logLipoxina = rlm(log10(Lipoxina) ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_logLipoxina_Coef = tidy(
  decay_logLipoxina,
  fit_decay_logLipoxina,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda, b e Ko CTL - sem correcao longitudinal ----

lambda_logLipoxina <-
  decay_logLipoxina_Coef$estimate[decay_logLipoxina_Coef$Diagnostic == "CONTROLE" &
                                decay_logLipoxina_Coef$term == "Age"]

b_logLipoxina <-
  decay_logLipoxina_Coef$estimate[decay_logLipoxina_Coef$Diagnostic == "CONTROLE" &
                                decay_logLipoxina_Coef$term == "(Intercept)"]


decay_logLipoxina <-
  filter(dados, Session == "1") %>%
  group_by(Diagnostic) %>%
  do(fit_decay_logLipoxina = rlm(log10(Lipoxina) ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_logLipoxina_Coef = tidy(
  decay_logLipoxina,
  fit_decay_logLipoxina,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda, b e Ko CTL - sem correcao longitudinal ----

lambda_logLipoxina <-
  decay_logLipoxina_Coef$estimate[decay_logLipoxina_Coef$Diagnostic == "CONTROLE" &
                                    decay_logLipoxina_Coef$term == "Age"]

b_logLipoxina <-
  decay_logLipoxina_Coef$estimate[decay_logLipoxina_Coef$Diagnostic == "CONTROLE" &
                                    decay_logLipoxina_Coef$term == "(Intercept)"]



dados_ses1_clincal <-
  filter(dados, Session == "1") %>% droplevels() %>% mutate(
    logMMSE_age_decay = as.numeric(
      log10(MMSE) - b_logMMSE - lambda_logMMSE * Age),
    logLipoxina_age_decay = as.numeric(
      log10(Lipoxina) - b_logLipoxina - lambda_logLipoxina * Age),
    
    )


dados <- full_join(dados, dados_ses1_clincal)

# NORMALIZACAO ----

dados <- dados %>%
  mutate(z_MMSE = (dados$MMSE - mean(dados$MMSE[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$MMSE[dados$Diagnostic == "CONTROLE"], na.rm = TRUE), 
         z_A7_A5 = (dados$`A7/A5` - mean(dados$`A7/A5`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$`A7/A5`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE), 
         z_TMT_B_A = (dados$`TMT B-A` - mean(dados$`TMT B-A`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$`TMT B-A`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE),
         z_relogio = (dados$relogio - mean(dados$relogio[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$relogio[dados$Diagnostic == "CONTROLE"], na.rm = TRUE), 
         z_DIGIT_SPAN_BACK = (dados$`DIGIT SPAN BACK` - mean(dados$`DIGIT SPAN BACK`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$`DIGIT SPAN BACK`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE),
         z_AB1_40 = (dados$`AB1-40` - mean(dados$`AB1-40`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$`AB1-40`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE),
         z_AB1_42 = (dados$`AB1-42` - mean(dados$`AB1-42`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$`AB1-42`[dados$Diagnostic == "CONTROLE"], na.rm = TRUE),
         z_TAU = (dados$TAU - mean(dados$TAU[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$TAU[dados$Diagnostic == "CONTROLE"], na.rm = TRUE),
         z_AB1_ratio = (dados$AB1_ratio - mean(dados$AB1_ratio[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$AB1_ratio[dados$Diagnostic == "CONTROLE"], na.rm = TRUE),
         z_TAU_AB1_42_ratio = (dados$TAU_AB1_42_ratio - mean(dados$TAU_AB1_42_ratio[dados$Diagnostic == "CONTROLE"], na.rm = TRUE)) / sd(dados$TAU_AB1_42_ratio[dados$Diagnostic == "CONTROLE"], na.rm = TRUE))
         
