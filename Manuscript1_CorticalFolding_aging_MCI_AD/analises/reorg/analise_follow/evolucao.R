# DK/DT

source("analises/analise_follow/diferenca_idade.R")

source("analises/analise_follow/diferenca_ks.R")

source("analises/analise_follow/diferenca_diagnostico.R")

dkdt <- dados_wide$log_kteoricoDiff / dados_wide$AgeDiff

dados_wide <-
  cbind(dados_wide, dkdt)

evolucao <- dados_wide %>%
  as_data_frame %>%
  select(
    SUBJ_ses,
    hemi,
    ROI,
    DiagnosticDiff,
    AgeDiff,
    log_kteoricoDiff,
    dkdt
  )

dados <- left_join(dados, evolucao)

rm(dkdt)
