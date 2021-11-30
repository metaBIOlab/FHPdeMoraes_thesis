dados_volume_CC <-
  dados_aseg_vol %>% mutate(
    WM_volume_CC = (
      CC_Posterior + CC_Anterior + CC_Central + CC_Mid_Anterior + CC_Mid_Posterior
    )
  )

dados_volume_CC <-
  dplyr::select(dados_volume_CC,
                c(SUBJ, Session, Longitudinal_correction, WM_volume_CC))

dados_volume_CC$Session <- as.double(dados_volume_CC$Session)

dados <- left_join(dados, dados_volume_CC)
