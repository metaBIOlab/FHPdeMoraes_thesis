# diferenca no kteorico

dados_wide = within(dados_wide, {
  log_kteoricoDiff = ifelse(
    is.na(dados_wide$K.3),
    dados_wide$K.2 - dados_wide$K.1,
    ifelse((dados_wide$K.3 >= 0),
           dados_wide$K.3 - dados_wide$K.1,NA
    )
  )
})