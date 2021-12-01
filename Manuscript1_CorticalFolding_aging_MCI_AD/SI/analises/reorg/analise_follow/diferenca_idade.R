# diferenca de idade

dados_wide = within(dados_wide, {
  AgeDiff = ifelse(
    is.na(dados_wide$Age.3),
    dados_wide$Age.2 - dados_wide$Age.1,
    ifelse((dados_wide$Age.3 >= 0), dados_wide$Age.3 - dados_wide$Age.1)
  )
})