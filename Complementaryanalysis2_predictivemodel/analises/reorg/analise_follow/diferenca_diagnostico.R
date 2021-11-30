# diferenca no diagnostico

dados_wide$DiagnosticDiff[is.na(dados_wide$Diagnostic.3) &
                            is.na(dados_wide$Diagnostic.2)] <- NA

dados_wide$DiagnosticDiff[is.na(dados_wide$Diagnostic.3) &
                            dados_wide$Diagnostic.1 == dados_wide$Diagnostic.2] <-
  "estavel_regrediu"


dados_wide$DiagnosticDiff[is.na(dados_wide$Diagnostic.3) &
                            dados_wide$Diagnostic.1 == "CONTROLE"&
                            dados_wide$Diagnostic.2 == "CCL"] <-
  "evoluiu"


dados_wide$DiagnosticDiff[is.na(dados_wide$Diagnostic.3) &
                            dados_wide$Diagnostic.1 == "CCL"&
                            dados_wide$Diagnostic.2 == "ALZ"] <-
  "evoluiu"

dados_wide$DiagnosticDiff[is.na(dados_wide$Diagnostic.3) &
                            dados_wide$Diagnostic.1 == "CCL"&
                            dados_wide$Diagnostic.2 == "CONTROLE"] <-
  "estavel_regrediu"

dados_wide$DiagnosticDiff[!is.na(dados_wide$Diagnostic.3) &
                            dados_wide$Diagnostic.1 == dados_wide$Diagnostic.3] <-
  "estavel_regrediu"

dados_wide$DiagnosticDiff[!is.na(dados_wide$Diagnostic.3) &
                            dados_wide$Diagnostic.1 == "CONTROLE"&
                            dados_wide$Diagnostic.3 == "CCL"] <-
  "evoluiu"

dados_wide$DiagnosticDiff[!is.na(dados_wide$Diagnostic.3) &
                            dados_wide$Diagnostic.1 == "CCL"&
                            dados_wide$Diagnostic.3 == "ALZ"] <-
  "evoluiu"

dados_wide$DiagnosticDiff[!is.na(dados_wide$Diagnostic.3) &
                            dados_wide$Diagnostic.1 == "CCL"&
                            dados_wide$Diagnostic.3 == "CONTROLE"] <-
  "estavel_regrediu"