dados_Philips <-
  filter(dados_Philips,
         Diagnostic == "CONTROLE"|
           Diagnostic == "CCL"| Diagnostic == "ALZ")

follows <-
  filter(
    dados_Philips,
    !is.na(dkdt),
    Diagnostic == "CONTROLE"|
      Diagnostic == "CCL"| Diagnostic == "ALZ"
  )

summary_dados_Philips <- dados_Philips %>%
  group_by(Session, Gender) %>%
  summarise(
    N = n_distinct(SUBJ_ses),
    Mean = mean(Age),
    Max = max(Age),
    Min = min(Age),
    Median = median(Age),
    Std = sd(Age)
  )


summary_follows <- follows %>%
  group_by(Session) %>%
  summarise(
    N = n_distinct(SUBJ_ses),
    Mean = mean(Age),
    Max = max(Age),
    Min = min(Age),
    Median = median(Age),
    Std = sd(Age)
  )