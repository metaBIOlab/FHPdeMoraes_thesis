# separa os dados_hemi_v1 para análise da lipoxina ----
# dados_lipoxina <- subset(dados_hemi_v1, Lipoxina != 0, drop = FALSE)

# separa os dados_hemi_v1 para analise da neuroquant ----
# dados_neuroquant <- subset(dados_hemi_v1, NeuroQuant != 0, drop = FALSE)

# separa os resultados por visita na mesma linha do sujeito ----
dados_hemi_v1 <- dados_hemi_v1 %>% unite(SUBJclean_hemi, SUBJ_ses, hemi)

# transforma a coluna em horizontal dividindo por visita do sujeito
dados_wide <-
  reshape(
    as.data.frame(dados_hemi_v1),
    idvar = 'SUBJclean_hemi',
    timevar = 'Session',
    direction = 'wide'
  )

dados_hemi_v1 <-
  dados_hemi_v1 %>% separate(SUBJclean_hemi, into = c("SUBJ_ses", "hemi"))
dados_wide <-
  dados_wide %>% separate(SUBJclean_hemi, into = c("SUBJ_ses", "hemi"))

dados_wide <-
  dplyr::select(dados_wide,-c(SUBJ_ses.2,
                              SUBJ_ses.3,
                              Gender.2,
                              Gender.3,
                              Birthdate.2,
                              Birthdate.3))

oldnames = c("SUBJ_ses.1",
             "Gender.1",
             "Birthdate.1")
newnames <- c("SUBJ_ses",
              "Gender",
              "Birthdate")

# retira os sujeitos com outros diagnosticos


# ORGANIZACAO DOS dados_hemi_v1 LONGITUDINAIS ----
dados_wide <- dados_wide %>% rename_at(vars(oldnames), ~ newnames)

# deleta os dummies ----
rm(oldnames)
rm(newnames)