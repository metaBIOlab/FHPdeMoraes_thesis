# REORGANIZA dados_hemi_v1 WIDE

dados_wide <-
  reshape(
    as.data.frame(dados),
    idvar = 'SUBJclean_hemi_ROI',
    timevar = 'Session',
    direction = 'wide'
  )

dados_wide <-
  dados_wide %>% separate(SUBJclean_hemi_ROI, into = c("SUBJclean_hemi", "ROI"), sep = "-") %>% separate(SUBJclean_hemi, into = c("SUBJ_ses", "hemi"))

dados_wide <-
  dplyr::select(dados_wide,
                -c(SUBJ_ses.2,
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
