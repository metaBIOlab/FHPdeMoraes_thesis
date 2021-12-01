# REORGANIZA EM FORMATO LONGO

# separa os resultados por visita na mesma linha do sujeito ----
dados <-
  dados %>% unite(SUBJses_hemi, SUBJ_ses, hemi) %>% unite(SUBJses_hemi_ROI, SUBJses_hemi, ROI, sep = "-")

# ORGANIZACAO DOS DADOS LONGITUDINAIS ----
source("organiza/reorganiza_dados_wide.R")

# VOLTA AO NORMAL -> separa os resultados por visita na mesma linha do sujeito ----

dados <-
  dados %>% separate(SUBJses_hemi_ROI,
                     into = c("SUBJses_hemi", "ROI"),
                     sep = "-") %>% separate(SUBJses_hemi, into = c("SUBJ_ses", "hemi"))
