# ORGANIZING ALL DATA AND INCLUDING VARIABLES ####

# remove extra info ----
# tabela_sujeitos <- dplyr::select(tabela_sujeitos, -c(participant_id, session_id))

# combina todas as tabelas ----
data <- full_join(data_longcor_ses1, data_longcor_ses1_5) %>%
  full_join(data_longcor_ses1_20) %>%
  full_join(data_longcor_ses1_30) %>%
  full_join(data_longcor_ses1_50) %>%
  full_join(data_longcor_ses1_75) %>%
  full_join(data_longcor_ses1_100) %>%
  full_join(data_h) %>%
  dplyr::select(-c(PialFullArea, WhiteFullArea, SmoothPialFullArea, ConvexHullFullArea, PialFullVol, WhiteFullVol, SmoothPialFullVol)) %>% unique()

# combina as tabelas geradas (Surfarea + Thickness + Volume + lGI = data) através do sujeito
dados <- inner_join(tabela_sujeitos, data)
dados <- dados %>% filter(Session == 1) %>% dplyr::select(-c(participant_id, Session, session_id, acq_date, birthday, SUBJ_antigo, NeuroQuant, ESC, Birthdate, Lipoxina))

colnames(dados)[which(names(dados) == "Sexo")] <- "Gender"
colnames(dados)[which(names(dados) == "Idade")] <- "Age"
colnames(dados)[which(names(dados) == "Diagnostico")] <- "Diagnostic"

dados$Age <- as.numeric(dados$Age)
dados$Diagnostic <- as.factor(dados$Diagnostic)
dados$Gender <- as.factor(dados$Gender)

