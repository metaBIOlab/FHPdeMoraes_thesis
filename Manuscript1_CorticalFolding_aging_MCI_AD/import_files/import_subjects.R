# Tabela dos sujeitos

####informacoes dos sujeitos####

tabela_sujeitos <- read_delim("~/idor/Gyrification/data/tabela_sujeitos.csv", 
                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                  grouping_mark = "."), trim_ws = TRUE)

all_sessions <- read.delim("~/idor/Gyrification/data/all_sessions.tsv")

participants <- read.delim("~/idor/Gyrification/data/participants.tsv")

participants <- dplyr::select(participants,-c(group))

part_ses <- full_join(all_sessions, participants)

# cria a coluna subj

part_ses <-
  part_ses %>% mutate(
    SUBJ = str_sub(all_sessions$participant_id, 5, 11),
    Session = str_sub(all_sessions$session_id, 5, 5),
    birthday = as.Date(dob, "%d-%m-%Y"),
    acq_date = as_date(str_sub(all_sessions$acq_time, 1, 10)),
    Age = (acq_date - birthday) / 365
  ) %>% dplyr::select(-c(acq_time))

# vou deixar age com letra minuscula para diferenciar e ver a diferenca entre elas

# Age_ <-
#  as.period(tabela_sujeitos$Data_aquisicao_RM - tabela_sujeitos$Data_nascimento,
#            unit = "years")

# RENOMEIA AS VARIAVEIS ----
oldnames = c(
  "Sexo",
  "Data_nascimento",
  "Visita",
  "Idade",
  "Diagnostico",
  "RM  Maquina",
  "Data_aquisicao_RM"
)
newnames <- c("Gender",
              "Birthdate",
              "Session",
              "Age",
              "Diagnostic",
              "machine",
              "acq_date")

tabela_sujeitos <-
  tabela_sujeitos %>% rename_at(vars(oldnames), ~ newnames) %>% mutate(SUBJ = SUBJ_clean)

tabela_sujeitos <-
  dplyr::select(tabela_sujeitos,-c(acq_date, machine, Age))

part_ses$Session <- as.numeric(part_ses$Session)

tabela_sujeitos <- full_join(part_ses, tabela_sujeitos)

rm(oldnames)
rm(newnames)

tabela_sujeitos <- dplyr::select(tabela_sujeitos,-c(dob, age, sex))
