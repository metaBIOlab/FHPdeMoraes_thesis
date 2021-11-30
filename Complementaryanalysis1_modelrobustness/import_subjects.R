# Tabela dos sujeitos

####informacoes dos sujeitos####

tabela_sujeitos <- as_tibble(
  read_delim(
    "data/tabela_sujeitos.csv",
    ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ",",
                    grouping_mark = "."),
    trim_ws = TRUE
  )
)

all_sessions <- read_delim(
  str_c("data/all_sessions.tsv"),
  "\t",
  escape_double = FALSE,
  trim_ws = TRUE
)

participants <-
  read_delim(
    str_c("data/participants.tsv"),
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE
  )

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
  ) %>% dplyr::select(-c(acq_time, dob))

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
  tabela_sujeitos %>% rename_at(vars(oldnames), ~ newnames)

tabela_sujeitos <-
  dplyr::select(tabela_sujeitos,-c(acq_date, machine, Idade, dob))

part_ses$Session <- as.numeric(part_ses$Session)

tabela_sujeitos <- full_join(part_ses, tabela_sujeitos)

rm(oldnames)
rm(newnames)

# Age intervals ----
# 
# tabela_sujeitos$Age_interval <- cut(tabela_sujeitos$Age,
#                                        breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100),
#                                        right = FALSE,
#                                        include.lowest = TRUE)
# 
# tabela_sujeitos$Age_interval10 <- cut(tabela_sujeitos$Age,
#                                          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                                          right = FALSE,
#                                          include.lowest = TRUE)


tabela_sujeitos <- dplyr::select(tabela_sujeitos,-c(SUBJ_antigo, Birthdate, ESC, acq_date, NeuroQuant, Lipoxina))

write.csv(tabela_sujeitos, "tabela_sujeitos.csv")
