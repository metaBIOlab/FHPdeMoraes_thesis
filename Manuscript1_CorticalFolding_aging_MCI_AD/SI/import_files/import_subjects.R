# Tabela dos sujeitos

####informacoes dos sujeitos####

tabela_sujeitos <- as_tibble(
  read_delim(
    "C:/Users/ferna/Documents/idor/Gyrification/data/tabela_sujeitos.csv",
    ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ",",
                    grouping_mark = "."),
    trim_ws = TRUE
  )
)

all_sessions <- read_delim(
  "C:/Users/ferna/Documents/idor/Gyrification/data/all_sessions.tsv",
  "\t",
  escape_double = FALSE,
  trim_ws = TRUE
)

participants <-
  read_delim(
    "C:/Users/ferna/Documents/idor/Gyrification/data/participants.tsv",
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
  ) %>% dplyr::select(-c(acq_time))


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
  tabela_sujeitos %>% rename_at(vars(oldnames), ~ newnames) %>%
  mutate(SUBJ = SUBJ_clean) %>% 
  dplyr::select(-c(SUBJ_clean)) 

tabela_sujeitos <-
  dplyr::select(tabela_sujeitos,-c(acq_date, Age, NeuroQuant, Lipoxina, Birthdate, machine))
part_ses <-
  dplyr::select(part_ses,-c(participant_id , session_id, dob, sex))

part_ses$Age <- as.numeric(part_ses$Age)
part_ses$Session <- as.numeric(part_ses$Session)

tabela_sujeitos <- full_join(part_ses, tabela_sujeitos) %>% dplyr::select(-c(age))


rm(oldnames)
rm(newnames)

# SEPARA POR INTERVALO DE IDADE, A CADA 5 ANOS ----

tabela_sujeitos <- tabela_sujeitos %>%
  mutate(
    Age_interval = ifelse(
      Age >= 40 & Age < 45,
      "41-45",
      ifelse(
        Age >= 45 & Age < 50,
        "46-50",
        ifelse(
          Age >= 50 & Age < 55,
          "51-55",
          ifelse(
            Age >= 55 & Age < 60,
            "56-60",
            ifelse(
              Age >= 60 & Age < 65,
              "61-65",
              ifelse(
                Age >= 65 & Age < 70,
                "66-70",
                ifelse(
                  Age >= 70 & Age < 75,
                  "71-75",
                  ifelse(
                    Age >= 75 & Age < 80,
                    "76-80",
                    ifelse(Age >= 80 & Age < 85, "81-85",
                           ifelse(Age >= 85 & Age < 90, "86-90", ""))
                  )
                )
              )
            )
          )
        )
      )
    ),
    Age_interval10 = ifelse(Age >= 40 & Age < 50, "40",
                            ifelse(
                              Age >= 50 & Age < 60, "50",
                              ifelse(Age >= 60 & Age < 70, "60",
                                     ifelse(
                                       Age >= 70 & Age < 80, "70",
                                       ifelse(Age >= 80 & Age < 90, "80", "")
                                     ))
                            ))
  )

tabela_sujeitos <- filter(tabela_sujeitos, !is.na(machine))
