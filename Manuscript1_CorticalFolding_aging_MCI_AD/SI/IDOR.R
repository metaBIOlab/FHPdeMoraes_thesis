source("import_files/import_subjects.R") # arquivos dos sujeitos

tabela_sujeitos <- tabela_sujeitos %>% filter(Session == 1)

data <-
  read_csv(
    "C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_longcor_ses1.txt"
  )

data <- data %>% mutate(ROI = "hemisphere")

data_lobes <-
  read_csv(
    "C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_longcor_ses1_lobes.txt"
  )

data_lobes <- data_lobes %>% mutate(ROI = as.character(Lobe))

# Tabela dos sujeitos

####informacoes dos sujeitos####

# ----
data <- full_join(data, data_lobes) %>% mutate(SUBJ = SubjectID)

dados_idor <-
  inner_join(tabela_sujeitos, data) %>% mutate(Sample = "IDOR",
                                               machine = "Philips-Achieva 3T",
                                               FieldStrenght = 3)
# ABETA and Tau ----
alpha_beta <- read_excel("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/alpha_beta.xlsx", 
                         sheet = "Sheet1")

alpha_beta <- alpha_beta %>% mutate(Diagnostic_full = Diagnostic, Session = 1, AB1_ratio = `AB1-42`/`AB1-40`, TAU_AB1_42_ratio = TAU / `AB1-42`, TAU_AB1_ratio = TAU/AB1_ratio) %>% dplyr::select(-c(Diagnostic))

dados_idor <- full_join(dados_idor, alpha_beta)

# Cognitive data ----

clinic_data <-
  read_excel("C:/Users/ferna/Documents/idor/Gyrification/data/SUBJ FERNANDA H.xlsx", sheet = "Planilha1", na = "NA")

SUBJ_ESC_COGNITIVE <- read_excel("C:/Users/ferna/Documents/idor/Gyrification/data/SUBJ-ESC-COGNITIVE.xlsx")

#View(clinic_data)

colnames(clinic_data)[which(names(clinic_data) == "SUBJ_clean")] <-
  "SUBJ"

clinic_data <- mutate(clinic_data, Session = 1)

clinic_data$relogio <- as.integer(clinic_data$relogio)

clinic_data$Session <- as.integer(clinic_data$Session)
dados_idor$Session <- as.integer(dados_idor$Session)

dados_idor <- left_join(dados_idor, clinic_data)

dados_idor <- left_join(dados_idor, SUBJ_ESC_COGNITIVE)


# ----

dados_idor <- dados_idor %>%
  filter(
    Diagnostic == "CONTROLE" |
      Diagnostic == "CCL" |
      Diagnostic == "ALZ",
    !is.na(Age),
    SUBJ != "SUBJ211",
    SUBJ != "SUBJ223",
    SUBJ != "SUBJ231",
    SUBJ != "SUBJ136",
    SUBJ != "SUBJ014",
    SUBJ != "SUBJ128",
    SUBJ != "SUBJ157"
  ) %>%
  droplevels() %>%
  dplyr::select(-c(age))



write.csv(dados_idor, "IDOR.csv")
