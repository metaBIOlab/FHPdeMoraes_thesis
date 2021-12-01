
data <-
  read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_zk.txt")

data <- data %>% mutate(
  ROI = "hemisphere")

data_lobes <-
  read_csv(
    "C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_zk_lobes.txt"
  )

data_lobes <- data_lobes %>% mutate(ROI = as.character(Lobe))

aseg_stats_ZK <- read_table("~/idor/Gyrification/data/resultados/aseg_stats_ZK.txt") %>%
  mutate(SUBJ = `Measure:volume`) %>%
  dplyr::select(SUBJ, CSF, EstimatedTotalIntraCranialVol)

# Tabela dos sujeitos

####informacoes dos sujeitos####

tabela_sujeitos <-
  read_excel("C:/Users/ferna/Documents/idor/Gyrification/data/t1w fase 2 freesurfer.xlsx")

# tabela_sujeitos <- tabela_sujeitos %>%
#   mutate(
#     Age = idade / 12,
#     Gender = ifelse(sexo == 0, "FEM", "MASC")) %>% dplyr::select(-c(idade, sexo))

# ----
dados_zk <- full_join(data, data_lobes) %>%
  mutate(SUBJ = SubjectID) %>%
  right_join(tabela_sujeitos) %>%
  left_join(aseg_stats_ZK) %>%
  dplyr::select(-c(SubjectID))
# passa as informacoes de area dos hemisferios para a coluna de area corrigida ----

# dados_zk$Diagnostic[dados_zk$SUBJ == "sub-SUBJ690"] <- "ZK" # confirmou ZK

# dados_zk$Diagnostic[dados_zk$SUBJ == "sub-SUBJ713"] <- "MICRO" # ZK muito grave com microcefalia

dados_zk_037 <- dados_zk %>%
  filter(SUBJ == "sub-SUBJ037") %>%
  mutate(FS = "T1")

dados_zk <- filter(dados_zk, SUBJ!="sub-SUBJ072" , SUBJ!="sub-SUBJ037") # As superficies geradas pelo FreeSurfer não estão boas o suficiente

# T2 ----

datat2 <-
  read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_zkt2.txt")

datat2 <- data %>%
  mutate(ROI = "hemisphere",
         )

data_lobest2 <-
  read_csv(
    "C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_zkt2_lobes.txt"
  )

data_lobest2 <- data_lobest2 %>%
  mutate(ROI = as.character(Lobe))

aseg_stats_ZKt2 <- read_table2("~/idor/Gyrification/data/resultados/aseg_vol_zkT2.txt") %>%
  mutate(SUBJ = `Measure:volume`) %>%
  dplyr::select(SUBJ, CSF, EstimatedTotalIntraCranialVol)

# ----
dados_zkt2 <- full_join(datat2, data_lobest2) %>%
  mutate(SUBJ = SubjectID) %>%
  left_join(aseg_stats_ZKt2) %>%
  dplyr::select(-c(SubjectID))
# passa as informacoes de area dos hemisferios para a coluna de area corrigida ----

# dados_zkt2 <- filter(dados_zk, SUBJ!="sub-SUBJ072") # As superficies geradas pelo FreeSurfer não estão boas o suficiente

dados_zk <- full_join(dados_zk, dados_zkt2)

dados_zk$Diagnostic <- as.factor(dados_zk$Diagnostic)

dados_zk <- dados_zk %>%
  mutate(Sample = ifelse(Diagnostic == "MICRO", "IDOR-MICRO", "IDOR-ZK"), machine = "Siemens Prisma 3T")

dados_zkt2 <- dados_zkt2  %>%
  mutate(FS = "T1T2")

dados_zk_t2_comp <- right_join(dados_zk_037, dados_zkt2)

dados_zk_t2_comp$ROI[dados_zk_t2_comp$ROI == "X1" | dados_zk_t2_comp$ROI == "1"] <- "F"
dados_zk_t2_comp$ROI[dados_zk_t2_comp$ROI == "X2" | dados_zk_t2_comp$ROI == "2"] <- "P"
dados_zk_t2_comp$ROI[dados_zk_t2_comp$ROI == "X3" | dados_zk_t2_comp$ROI == "3"] <- "T"
dados_zk_t2_comp$ROI[dados_zk_t2_comp$ROI == "X4" | dados_zk_t2_comp$ROI == "4"] <- "O"

# dados_zk_037_comp <- dados_zk_037_comp %>%
#   filter(ROI == "F" | ROI == "P"| ROI == "T"| ROI == "O" | ROI == "hemisphere")
