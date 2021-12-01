data <-
  read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_micro.txt")
data <- data %>% mutate(
  ROI = "hemisphere")

data_lobes <-
  read_csv(
    "C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_micro_lobes.txt"
  )

data_lobes <- data_lobes %>% mutate(ROI = as.character(Lobe))

aseg_stats <- read_table("~/idor/Gyrification/data/resultados/aseg_vol_microvera.txt") %>%
  mutate(SUBJ = `Measure:volume`) %>%
  dplyr::select(SUBJ, CSF, EstimatedTotalIntraCranialVol)

# Tabela dos sujeitos

#tabela_sujeitos <-   read_excel("C:/Users/ferna/Documents/idor/Gyrification/data/t1w fase 2 freesurfer.xlsx")

# tabela_sujeitos <- tabela_sujeitos %>%
#   mutate(
#     Age = idade / 12,
#     Gender = ifelse(sexo == 0, "FEM", "MASC")) %>% dplyr::select(-c(idade, sexo))

# ----
dados_micro <- full_join(data, data_lobes) %>% mutate(SUBJ = SubjectID) #%>% right_join(tabela_sujeitos)
# passa as informacoes de area dos hemisferios para a coluna de area corrigida ----

dados_micro <- dados_micro %>%
  mutate(
    Sample = "IDOR",
    machine = "multiple",
    Gender = "verificar",
    Diagnostic = as.factor("MICROVera"),
    Age = ifelse(
      SUBJ == "SUBJ052", 17,
      ifelse(SUBJ == "SUBJ664", 5,
             ifelse(SUBJ == "SUBJ666", 10,
                    22)
    ))
  ) %>% left_join(aseg_stats_ZK)
