#ACC

tabela_sujeitos <-
  read_csv("~/idor/Gyrification/data/amostras/AMOSTRA_PRJ1513_DISGENESIACC.csv")

tabela_sujeitos <- tabela_sujeitos %>%
  mutate(Diagnostic = ifelse(
    str_sub(SUBJ, start = 1, end = 3) == "CTL",
    "CTL",
    ifelse(str_sub(SUBJ, start = 1, end = 3) == "PAC",
           "AgCC",
           "")
  ))

data <-
  read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_acalosos.txt")
data <- data %>% mutate(ROI = "hemisphere")

data_lobes <-
  read_csv(
    "C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_acalosos_lobes.txt"
  )
data_lobes <- data_lobes %>% mutate(ROI = as.character(Lobe))

aseg_stats <- read_table("~/idor/Gyrification/data/resultados/aseg_vol_DCC.txt") %>%
  mutate(SUBJ = `Measure:volume`) %>%
  dplyr::select(SUBJ, CSF, EstimatedTotalIntraCranialVol)

                                     
# ORGANIZING ALL DATA AND INCLUDING VARIABLES ####

# ----
dados_ACC <-
  full_join(data, data_lobes) %>% mutate(SUBJ = SubjectID) %>% full_join(tabela_sujeitos) %>% left_join(aseg_stats)

dados_ACC <- dados_ACC %>%
  mutate(
    Sample = "IDOR-CCD"
#    , SUBJ = str_c("SUBJ", str_sub(SUBJ, start = 4, end = 6))
  )
