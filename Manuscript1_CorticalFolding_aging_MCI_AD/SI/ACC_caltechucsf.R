## CALTECH e UCSF

UCSF_CALTECH_IDOR_AgCCDatabase <- read_excel("~/idor/Gyrification/data/amostras/UCSF_CALTECH_IDOR_AgCCDatabase.xlsx", 
                                             col_types = c("text", "skip", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "skip", "skip", 
                                                           "skip"), skip = 1)

tabela_sujeitos_caltechucsf <- UCSF_CALTECH_IDOR_AgCCDatabase %>%
  mutate(
    SUBJ = SUBJID_ALL,
    Age = as.double(AGE),
    Gender = GENDER,
    Diagnostic = GROUP,
    Sample = Institution) %>%
  filter(`3D T1` == "YES") %>% dplyr::select(-c(SUBJID_ALL, Institution, AGE, GROUP, GENDER, `3D T1`))

data_caltechucsf <-
  read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_acalosos_caltechucsf.txt")

data_caltechucsf <- data_caltechucsf %>% mutate(
  ROI = "hemisphere")

data_lobes_caltechucsf <-
  read_csv(
    "C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_acalosos_caltechucsf_lobes.txt"
  )

data_lobes_caltechucsf <- data_lobes_caltechucsf %>% mutate(ROI = as.character(Lobe))

aseg_stats <- read_table("~/idor/Gyrification/data/resultados/aseg_vol_DCC_UCSF_CALTECH.txt") %>%
  mutate(SUBJ = `Measure:volume`) %>%
  dplyr::select(SUBJ, CSF, EstimatedTotalIntraCranialVol)

# ORGANIZING ALL DATA AND INCLUDING VARIABLES ####

# ----
dados_caltechucsf <-
  full_join(data_caltechucsf, data_lobes_caltechucsf) %>% mutate(SUBJ = SubjectID) %>% 
  full_join(tabela_sujeitos_caltechucsf) %>%
  mutate(machine = "multiple") %>%
  filter(SUBJ != "SUBJ010") %>% left_join(aseg_stats)
