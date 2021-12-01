recon_all_finished_without <- read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/BHRCS/recon-all_finished_without.csv", 
                                       col_names = FALSE)
colnames(recon_all_finished_without)[1] <- "SubjectID"

eulernumber_renumbered <- read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/BHRCS/eulernumber_renumbered.csv", 
                                   col_names = FALSE, col_types = cols(X4 = col_skip(), 
                                                                       X5 = col_skip(), X6 = col_skip(), 
                                                                       X7 = col_skip()))
colnames(eulernumber_renumbered)[1] <- "SubjectID"
colnames(eulernumber_renumbered)[2] <- "lhEuler"
colnames(eulernumber_renumbered)[3] <- "rhEuler"

eulernumber_renumbered <- eulernumber_renumbered %>%
  mutate(
    meanEulernumber = (lhEuler+rhEuler)/2
  )

eulernumber_renumbered_lh <- eulernumber_renumbered %>%
  dplyr::select(SubjectID, lhEuler, meanEulernumber) %>%
  mutate(hemi = "left")
colnames(eulernumber_renumbered_lh)[2] <- "Eulernumber"
eulernumber_renumbered_rh <- eulernumber_renumbered %>%
  dplyr::select(SubjectID, rhEuler, meanEulernumber) %>%
  mutate(hemi = "right")
colnames(eulernumber_renumbered_rh)[2] <- "Eulernumber"
eulernumber_renumbered <- rbind(eulernumber_renumbered_lh, eulernumber_renumbered_rh)

data_BHRCS <-
  read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/BHRCS/corticalfolding_hemisphere.csv")

data_BHRCS <- data_BHRCS %>% mutate(ROI = "hemisphere")

data_lobes_BHRCS <-
  read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/BHRCS/corticalfolding_lobes.csv")

data_lobes_BHRCS <- data_lobes_BHRCS %>% mutate(ROI = as.character(Lobe))

# aseg_stats <- read_table2("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/aseg_vol_microvera.txt") %>%
#   mutate(SUBJ = `Measure:volume`) %>%
#   dplyr::select(SUBJ, CSF, EstimatedTotalIntraCranialVol)

# Tabela dos sujeitos

subject_info <-
  readRDS("C:/Users/ferna/Documents/idor/Gyrification/data/BHRCS/fernanda_data.rds") %>%
  mutate(
    SUBJ = ifelse(
      str_length(ident) == 1,
      str_c("sub-0000", ident),
      ifelse(
        str_length(ident) == 2,
        str_c("sub-000", ident),
        ifelse(
          str_length(ident) == 3,
          str_c("sub-00", ident),
          ifelse(
            str_length(ident) == 4,
            str_c("sub-0", ident),
            str_c("sub-", ident)
          )
        )
      )
    ),
    Session = as.factor(ifelse(wave == "0", 1, ifelse(wave == "1", 2, 3))),
    Age = as.double(age),
    Gender = ifelse( gender == 1, "MASC", "FEM"),
    Sample = ifelse(str_sub(subjectid, 0, 2) == "10", "BHRCS-SP", "BHRCS-POA"),
    machine = ifelse(str_sub(subjectid, 0, 2) == "10", "GE HD 1.5T", "GE HDx 1.5T")
  ) %>%
  dplyr::select(-c(ident, subjectid, wave, age, gender))

# falta confirmar o código de gênero
# ----
dados_BHRCS <- full_join(data_BHRCS, data_lobes_BHRCS) %>%
  semi_join(recon_all_finished_without) %>%
  full_join(eulernumber_renumbered) %>%
  mutate(SUBJ = str_sub(SubjectID,0,9),
         Session = as.factor(str_sub(SubjectID,15,15)),
         Run = str_sub(SubjectID,21,21)) %>%
  filter(!is.na(Hemisphere)) %>%
  left_join(subject_info)

# passa as informacoes de area dos hemisferios para a coluna de area corrigida ----

dados_BHRCS <- dados_BHRCS %>%
  mutate(
    Diagnostic = "CTL",
    FieldStrenght = 1.5
  ) %>%
  dplyr::select(-c(SubjectID)) 

# anti_join(dados_BHRCS, dados_BHRCS_uniquerun_eulernumber) %>%
  # dplyr::select(c(SUBJ, Eulernumber, Session, Run))

# %>%
#   left_join(aseg_stats_ZK)
