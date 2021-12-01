#HCPINFANT
particpants <- read_delim("~/idor/Gyrification/HCPINFANT/particpants.csv", 
                          ";", escape_double = FALSE, col_types = cols(SUBJ = col_skip(),
                                                                       MR_Sessions = col_skip(), 
                                                                       `Age at scan in weeks` = col_skip(), 
                                                                       `Age at scan in months` = col_skip()), 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), 
                          trim_ws = TRUE)

data <-
  read_csv("C:/Users/ferna/Documents/idor/Gyrification/HCPINFANT/dataHCP.txt")
data <- data %>% mutate(ROI = "hemisphere")

data_lobes <-
  read_csv(
    "C:/Users/ferna/Documents/idor/Gyrification/HCPINFANT/dataHCPlobes.txt"
  )
data_lobes <- data_lobes  %>% mutate(ROI = as.character(Lobe))

# ORGANIZING ALL DATA AND INCLUDING VARIABLES ####

# ----
dados_HCP <-
  full_join(data, data_lobes) %>% mutate(SUBJ = SubjectID) %>% full_join(particpants)

dados_HCP <- dados_HCP %>%
  mutate(Diagnostic = "CTL",
    Sample = "dHCP-Control",
    Age_at_scan_week = Gestational_age_at_scan-Gestational_age_at_birth,
    Age = (Age_at_scan_week/4)/12) %>% filter(SmoothPialFullVol != 0, !is.na(SmoothPialFullVol))
