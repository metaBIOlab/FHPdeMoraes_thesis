source("import_files/import_subjects.R") # arquivos dos sujeitos

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

dados <-
  inner_join(tabela_sujeitos, data) %>%
  filter(
    Diagnostic == "CONTROLE" |
      Diagnostic == "CCL" |
      Diagnostic == "ALZ",
    !is.na(Age),
    SUBJ != "SUBJ211",
    SUBJ != "SUBJ223", SUBJ != "SUBJ231", SUBJ != "SUBJ136", SUBJ != "SUBJ014", SUBJ != "SUBJ128", SUBJ != "SUBJ157"
  ) %>%
  droplevels() %>% mutate(
  hemi = Hemisphere,
  AvgThickness = AvgCortThickness,
  TotalArea = PialArea,
  ExposedArea = SmoothPialArea,
  WhiteSurfArea = WhiteArea,
  GMvolume = GreymatterVol,
  logAvgThickness = log10(AvgThickness),
  logTotalArea = log10(TotalArea),
  logExposedArea = log10(ExposedArea),
  localGI = TotalArea / ExposedArea,
  k = sqrt(AvgThickness) * TotalArea / (ExposedArea ^ 1.25),
  K = 1 / 4 * log10(AvgThickness^2)  + log10(TotalArea) - 5 / 4 * log10(ExposedArea),
  S = 3 / 2 * log10(TotalArea) + 3 / 4 * log10(ExposedArea) - 9 /  4 * log10(AvgThickness^2) ,
  I = log10(TotalArea) + log10(ExposedArea) + log10(AvgThickness^2) ,
  Knorm = K/sqrt(1 + (1/4)^2 + (5/4)^2),
  Snorm = S/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
  Inorm = I/sqrt(1^2 + 1^2 + 1^2),
  c = as.double(ifelse(ROI == "hemisphere", NA, 4 * pi / GaussianCurvature)),
  WhiteSurfArea = WhiteArea,
  GMvolume = GreymatterVol,
  TotalArea_corrected = ifelse(ROI == "hemisphere", TotalArea, TotalArea * c),
  ExposedArea_corrected = ifelse(ROI == "hemisphere", ExposedArea, ExposedArea * c),
  logTotalArea_corrected = log10(TotalArea_corrected),
  logExposedArea_corrected = log10(ExposedArea_corrected),
  localGI_corrected = ifelse(
    ROI == "hemisphere",
    TotalArea / ExposedArea,
    TotalArea_corrected / ExposedArea_corrected
  ),
  k_corrected = ifelse(
    ROI == "hemisphere",
    sqrt(AvgThickness) * log10(TotalArea) / (log10(ExposedArea) ^ 1.25),
    sqrt(AvgThickness) * log10(TotalArea_corrected) / (log10(ExposedArea_corrected^1.25) )
  ),
  K_corrected =  ifelse(
    ROI == "hemisphere",
    1 / 4 * log10(AvgThickness^ 2)+ log10(TotalArea) - 5 / 4 * log10(ExposedArea),
    1 / 4 * log10(AvgThickness^ 2) + log10(TotalArea_corrected) - 5 / 4 * log10(ExposedArea_corrected)
  ),
  I_corrected = ifelse(
    ROI == "hemisphere",
    log10(TotalArea) + log10(ExposedArea) + log10(AvgThickness^ 2) ,
    log10(TotalArea_corrected) + log10(ExposedArea_corrected) + log10(AvgThickness^ 2) 
  ),
  S_corrected = ifelse(
    ROI == "hemisphere",
    3 / 2 * log10(TotalArea) + 3 / 4 * log10(ExposedArea) - 9 /  4 * log10(AvgThickness^ 2) ,
    3 / 2 * log10(TotalArea_corrected) + 3 / 4 * log10(ExposedArea_corrected) - 9 /  4 * log10(AvgThickness^ 2) 
  )
) %>% filter(ROI != "0", ROI != "5")

dados$hemi[dados$hemi == "left"] <- "L"
dados$hemi[dados$hemi == "right"] <- "R"

dados$ROI[dados$ROI == 1] <-
  "F"
dados$ROI[dados$ROI == 2] <-
  "P"
dados$ROI[dados$ROI == 3] <-
  "T"
dados$ROI[dados$ROI == 4] <-
  "O"
# dados$nROI[dados$nROI == 5] <- "Insula"

### AGE INTERVAL ####

dados$Age_interval[dados$Age >= 0 & dados$Age < 5] <- "00-05"
dados$Age_interval[dados$Age >= 5 & dados$Age < 10] <- "05-10"
dados$Age_interval[dados$Age >= 10 & dados$Age < 15] <- "10-15"
dados$Age_interval[dados$Age >= 15 & dados$Age < 20] <- "15-20"
dados$Age_interval[dados$Age >= 20 & dados$Age < 25] <- "20-25"
dados$Age_interval[dados$Age >= 25 & dados$Age < 30] <- "25-30"
dados$Age_interval[dados$Age >= 30 & dados$Age < 35] <- "30-35"
dados$Age_interval[dados$Age >= 35 & dados$Age < 40] <- "35-40"
dados$Age_interval[dados$Age >= 40 & dados$Age < 45] <- "40-45"
dados$Age_interval[dados$Age >= 45 & dados$Age < 50] <- "45-50"
dados$Age_interval[dados$Age >= 50 & dados$Age < 55] <- "50-55"
dados$Age_interval[dados$Age >= 55 & dados$Age < 60] <- "55-60"
dados$Age_interval[dados$Age >= 60 & dados$Age < 65] <- "60-65"
dados$Age_interval[dados$Age >= 65 & dados$Age < 70] <- "65-75"
dados$Age_interval[dados$Age >= 70 & dados$Age < 75] <- "70-75"
dados$Age_interval[dados$Age >= 75 & dados$Age < 80] <- "75-80"
dados$Age_interval[dados$Age >= 80 & dados$Age < 85] <- "80-85"
dados$Age_interval[dados$Age >= 85 & dados$Age < 90] <- "85-90"
dados$Age_interval[dados$Age >= 90 & dados$Age < 95] <- "90-95"
dados$Age_interval[dados$Age >= 95 & dados$Age < 100] <- "95-100"
dados$Age_interval[dados$Age == "95"] <- "91"

dados$Age_interval10[dados$Age >= 00 & dados$Age < 10] <- "00"
dados$Age_interval10[dados$Age >= 10 & dados$Age < 20] <- "10"
dados$Age_interval10[dados$Age >= 20 & dados$Age < 30] <- "20"
dados$Age_interval10[dados$Age >= 30 & dados$Age < 40] <- "30"
dados$Age_interval10[dados$Age >= 40 & dados$Age < 50] <- "40"
dados$Age_interval10[dados$Age >= 50 & dados$Age < 60] <- "50"
dados$Age_interval10[dados$Age >= 60 & dados$Age < 70] <- "60"
dados$Age_interval10[dados$Age >= 70 & dados$Age < 80] <- "70"
dados$Age_interval10[dados$Age >= 80 &  dados$Age < 90] <-"80"
dados$Age_interval10[dados$Age >= 90 & dados$Age < 100] <- "90"
