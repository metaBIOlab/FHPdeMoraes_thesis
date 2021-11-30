#AHEAD DESCRIPTION

AHEAD <- read_csv(
  "~/idor/Gyrification/AHEAD/participants.csv",
  col_types = cols(
    X4 = col_skip(),
    X5 = col_skip(),
    X6 = col_skip(),
    X7 = col_skip()
  )
)
colnames(AHEAD)[which(names(AHEAD) == "ScanName")] <-
  "SUBJ"
colnames(AHEAD)[which(names(AHEAD) == "Group")] <-
  "age"

source("import_files/import_hemi_AHEAD.R")
source("import_files/import_lobesresults_AHEAD.R")

#AHEAD$age <- as.double(AHEAD$age)

summary(AHEAD)

AHEAD <- filter(AHEAD,!is.na(age))
AHEAD %>% group_by(Sex) %>% summarise(n = n_distinct(SUBJ))

dados_AHEAD <- full_join(dataAHEAD, dataAHEADlobes)
dados_AHEAD <- right_join(AHEAD, dados_AHEAD)

dados_AHEAD <-
  mutate(dados_AHEAD, Session = "1", Diagnostic = "CONTROLE")

# colnames(dataAHEADlobes)[which(names(dataAHEADlobes) == "age")] <- "Age"
#colnames(dataAHEADlobes)[which(names(dataAHEADlobes) == "Sex")] <-
  "Gender"


dados_AHEAD$TotalArea_corrected[dados_AHEAD$ROI == "hemisphere"] <-
  dados_AHEAD$TotalArea[dados_AHEAD$ROI == "hemisphere"]
dados_AHEAD$ExposedArea_corrected[dados_AHEAD$ROI == "hemisphere"] <-
  dados_AHEAD$ExposedArea[dados_AHEAD$ROI == "hemisphere"]
dados_AHEAD$K_corrected[dados_AHEAD$ROI == "hemisphere"] <-
  dados_AHEAD$K[dados_AHEAD$ROI == "hemisphere"]
dados_AHEAD$localGI_corrected[dados_AHEAD$ROI == "hemisphere"] <-
  dados_AHEAD$localGI[dados_AHEAD$ROI == "hemisphere"]
dados_AHEAD$logTotalArea_corrected[dados_AHEAD$ROI == "hemisphere"] <-
  dados_AHEAD$logTotalArea[dados_AHEAD$ROI == "hemisphere"]
dados_AHEAD$logExposedArea_corrected[dados_AHEAD$ROI == "hemisphere"] <-
  dados_AHEAD$logExposedArea[dados_AHEAD$ROI == "hemisphere"]
dados_AHEAD$S_corrected[dados_AHEAD$ROI == "hemisphere"] <-
  dados_AHEAD$S[dados_AHEAD$ROI == "hemisphere"]
dados_AHEAD$I_corrected[dados_AHEAD$ROI == "hemisphere"] <-
  dados_AHEAD$I[dados_AHEAD$ROI == "hemisphere"]

dados_AHEAD <-
  dados_AHEAD %>% filter(!is.infinite(logTotalArea_corrected)) %>% mutate(machine = "7T")

dados_AHEAD <-
  dplyr::select(
    dados_AHEAD,-c(
      ConvexHullArea,
      PialFullArea ,
      WhiteFullArea ,
      SmoothPialFullArea ,
      ConvexHullFullArea ,
      PialFullVol ,
      WhiteFullVol ,
      SmoothPialFullVol ,
      logTotalFullArea ,
      logConvexHullArea ,
      WhiteArea ,
      GaussianCurvature ,
      PialVol ,
      WhiteVol ,
      GreymatterVol ,
      c
    )
  )

# colnames(dados_AHEAD)[which(names(dados_AHEAD) == "age")] <- "Age"
colnames(dados_AHEAD)[which(names(dados_AHEAD) == "Sex")] <-
  "Gender"