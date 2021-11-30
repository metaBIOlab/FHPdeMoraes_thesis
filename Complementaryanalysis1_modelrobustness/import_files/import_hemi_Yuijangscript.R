
# IMPORTING HEMISPHERE DATA FROM YUJIANG/TOBIAS SCRIPT

data_longcor_ses1 <- read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_longcor_ses1.txt")

data_longcor_ses1 <- data_longcor_ses1 %>% mutate(
  SUBJ = SubjectID,
  hemi = Hemisphere,
  ROI = "hemisphere",
  method = "CorticalFoldingTool",
  Longitudinal_correction = "yes",
  EEdiameter = 15,
  AvgThickness = AvgCortThickness,
  logAvgThickness = log10(AvgCortThickness),
  TotalArea = PialArea,
  logTotalArea = log10(PialArea),
  TotalFullArea = PialFullArea,
  logTotalFullArea = log10(PialFullArea),
  ExposedArea = SmoothPialArea,
  logExposedArea = log10(SmoothPialArea),
  logConvexHullArea = log10(ConvexHullArea),
  WhiteSurfArea = WhiteArea,
  logWhiteSurfArea = log10(WhiteArea),
  GMvolume = GreymatterVol,
  logGMvolume = log10(GreymatterVol),
  logConvexHullArea = log10(ConvexHullArea),
  localGI = TotalArea / ExposedArea,
  K = 1 / 2 * logAvgThickness + logTotalArea - 5 / 4 * logExposedArea,
  I = logTotalArea + logExposedArea + logAvgThickness ^ 2,
  S = 3 / 2 * logTotalArea + 3 / 4 * logExposedArea - 9 / 4 * logAvgThickness ^ 2
) %>% dplyr::select(
  -c(
    SubjectID,
    Hemisphere,
    AvgCortThickness,
    PialArea,
    SmoothPialArea,
    WhiteArea,
    GreymatterVol
  )
)

data_longcor_ses1$hemi[data_longcor_ses1$hemi == "left"] <- "L"
data_longcor_ses1$hemi[data_longcor_ses1$hemi == "right"] <- "R"
