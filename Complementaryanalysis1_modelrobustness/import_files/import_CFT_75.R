
# IMPORTING HEMISPHERE DATA FROM YUJIANG/TOBIAS SCRIPT

data_longcor_ses1_75 <- read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_longcor_ses1_75mm.txt")

data_longcor_ses1_75 <- data_longcor_ses1_75 %>% mutate(
  SUBJ = SubjectID,
  hemi = Hemisphere,
  ROI = "hemisphere",
  method = "CorticalFoldingTool",
  Longitudinal_correction = "yes",
  EEdiameter = 75,
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

data_longcor_ses1_75$hemi[data_longcor_ses1_75$hemi == "left"] <- "L"
data_longcor_ses1_75$hemi[data_longcor_ses1_75$hemi == "right"] <- "R"

# LOBOS----

# data_longcor_ses1_lobes_75 <- read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_longcor_ses1_lobes_75mm.txt")
# 
# data_longcor_ses1_lobes_75 <- data_longcor_ses1_lobes_75 %>% mutate(
#   SUBJ = SubjectID,
#   hemi = Hemisphere,
#   ROI = Lobe,
#   method = "CorticalFoldingTool",
#   EEdiameter = 75,
#   AvgThickness = AvgCortThickness,
#   logAvgThickness = log10(AvgCortThickness),
#   TotalArea = PialArea,
#   logTotalArea = log10(PialArea),
#   ExposedArea = SmoothPialArea,
#   logExposedArea = log10(SmoothPialArea),
#   WhiteSurfArea = WhiteArea,
#   logWhiteSurfArea = log10(WhiteArea),
#   GMvolume = GreymatterVol,
#   logGMvolume = log10(GreymatterVol),
#   localGI = TotalArea / ExposedArea,
#   K = 1 / 2 * logAvgThickness + logTotalArea - 5 / 4 * logExposedArea,
#   I = logTotalArea + logExposedArea + logAvgThickness^2,
#   S = 3/2 * logTotalArea + 3/4 * logExposedArea - 9/4*logAvgThickness^2,
#   c = 4 * pi / GaussianCurvature,
#   TotalArea_corrected = TotalArea * c,
#   ExposedArea_corrected = ExposedArea * c,
#   localGI_corrected = TotalArea_corrected / ExposedArea_corrected,
#   logTotalArea_corrected = log10(TotalArea_corrected),
#   logExposedArea_corrected = log10(ExposedArea_corrected),
#   K_corrected = 1 / 2 * logAvgThickness + logTotalArea_corrected - 5 / 4 * logExposedArea_corrected,
#   I_corrected = logTotalArea_corrected + logExposedArea_corrected + logAvgThickness^2,
#   S_corrected = 3/2 * logTotalArea_corrected + 3/4 * logExposedArea_corrected - 9/4*logAvgThickness^2
# ) %>% dplyr::select(-c(SubjectID, Hemisphere, Lobe, AvgCortThickness, PialArea, SmoothPialArea, WhiteArea, GreymatterVol)) %>% filter(ROI != "0" & ROI != "5")
# 
# data_longcor_ses1_lobes_75$hemi[data_longcor_ses1_lobes_75$hemi == "left"] <- "L"
# data_longcor_ses1_lobes_75$hemi[data_longcor_ses1_lobes_75$hemi == "right"] <- "R"
# 
# data_longcor_ses1_lobes_75$ROI[data_longcor_ses1_lobes_75$ROI == 1] <-
#   "F"
# data_longcor_ses1_lobes_75$ROI[data_longcor_ses1_lobes_75$ROI == 2] <-
#   "P"
# data_longcor_ses1_lobes_75$ROI[data_longcor_ses1_lobes_75$ROI == 3] <-
#   "T"
# data_longcor_ses1_lobes_75$ROI[data_longcor_ses1_lobes_75$ROI == 4] <-
#   "O"
# # data_longcor_ses1_lobes_75$nROI[data_longcor_ses1_lobes_75$nROI == 5] <- "Insula"
# 
# data_longcor_ses1_lobes_75$ROI <- as.factor(data_longcor_ses1_lobes_75$ROI)
