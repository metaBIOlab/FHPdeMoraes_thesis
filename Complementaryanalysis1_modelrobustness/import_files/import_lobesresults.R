# importa os arquivos----

data_longcor_ses1_lobes <- read_csv("C:/Users/ferna/Documents/idor/Gyrification/data/resultados/data_longcor_ses1_lobes.txt")

data_lobes <- data_longcor_ses1_lobes

data_lobes <- data_lobes %>% mutate(
  SUBJ = SubjectID,
  hemi = Hemisphere,
  ROI = Lobe,
  method = "CorticalFoldingTool",
  EEdiameter = 15,
  AvgThickness = AvgCortThickness,
  logAvgThickness = log10(AvgCortThickness),
  TotalArea = PialArea,
  logTotalArea = log10(PialArea),
  ExposedArea = SmoothPialArea,
  logExposedArea = log10(SmoothPialArea),
  WhiteSurfArea = WhiteArea,
  logWhiteSurfArea = log10(WhiteArea),
  GMvolume = GreymatterVol,
  logGMvolume = log10(GreymatterVol),
  localGI = TotalArea / ExposedArea,
  K = 1 / 2 * logAvgThickness + logTotalArea - 5 / 4 * logExposedArea,
  I = logTotalArea + logExposedArea + logAvgThickness^2,
  S = 3/2 * logTotalArea + 3/4 * logExposedArea - 9/4*logAvgThickness^2,
  c = 4 * pi / GaussianCurvature,
  TotalArea_corrected = TotalArea * c,
  ExposedArea_corrected = ExposedArea * c,
  localGI_corrected = TotalArea_corrected / ExposedArea_corrected,
  logTotalArea_corrected = log10(TotalArea_corrected),
  logExposedArea_corrected = log10(ExposedArea_corrected),
  K_corrected = 1 / 2 * logAvgThickness + logTotalArea_corrected - 5 / 4 * logExposedArea_corrected,
  I_corrected = logTotalArea_corrected + logExposedArea_corrected + logAvgThickness^2,
  S_corrected = 3/2 * logTotalArea_corrected + 3/4 * logExposedArea_corrected - 9/4*logAvgThickness^2
) %>% dplyr::select(-c(SubjectID, Hemisphere, Lobe, AvgCortThickness, PialArea, SmoothPialArea, WhiteArea, GreymatterVol)) %>% filter(ROI != "0" & ROI != "5")

data_lobes$hemi[data_lobes$hemi == "left"] <- "L"
data_lobes$hemi[data_lobes$hemi == "right"] <- "R"

data_lobes$ROI[data_lobes$ROI == 1] <-
  "F"
data_lobes$ROI[data_lobes$ROI == 2] <-
  "P"
data_lobes$ROI[data_lobes$ROI == 3] <-
  "T"
data_lobes$ROI[data_lobes$ROI == 4] <-
  "O"
# data_lobes$nROI[data_lobes$nROI == 5] <- "Insula"

data_lobes$ROI <- as.factor(data_lobes$ROI)
