
# IMPORTING HEMISPHERE DATA FROM YUJIANG/TOBIAS SCRIPT


dataAHEAD <- dataAHEAD %>% mutate(
  SUBJ = SubjectID,
  hemi = Hemisphere,
  ROI = "hemisphere",
  method = "Yujiang_script",
  Longitudinal_correction = "no",
  Sample = "AHEAD",
  Diagnostic = "CONTROLE",
  AvgThickness = AvgCortThickness,
  logAvgThickness = log10(AvgCortThickness),
  TotalArea = PialArea,
  logTotalArea = log10(PialArea),
  logTotalFullArea = log10(PialFullArea),
  ExposedArea = SmoothPialArea,
  logExposedArea = log10(SmoothPialArea),
  WhiteSurfArea = WhiteArea,
  logWhiteSurfArea = log10(WhiteArea),
  GMvolume = GreymatterVol,
  logConvexHullArea = log10(ConvexHullArea),
  localGI = TotalArea / ExposedArea,
  k = sqrt(AvgThickness)*TotalArea/(ExposedArea^1.25),
  K = 1 / 4*log10(AvgThickness^2) + logTotalArea - 5 / 4 * logExposedArea,
  I = logTotalArea + logExposedArea + logAvgThickness^2,
  S = 3/2 * logTotalArea + 3/4 * logExposedArea - 9/4*logAvgThickness^2
) %>% dplyr::select(-c(SubjectID, Hemisphere, AvgCortThickness, PialArea, SmoothPialArea, WhiteArea, GreymatterVol))


dataAHEAD$hemi[dataAHEAD$hemi == "left"] <- "L"
dataAHEAD$hemi[dataAHEAD$hemi == "right"] <- "R"