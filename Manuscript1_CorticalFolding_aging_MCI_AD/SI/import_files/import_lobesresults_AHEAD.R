# importa os arquivos----


dataAHEADlobes <- unique(dataAHEADlobes)

# RENOMEIA ID PARA SUBJ_ses ----
names(dataAHEADlobes)[1] <- "SUBJ"

# NOMEIA AS ROIS ----


dataAHEADlobes$Lobe[dataAHEADlobes$Lobe == 1] <-
  "F"
dataAHEADlobes$Lobe[dataAHEADlobes$Lobe == 2] <-
  "P"
dataAHEADlobes$Lobe[dataAHEADlobes$Lobe == 3] <-
  "T"
dataAHEADlobes$Lobe[dataAHEADlobes$Lobe == 4] <-
  "O"

colnames(dataAHEADlobes)[which(names(dataAHEADlobes) == "Hemisphere")]  <- "hemi"

dataAHEADlobes$hemi[dataAHEADlobes$hemi == "left"] <- "L"
dataAHEADlobes$hemi[dataAHEADlobes$hemi == "right"] <- "R"

colnames(dataAHEADlobes)[which(names(dataAHEADlobes) == "Lobe")]  <- "ROI"

dataAHEADlobes$ROI <-
  as.factor(dataAHEADlobes$ROI)

# ----
colnames(dataAHEADlobes)[which(names(dataAHEADlobes) == "SmoothPialArea")] <-
  "ExposedArea"
colnames(dataAHEADlobes)[which(names(dataAHEADlobes) == "PialArea")] <-
  "TotalArea"
colnames(dataAHEADlobes)[which(names(dataAHEADlobes) == "AvgCortThickness")] <-
  "AvgThickness"

# verificacao dos sujeitos com ExposedArea == 0 -----
verificar_SmoothArea_zero <- filter(dataAHEADlobes, ExposedArea == 0)
write.csv(verificar_SmoothArea_zero, "verificar_SmoothArea_zero.csv")

# limpeza - tira os sujeitos com ExposedArea == 0 ---
dataAHEADlobes <- filter(dataAHEADlobes, ExposedArea != 0)

# ----


dataAHEADlobes <- dataAHEADlobes %>% mutate(
,
  localGI = TotalArea / ExposedArea,
  logAvgThickness = log10(AvgThickness),
  logTotalArea = log10(TotalArea),
  logExposedArea = log10(ExposedArea),
  k = sqrt(AvgThickness)*TotalArea/(ExposedArea^1.25),
  K = 1 / 4 * logAvgThickness^2 + logTotalArea - 5 / 4 * logExposedArea,
  c = 4 * pi / GaussianCurvature,
  WhiteSurfArea = WhiteArea,
  logWhiteSurfArea = log10(WhiteArea),
  GMvolume = GreymatterVol,
  TotalArea_corrected = TotalArea * c,
  ExposedArea_corrected = ExposedArea * c,
  localGI_corrected = TotalArea_corrected / ExposedArea_corrected,
  logTotalArea_corrected = log10(TotalArea_corrected),
  logExposedArea_corrected = log10(ExposedArea_corrected),
  k_corrected = sqrt(AvgThickness)*TotalArea_corrected/(ExposedArea_corrected^1.25),
  K_corrected = 1 / 4 * logAvgThickness^2 + logTotalArea_corrected - 5 / 4 * logExposedArea_corrected,
  I_corrected = logTotalArea_corrected + logExposedArea_corrected + logAvgThickness^2,
  S_corrected = 3/2 * logTotalArea_corrected + 3/4 * logExposedArea_corrected - 9/4*logAvgThickness^2
) %>% filter(ROI != "0", ROI != "5")

