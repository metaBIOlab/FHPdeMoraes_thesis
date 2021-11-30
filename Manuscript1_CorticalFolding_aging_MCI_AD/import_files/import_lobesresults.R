# importa os arquivos----

data_longcor_ses1_lobes <- read_csv("~/idor/Gyrification/data/resultados/data_longcor_ses1_lobes.txt")
data_longcor_ses2_lobes <- read_csv("~/idor/Gyrification/data/resultados/data_longcor_ses2_lobes.txt")
data_longcor_ses3_lobes <- read_csv("~/idor/Gyrification/data/resultados/data_longcor_ses3_lobes.txt")

data_longcor_ses1_lobes <- data_longcor_ses1_lobes %>% mutate(Session = "1")

data_longcor_ses2_lobes <- data_longcor_ses2_lobes %>% mutate(Session = "2")

data_longcor_ses3_lobes <- data_longcor_ses3_lobes %>% mutate(Session = "3")

data_lobes <- full_join(data_longcor_ses1_lobes, data_longcor_ses2_lobes)
data_lobes <- data_lobes %>% full_join(data_longcor_ses3_lobes) 

data_lobes <- data_lobes %>% mutate(
  SUBJ = SubjectID,
  hemi = Hemisphere,
  ROI = Lobe,
  AvgThickness = AvgCortThickness,
  logAvgThickness = log10(AvgCortThickness),
  TotalArea = PialArea,
  logTotalArea = log10(PialArea),
  ExposedArea = SmoothPialArea,
  logExposedArea = log10(SmoothPialArea),
  WhiteSurfArea = WhiteArea,
  logWhiteSurfArea = log10(WhiteArea),
  GMvolume = GreymatterVol,
  localGI = TotalArea / ExposedArea,
  k = sqrt(AvgThickness) * TotalArea / (ExposedArea ^ 1.25),
  K = 1 / 4 * log10(AvgThickness^2)  + log10(TotalArea) - 5 / 4 * log10(ExposedArea),
  S = 3 / 2 * log10(TotalArea) + 3 / 4 * log10(ExposedArea) - 9 /  4 * log10(AvgThickness^2) ,
  I = log10(TotalArea) + log10(ExposedArea) + log10(AvgThickness^2) ,
  Knorm = K/sqrt(1 + (1/4)^2 + (5/4)^2),
  Snorm = S/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
  Inorm = I/sqrt(1^2 + 1^2 + 1^2),
  c = 4 * pi / GaussianCurvature,
  TotalArea_corrected = TotalArea * c,
  ExposedArea_corrected = ExposedArea * c,
  localGI_corrected = TotalArea_corrected / ExposedArea_corrected,
  logTotalArea_corrected = log10(TotalArea_corrected),
  logExposedArea_corrected = log10(ExposedArea_corrected),
  k_corrected = sqrt(AvgThickness) * TotalArea_corrected / (ExposedArea_corrected ^ 1.25),
  K_corrected = 1 / 4 * log10(AvgThickness^ 2) + log10(TotalArea_corrected) - 5 / 4 * log10(ExposedArea_corrected),
  I_corrected = log10(TotalArea_corrected) + log10(ExposedArea_corrected) + log10(AvgThickness^ 2) ,
  S_corrected = 3 / 2 * log10(TotalArea_corrected) + 3 / 4 * log10(ExposedArea_corrected) - 9 /  4 * log10(AvgThickness^ 2) 

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

# verificacao dos sujeitos com ExposedArea == 0 -----
verificar_SmoothArea_zero <- filter(data_lobes, ExposedArea == 0)
write.csv(verificar_SmoothArea_zero, "verificar_SmoothArea_zero.csv")


data_lobes$Session <- as.double(data_lobes$Session)
