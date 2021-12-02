
# dados_datasetscomp <- dados_datasetscomp %>% mutate(
#   hemi = Hemisphere,
#   AvgThickness = AvgCortThickness,
#   TotalArea = PialArea,
#   ExposedArea = SmoothPialArea)

dados_datasetscomp <- dados_datasetscomp %>%
  #full_join(datasets_yujiang) %>%
  mutate(
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
)

dados_datasetscomp$hemi[dados_datasetscomp$hemi == "left"] <- "L"
dados_datasetscomp$hemi[dados_datasetscomp$hemi == "right"] <- "R"

dados_datasetscomp$ROI[dados_datasetscomp$ROI == "X1" | dados_datasetscomp$ROI == "1"] <- "F"
dados_datasetscomp$ROI[dados_datasetscomp$ROI == "X2" | dados_datasetscomp$ROI == "2"] <- "P"
dados_datasetscomp$ROI[dados_datasetscomp$ROI == "X3" | dados_datasetscomp$ROI == "3"] <- "T"
dados_datasetscomp$ROI[dados_datasetscomp$ROI == "X4" | dados_datasetscomp$ROI == "4"] <- "O"
# dados_datasetscomp$nROI[dados_datasetscomp$nROI == "X5"] <- "Insula"

dados_datasetscomp <- dados_datasetscomp %>%
  filter(ROI == "F" | ROI == "P"| ROI == "T"| ROI == "O" | ROI == "hemisphere")

### AGE INTERVAL ####

dados_datasetscomp$Age_interval <- cut(dados_datasetscomp$Age,
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100),
                                       right = FALSE,
                                       include.lowest = TRUE)

dados_datasetscomp$Age_interval10 <- cut(dados_datasetscomp$Age,
                                       breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                                       right = FALSE,
                                       include.lowest = TRUE)
