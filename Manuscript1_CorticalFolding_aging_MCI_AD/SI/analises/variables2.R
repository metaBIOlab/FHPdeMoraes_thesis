
# Wang <- Wang %>% mutate(
#   hemi = Hemisphere,
#   AvgThickness = AvgCortThickness,
#   TotalArea = PialArea,
#   ExposedArea = SmoothPialArea)

Wang <- Wang %>%
  #full_join(datasets_yujiang) %>%
  mutate(
    WhiteSurfArea = WhiteArea,
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
    TotalArea_corrected = ifelse(ROI == "hemisphere", TotalArea, TotalArea*c),
    ExposedArea_corrected = ifelse(ROI == "hemisphere", ExposedArea, ExposedArea*c),
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
Wang$hemi[Wang$hemi == "left"] <- "L"
Wang$hemi[Wang$hemi == "right"] <- "R"

Wang$ROI[Wang$ROI == "X2" | Wang$ROI == "2"] <- "F"
Wang$ROI[Wang$ROI == "X3" | Wang$ROI == "3"] <- "P"
Wang$ROI[Wang$ROI == "X4" | Wang$ROI == "4"] <- "T"
Wang$ROI[Wang$ROI == "X5" | Wang$ROI == "5"] <- "O"
# Wang$nROI[Wang$nROI == "X5"] <- "Insula"

Wang <- Wang %>%
  filter(ROI == "F" | ROI == "P"| ROI == "T"| ROI == "O" | ROI == "hemisphere")


### AGE INTERVAL ####

Wang$Age_interval <- cut(Wang$Age,
                         breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100),
                         right = FALSE,
                         include.lowest = TRUE)

# Wang$Age_interval[Wang$Age >= 0 & Wang$Age < 5] <- "00-05"
# Wang$Age_interval[Wang$Age >= 5 & Wang$Age < 10] <- "05-10"
# Wang$Age_interval[Wang$Age >= 10 & Wang$Age < 15] <- "10-15"
# Wang$Age_interval[Wang$Age >= 15 & Wang$Age < 20] <- "15-20"
# Wang$Age_interval[Wang$Age >= 20 & Wang$Age < 25] <- "20-25"
# Wang$Age_interval[Wang$Age >= 25 & Wang$Age < 30] <- "25-30"
# Wang$Age_interval[Wang$Age >= 30 & Wang$Age < 35] <- "30-35"
# Wang$Age_interval[Wang$Age >= 35 & Wang$Age < 40] <- "35-40"
# Wang$Age_interval[Wang$Age >= 40 & Wang$Age < 45] <- "40-45"
# Wang$Age_interval[Wang$Age >= 45 & Wang$Age < 50] <- "45-50"
# Wang$Age_interval[Wang$Age >= 50 & Wang$Age < 55] <- "50-55"
# Wang$Age_interval[Wang$Age >= 55 & Wang$Age < 60] <- "55-60"
# Wang$Age_interval[Wang$Age >= 60 & Wang$Age < 65] <- "60-65"
# Wang$Age_interval[Wang$Age >= 65 & Wang$Age < 70] <- "65-75"
# Wang$Age_interval[Wang$Age >= 70 & Wang$Age < 75] <- "70-75"
# Wang$Age_interval[Wang$Age >= 75 & Wang$Age < 80] <- "75-80"
# Wang$Age_interval[Wang$Age >= 80 & Wang$Age < 85] <- "80-85"
# Wang$Age_interval[Wang$Age >= 85 & Wang$Age < 90] <- "85-90"
# Wang$Age_interval[Wang$Age >= 90 & Wang$Age < 95] <- "90-95"
# Wang$Age_interval[Wang$Age >= 95 & Wang$Age < 100] <- "95-100"
# Wang$Age_interval[Wang$Age == "95"] <- "95-100"

Wang$Age_interval10 <- cut(Wang$Age,
                           breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                           right = FALSE,
                           include.lowest = TRUE)

# Wang$Age_interval10[Wang$Age >= 00 & Wang$Age < 10] <- "00"
# Wang$Age_interval10[Wang$Age >= 10 & Wang$Age < 20] <- "10"
# Wang$Age_interval10[Wang$Age >= 20 & Wang$Age < 30] <- "20"
# Wang$Age_interval10[Wang$Age >= 30 & Wang$Age < 40] <- "30"
# Wang$Age_interval10[Wang$Age >= 40 & Wang$Age < 50] <- "40"
# Wang$Age_interval10[Wang$Age >= 50 & Wang$Age < 60] <- "50"
# Wang$Age_interval10[Wang$Age >= 60 & Wang$Age < 70] <- "60"
# Wang$Age_interval10[Wang$Age >= 70 & Wang$Age < 80] <- "70"
# Wang$Age_interval10[Wang$Age >= 80 & Wang$Age < 90] <- "80"
# Wang$Age_interval10[Wang$Age >= 90 & Wang$Age < 100] <- "90"
