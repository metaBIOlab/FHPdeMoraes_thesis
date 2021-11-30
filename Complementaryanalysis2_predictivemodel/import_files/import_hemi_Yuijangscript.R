
# IMPORTING HEMISPHERE DATA FROM YUJIANG/TOBIAS SCRIPT

data_longcor_ses1 <- read_csv("~/idor/Gyrification/data/resultados/data_longcor_ses1.txt")
data_longcor_ses2 <- read_csv("~/idor/Gyrification/data/resultados/data_longcor_ses2.txt")
data_longcor_ses3 <- read_csv("~/idor/Gyrification/data/resultados/data_longcor_ses3.txt")

#data_ses1 <- read_csv(str_c(path_yujiangscript,"data_ses1.txt"))
#data_ses2 <- read_csv(str_c(path_yujiangscript,"data_ses2.txt"))
#data_ses3 <- read_csv(str_c(path_yujiangscript,"data_ses3.txt"))

data_longcor_ses1 <- data_longcor_ses1 %>% mutate(Session = "1")

data_longcor_ses2 <- data_longcor_ses2 %>% mutate(Session = "2")

data_longcor_ses3 <- data_longcor_ses3 %>% mutate(Session = "3")


# data_ses1 <- data_ses1 %>% mutate(Session = "1")
# 
# data_ses2 <- data_ses2 %>% mutate(Session = "2")
# 
# data_ses3 <- data_ses3 %>% mutate(Session = "3")


data_Y_T_script <- full_join(data_longcor_ses1, data_longcor_ses2)
data_Y_T_script <-
  data_Y_T_script %>% full_join(data_longcor_ses3) 

#%>% full_join(data_ses1) %>% full_join(data_ses2) %>% full_join(data_ses3)

data_Y_T_script <- data_Y_T_script %>% mutate(
  SUBJ = SubjectID,
  hemi = Hemisphere,
  ROI = "hemisphere",
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
  k = sqrt(AvgThickness) * TotalArea / (ExposedArea ^ 1.25),
  K = 1 / 4 * log10(AvgThickness^2)  + log10(TotalArea) - 5 / 4 * log10(ExposedArea),
  S = 3 / 2 * log10(TotalArea) + 3 / 4 * log10(ExposedArea) - 9 /  4 * log10(AvgThickness^2) ,
  I = log10(TotalArea) + log10(ExposedArea) + log10(AvgThickness^2) ,
  Knorm = K/sqrt(1 + (1/4)^2 + (5/4)^2),
  Snorm = S/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
  Inorm = I/sqrt(1^2 + 1^2 + 1^2),
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

data_Y_T_script$hemi[data_Y_T_script$hemi == "left"] <- "L"
data_Y_T_script$hemi[data_Y_T_script$hemi == "right"] <- "R"
