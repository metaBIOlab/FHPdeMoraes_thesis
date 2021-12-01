# COMPARING SLOPES - DATASETS PAPER YUJIANG

# IMPORT ####

HCP500r <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/amostras/HCP500r.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

NKI <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/amostras/NKI.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

OASIS_healthy <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/amostras/OASIS_healthy.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

ADNIControl_NonLongitudinal <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/amostras/ADNIControl_NonLongitudinal.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

ADNIAD_NonLongitudinal <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/amostras/ADNIAD_NonLongitudinal.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)


# ORGANIZING ####
HCP500r <-
  HCP500r %>% mutate(Sample = "HCP500r", Diagnostic = "CTL", ROI = "hemisphere")

HCP500r_lh <- HCP500r %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
HCP500r_rh <- HCP500r %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

NKI <- NKI %>% mutate(Sample = "NKI", Diagnostic = "CTL", ROI = "hemisphere")

NKI_lh <- NKI %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
NKI_rh <- NKI %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

OASIS_healthy <-
  OASIS_healthy %>% mutate(Sample = "OASIS", Diagnostic = "CTL", ROI = "hemisphere") 

OASIS_healthy_lh <- OASIS_healthy %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
OASIS_healthy_rh <- OASIS_healthy %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

ADNIAD_NonLongitudinal <-
ADNIAD_NonLongitudinal %>% mutate(Sample = "ADNI", Diagnostic = "AD", ROI = "hemisphere") 

ADNIAD_NonLongitudinal_lh <- ADNIAD_NonLongitudinal %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
ADNIAD_NonLongitudinal_rh <- ADNIAD_NonLongitudinal %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

ADNIControl_NonLongitudinal <-
ADNIControl_NonLongitudinal %>% mutate(Sample = "ADNI", Diagnostic = "CTL", ROI = "hemisphere") 

ADNIControl_NonLongitudinal_lh <- ADNIControl_NonLongitudinal %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
ADNIControl_NonLongitudinal_rh <- ADNIControl_NonLongitudinal %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

datasets_yujiang_lh <- do.call("rbind", list(HCP500r_lh, NKI_lh, OASIS_healthy_lh, ADNIAD_NonLongitudinal_lh, ADNIControl_NonLongitudinal_lh))

datasets_yujiang_rh <- do.call("rbind", list(HCP500r_rh, NKI_rh, OASIS_healthy_rh, ADNIAD_NonLongitudinal_rh, ADNIControl_NonLongitudinal_rh))

colnames(datasets_yujiang_lh)[which(names(datasets_yujiang_lh) == "TotalArea_lh")] <-
  "TotalArea"

colnames(datasets_yujiang_lh)[which(names(datasets_yujiang_lh) == "ExposedArea_lh")] <-
  "ExposedArea"

colnames(datasets_yujiang_lh)[which(names(datasets_yujiang_lh) == "AvgCorticalThickness_lh")] <-
  "AvgThickness"

colnames(datasets_yujiang_rh)[which(names(datasets_yujiang_rh) == "TotalArea_rh")] <-
  "TotalArea"

colnames(datasets_yujiang_rh)[which(names(datasets_yujiang_rh) == "ExposedArea_rh")] <-
  "ExposedArea"

colnames(datasets_yujiang_rh)[which(names(datasets_yujiang_rh) == "AvgCorticalThickness_rh")] <-
  "AvgThickness"

datasets_yujiang <- rbind(datasets_yujiang_lh, datasets_yujiang_rh)

colnames(datasets_yujiang)[which(names(datasets_yujiang) == "SubjectID")] <-
  "SUBJ"

datasets_yujiang <-
  datasets_yujiang %>% mutate(
    ROI = "hemisphere"
  )

datasets_yujiang$Age[datasets_yujiang$Age == "22-25"] <- 23.5
datasets_yujiang$Age[datasets_yujiang$Age == "26-30"] <- 28
datasets_yujiang$Age[datasets_yujiang$Age == "31-35"] <- 33
datasets_yujiang$Age[datasets_yujiang$Age == "36+"] <- 37

datasets_yujiang$Age <- as.double(datasets_yujiang$Age)

