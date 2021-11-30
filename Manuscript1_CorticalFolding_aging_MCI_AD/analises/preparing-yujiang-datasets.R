# COMPARING SLOPES - DATASETS PAPER YUJIANG

# IMPORT ####

HCP500r <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/Amostras/HCP500r.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

NKI <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/Amostras/NKI.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

OASIS_healthy <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/Amostras/OASIS_healthy.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

ADNIControl_NonLongitudinal <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/Amostras/ADNIControl_NonLongitudinal.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

ADNIAD_NonLongitudinal <- read_delim("C:/Users/ferna/Documents/idor/Gyrification/data/Amostras/ADNIAD_NonLongitudinal.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)


# ORGANIZING ####
HCP500r <-
  HCP500r %>% mutate(Sample = "HCP500r", Diagnostic = "CTL", ROI = "hemisphere")

HCP500r_lh <- HCP500r %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
HCP500r_rh <- HCP500r %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

NKI <- NKI %>% mutate(Sample = "NKI", Diagnostic = "CTL", ROI = "hemisphere")

NKI_lh <- NKI %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
NKI_rh <- NKI %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

OASIS_healthy <-
  OASIS_healthy %>% mutate(Sample = "OASIS_healthy", Diagnostic = "CTL", ROI = "hemisphere") 

OASIS_healthy_lh <- OASIS_healthy %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
OASIS_healthy_rh <- OASIS_healthy %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

ADNIAD_NonLongitudinal <-
ADNIAD_NonLongitudinal %>% mutate(Sample = "ADNIAD", Diagnostic = "AD", ROI = "hemisphere") 

ADNIAD_NonLongitudinal_lh <- ADNIAD_NonLongitudinal %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
ADNIAD_NonLongitudinal_rh <- ADNIAD_NonLongitudinal %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

ADNIControl_NonLongitudinal <-
ADNIControl_NonLongitudinal %>% mutate(Sample = "ADNIControl", Diagnostic = "CTL", ROI = "hemisphere") 

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
    localGI = TotalArea / ExposedArea,
    logAvgThickness = log10(AvgThickness),
    logTotalArea = log10(TotalArea),
    logExposedArea = log10(ExposedArea),
    K = 1 / 2 * logAvgThickness + logTotalArea - 5 / 4 * logExposedArea,
    I = logTotalArea + logExposedArea + logAvgThickness^2,
    S = 3/2 * logTotalArea + 3/4 * logExposedArea - 9/4*logAvgThickness^2
  )

datasets_yujiang$Gender <- as.factor(datasets_yujiang$Gender)
datasets_yujiang$Diagnostic <- as.factor(datasets_yujiang$Diagnostic)

datasets_yujiang$Age[datasets_yujiang$Age == "22-25"] <- "22"
datasets_yujiang$Age[datasets_yujiang$Age == "26-30"] <- "26"
datasets_yujiang$Age[datasets_yujiang$Age == "31-35"] <- "31"
datasets_yujiang$Age[datasets_yujiang$Age == "36+"] <- "36"

datasets_yujiang$Age <- as.double(datasets_yujiang$Age)