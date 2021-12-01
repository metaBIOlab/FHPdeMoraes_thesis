## Wang 2016

# OASIS
OASIS <- read_csv("~/idor/Gyrification/data/Wang2016e2019/OASIS.csv")

OASIS <-
  OASIS %>% mutate(Sample = "OASIS", Diagnostic = "CTL", ROI = "hemisphere") 

OASIS_lh <- OASIS %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
OASIS_rh <- OASIS %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

colnames(OASIS_lh)[which(names(OASIS_lh) == "TotalArea_lh")] <-
  "TotalArea"

colnames(OASIS_lh)[which(names(OASIS_lh) == "ExposedArea_lh")] <-
  "ExposedArea"

colnames(OASIS_lh)[which(names(OASIS_lh) == "AvgCorticalThickness_lh")] <-
  "AvgThickness"

colnames(OASIS_rh)[which(names(OASIS_rh) == "TotalArea_rh")] <-
  "TotalArea"

colnames(OASIS_rh)[which(names(OASIS_rh) == "ExposedArea_rh")] <-
  "ExposedArea"

colnames(OASIS_rh)[which(names(OASIS_rh) == "AvgCorticalThickness_rh")] <-
  "AvgThickness"

OASIS <- rbind(OASIS_lh, OASIS_rh)

colnames(OASIS)[which(names(OASIS) == "SubjectID")] <- "SUBJ"

OASIS$SUBJ <- as.character(OASIS$SUBJ)
OASIS$Age <- as.double(OASIS$Age)