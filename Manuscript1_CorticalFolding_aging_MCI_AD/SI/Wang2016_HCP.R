## Wang 2016

# HCP500r
HCP500r <- read_excel("~/idor/Gyrification/data/Wang2016e2019/HCP500r.xlsx")

HCP500r <-
  HCP500r %>% mutate(Sample = "HCP500r", Diagnostic = "CTL", ROI = "hemisphere",
                     Age = ifelse(Age == "26-30", 28,
                                  ifelse(Age == "31-35", 33,
                                         ifelse(Age == "22-25", 23.5,
                                                ifelse(Age == "36+", 37, ""))))) 

HCP500r_lh <- HCP500r %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_lh,TotalArea_lh,ExposedArea_lh, Diagnostic, ROI) %>% mutate(hemi = "L")
HCP500r_rh <- HCP500r %>% dplyr::select(SubjectID, Sample, Gender, Age, AvgCorticalThickness_rh,TotalArea_rh,ExposedArea_rh, Diagnostic, ROI) %>% mutate(hemi = "R")

colnames(HCP500r_lh)[which(names(HCP500r_lh) == "TotalArea_lh")] <-
  "TotalArea"

colnames(HCP500r_lh)[which(names(HCP500r_lh) == "ExposedArea_lh")] <-
  "ExposedArea"

colnames(HCP500r_lh)[which(names(HCP500r_lh) == "AvgCorticalThickness_lh")] <-
  "AvgThickness"

colnames(HCP500r_rh)[which(names(HCP500r_rh) == "TotalArea_rh")] <-
  "TotalArea"

colnames(HCP500r_rh)[which(names(HCP500r_rh) == "ExposedArea_rh")] <-
  "ExposedArea"

colnames(HCP500r_rh)[which(names(HCP500r_rh) == "AvgCorticalThickness_rh")] <-
  "AvgThickness"

HCP500r <- rbind(HCP500r_lh, HCP500r_rh)

colnames(HCP500r)[which(names(HCP500r) == "SubjectID")] <- "SUBJ"

HCP500r$SUBJ <- as.character(HCP500r$SUBJ)
HCP500r$Age <- as.double(HCP500r$Age)