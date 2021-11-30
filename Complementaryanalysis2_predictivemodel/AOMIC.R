#AOMIC DESCRIPTION

AOMIC <- read_delim("~/idor/Gyrification/AOMIC/participants.tsv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
colnames(AOMIC)[which(names(AOMIC) == "participant_id")] <-
  "SUBJ"

source("import_files/import_hemi_AOMIC.R")
source("import_files/import_lobesresults_AOMIC.R")

AOMIC$age <- as.double(AOMIC$age)
AOMIC$BMI <- as.double(AOMIC$BMI)

summary(AOMIC)

AOMIC <- filter(AOMIC, !is.na(age))
AOMIC %>% group_by(sex) %>% summarise(n = n_distinct(SUBJ))

dados_AOMIC <- full_join(dataAOMIC, dataAOMIClobes)
dados_AOMIC <- right_join(AOMIC, dados_AOMIC)

dados_AOMIC <- mutate(dados_AOMIC, Session = "1", Diagnostic = "CONTROLE")

colnames(dataAOMIClobes)[which(names(dataAOMIClobes) == "age")] <-
  "Age"
colnames(dataAOMIClobes)[which(names(dataAOMIClobes) == "sex")] <-
  "Gender"


dados_AOMIC$TotalArea_corrected[dados_AOMIC$ROI == "hemisphere"] <- dados_AOMIC$TotalArea[dados_AOMIC$ROI == "hemisphere"]
dados_AOMIC$ExposedArea_corrected[dados_AOMIC$ROI == "hemisphere"] <- dados_AOMIC$ExposedArea[dados_AOMIC$ROI == "hemisphere"]
dados_AOMIC$K_corrected[dados_AOMIC$ROI == "hemisphere"] <- dados_AOMIC$K[dados_AOMIC$ROI == "hemisphere"]
dados_AOMIC$localGI_corrected[dados_AOMIC$ROI == "hemisphere"] <- dados_AOMIC$localGI[dados_AOMIC$ROI == "hemisphere"]
dados_AOMIC$logTotalArea_corrected[dados_AOMIC$ROI == "hemisphere"] <- dados_AOMIC$logTotalArea[dados_AOMIC$ROI == "hemisphere"]
dados_AOMIC$logExposedArea_corrected[dados_AOMIC$ROI == "hemisphere"] <- dados_AOMIC$logExposedArea[dados_AOMIC$ROI == "hemisphere"]
dados_AOMIC$S_corrected[dados_AOMIC$ROI == "hemisphere"] <- dados_AOMIC$S[dados_AOMIC$ROI == "hemisphere"]
dados_AOMIC$I_corrected[dados_AOMIC$ROI == "hemisphere"] <- dados_AOMIC$I[dados_AOMIC$ROI == "hemisphere"]

dados_AOMIC <- filter(dados_AOMIC, !is.infinite(logTotalArea_corrected))