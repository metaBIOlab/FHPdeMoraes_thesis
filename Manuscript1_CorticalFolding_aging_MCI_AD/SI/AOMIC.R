#AOMIC DESCRIPTION

AOMIC <- read_delim("~/idor/Gyrification/AOMIC/participants.tsv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

colnames(AOMIC)[which(names(AOMIC) == "participant_id")] <-
  "SUBJ"

dataAOMIC <- read_csv("~/idor/Gyrification/AOMIC/dataAOMIC.txt")
dataAOMIC <- dataAOMIC %>% mutate(
  ROI = "hemisphere", GreymatterVol = as.double(GreymatterVol))

dataAOMIClobes <- read_csv("~/idor/Gyrification/AOMIC/dataAOMIClobes2.txt")
dataAOMIClobes <- dataAOMIClobes %>% mutate(ROI = as.character(Lobe), GreymatterVol = as.double(GreymatterVol))

AOMIC <- filter(AOMIC, !is.na(age))
AOMIC <- filter(AOMIC, !is.na(BMI))

AOMIC$age <- as.double(AOMIC$age)
AOMIC$BMI <- as.double(AOMIC$BMI)

dados_AOMIC <- full_join(dataAOMIC, dataAOMIClobes) %>%
  mutate(SUBJ = SubjectID) %>%
  left_join(AOMIC)

dados_AOMIC <- mutate(dados_AOMIC, Sample = "AOMICPIOP1", Diagnostic = "CONTROLE", machine = "Philips-Achieva 3T", FieldStrenght =3)

colnames(dados_AOMIC)[which(names(dados_AOMIC) == "age")] <-
  "Age"
colnames(dados_AOMIC)[which(names(dados_AOMIC) == "sex")] <-
  "Gender"

write.csv(dados_AOMIC, "AOMICPIOP1.csv")
