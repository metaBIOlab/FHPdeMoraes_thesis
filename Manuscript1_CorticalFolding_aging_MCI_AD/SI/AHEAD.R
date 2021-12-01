#AHEAD DESCRIPTION

AHEAD <- read_csv(
  "C:/Users/ferna/Documents/idor/Gyrification/AHEAD/participants.csv",
  col_types = cols(
    X4 = col_skip(),
    X5 = col_skip(),
    X6 = col_skip(),
    X7 = col_skip()
  )
)

colnames(AHEAD)[which(names(AHEAD) == "ScanName")] <-
  "SUBJ"
colnames(AHEAD)[which(names(AHEAD) == "Group")] <-
  "age"

dataAHEAD <- read_csv("~/idor/Gyrification/AHEAD/dataAHEAD.txt")
dataAHEAD <- dataAHEAD %>% mutate(
  ROI = "hemisphere")

dataAHEADlobes <- read_csv("~/idor/Gyrification/AHEAD/dataAHEADlobes2.txt")
dataAHEADlobes <- dataAHEADlobes %>% mutate(ROI = as.character(Lobe))


AHEAD <- filter(AHEAD,!is.na(age))

dados_AHEAD <- full_join(dataAHEAD, dataAHEADlobes) %>% mutate(SUBJ = SubjectID) %>% left_join(AHEAD)

dados_AHEAD <-
  mutate(dados_AHEAD, Diagnostic = "CONTROLE", 
         Sample = "AHEAD", machine = "Philips-Achieva-7T", FieldStrenght =7)

# colnames(dados_AHEAD)[which(names(dados_AHEAD) == "age")] <- "Age"
colnames(dados_AHEAD)[which(names(dados_AHEAD) == "Sex")] <-
  "Gender"

dados_AHEAD$Gender[dados_AHEAD$Gender == "f"] <- "FEM"
dados_AHEAD$Gender[dados_AHEAD$Gender == "m"] <- "MASC"

dados_AHEAD <- dados_AHEAD %>% mutate(Age = ifelse(age == "18-30",24, ifelse(age == "31-40", 35.5, ifelse(age == "41-50", 45.5, ifelse(age == "51-60", 55.5, ifelse(age == "61-70", 65.5, 75.5))))))
dados_AHEAD <- dados_AHEAD %>% filter(SUBJ != "sub-0029",  SUBJ != "sub-0050") %>% dplyr::select(-c(age))

write.csv(dados_AHEAD, "AHEAD.csv")