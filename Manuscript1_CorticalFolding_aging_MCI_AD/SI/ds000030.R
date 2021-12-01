#ds000030 DESCRIPTION

ds000030 <- read_delim("~/idor/Gyrification/data/ds000030/participants.tsv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::select(c(participant_id, diagnosis, age, gender))

colnames(ds000030)[which(names(ds000030) == "participant_id")] <-
  "SUBJ"

datads000030 <- read_csv("~/idor/Gyrification/data/ds000030/datads000030.txt")
datads000030 <- datads000030 %>% mutate(
  ROI = "hemisphere", GreymatterVol = as.double(GreymatterVol))

datads000030lobes <- read_csv("~/idor/Gyrification/data/ds000030/datads000030lobes.txt")
datads000030lobes <- datads000030lobes %>% mutate(ROI = as.character(Lobe), GreymatterVol = as.double(GreymatterVol))

ds000030 <- filter(ds000030, !is.na(age))

ds000030$age <- as.double(ds000030$age)

dados_ds000030 <- full_join(datads000030, datads000030lobes) %>% mutate(SUBJ = SubjectID) %>% left_join(ds000030)

dados_ds000030 <- mutate(dados_ds000030,
                         Sample = "ds000030",
                         Session <- as.double(1)
                         )

colnames(dados_ds000030)[which(names(dados_ds000030) == "age")] <-
  "Age"
colnames(dados_ds000030)[which(names(dados_ds000030) == "gender")] <-
  "Gender"
colnames(dados_ds000030)[which(names(dados_ds000030) == "diagnosis")] <-
  "Diagnostic"

write.csv(dados_ds000030, "ds000030.csv")