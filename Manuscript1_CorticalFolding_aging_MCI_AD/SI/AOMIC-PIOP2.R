#AOMICPIOP2 DESCRIPTION

AOMICPIOP2 <- read_delim("~/idor/Gyrification/AOMIC_PIOP2/participants.tsv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

colnames(AOMICPIOP2)[which(names(AOMICPIOP2) == "participant_id")] <-
  "SUBJ"

data <-
  read_csv("~/idor/Gyrification/AOMIC_PIOP2/dataAOMIC_PIOP2.txt") %>%
  mutate(ROI = "hemisphere",
         SUBJ = SubjectID) 

data_lobes <- read_csv("~/idor/Gyrification/AOMIC_PIOP2/dataAOMIC_PIOP2lobes.txt") %>%
  mutate(ROI = as.character(Lobe),
         SUBJ = SubjectID)

dados_AOMICPIOP2 <- full_join(data, data_lobes) %>%
   full_join(AOMICPIOP2)

dados_AOMICPIOP2 <- dados_AOMICPIOP2 %>% 
  mutate(Age = as.double(age), BMI =as.double(BMI), machine = "Philips-Achieva dStream 3T", FieldStrenght = 3, Diagnostic = "CTL", Sample = "AOMICPIOP2")

colnames(dados_AOMICPIOP2)[which(names(dados_AOMICPIOP2) == "sex")] <-
  "Gender"

dados_AOMICPIOP2 <- dados_AOMICPIOP2 %>% filter(!is.na(Age), age != "n/a") %>% dplyr::select(-c(age))

write.csv(dados_AOMICPIOP2, "AOMICPIOP2.csv")