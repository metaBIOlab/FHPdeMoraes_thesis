# PREPARACAO ----
source("ZK.R")
source("Micro.R")

# junta as amostras para comparacao ----
dados_datasetscomp <- dados_micro %>%
  mutate(Trimestre_exantema = 0) %>%
  full_join(dados_zk)

dados_datasetscomp <- dados_datasetscomp %>%
  mutate(
    hemi = Hemisphere,
    AvgThickness = AvgCortThickness,
    TotalArea = PialArea,
    ExposedArea = SmoothPialArea
  ) 

source("analises/variables.R")

dados_datasetscomp$ROI <- as.factor(dados_datasetscomp$ROI)
dados_datasetscomp$Diagnostic <- as.factor(dados_datasetscomp$Diagnostic)
dados_datasetscomp$Sample <- as.factor(dados_datasetscomp$Sample)
dados_datasetscomp$Gender <- as.factor(dados_datasetscomp$Gender)

dados_datasetscomp <- dados_datasetscomp %>%
  dplyr::select(
    -c(Lobe,
      SubjectID,
      Lobe,
      AvgCortThickness,
      PialArea,
      WhiteArea,
      SmoothPialArea,
      GreymatterVol,
      Hemisphere
    )
  )  %>%
  filter(
    !is.na(TotalArea),
    ExposedArea != 0,!is.na(localGI),!is.infinite(AvgThickness),!is.na(Diagnostic), AvgThickness !=0,
    localGI != 0,!is.infinite(AvgThickness),!is.na(AvgThickness)
    ) %>%
  droplevels() %>%
  unique()

# dados_datasetscomp$Diagnostic <- revalue(dados_datasetscomp$Diagnostic, c("CONTROLE"="CTL", "Control" = "CTL", "ALZ"="AD", "CCL" = "MCI"))
# dados_datasetscomp$Diagnostic <- revalue(dados_datasetscomp$Diagnostic, c("CONTROLE"="CTL", "ALZ"="AD", "CCL" = "MCI", "AgCC" = "CCD"))

dados_datasetscomp$Diagnostic <- as.character(dados_datasetscomp$Diagnostic)

dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "CONTROLE"] <- "CTL"
dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "Control"] <- "CTL"
dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "AgCC"] <- "CCD"
dados_datasetscomp$Diagnostic[dados_datasetscomp$Diagnostic == "MICRO"] <- "MICRO"

dados_datasetscomp$Diagnostic <- factor(dados_datasetscomp$Diagnostic)

dados_datasetscomp$Sample <- as.character(dados_datasetscomp$Sample)

dados_datasetscomp$Sample[dados_datasetscomp$Sample == "IDOR-CCD"] <- "IDOR"
dados_datasetscomp$Sample[dados_datasetscomp$Sample == "IDOR-ZK"] <- "IDOR"
dados_datasetscomp$Sample[dados_datasetscomp$Sample == "IDOR-ZK-MICRO"] <- "IDOR"
dados_datasetscomp$Sample[dados_datasetscomp$Sample == "IDOR-MICRO"] <- "IDOR"

dados_datasetscomp$Sample <- factor(dados_datasetscomp$Sample)

dados_datasetscomp$Gender[dados_datasetscomp$Gender == "F"] <- "FEM"
dados_datasetscomp$Gender[dados_datasetscomp$Gender == "M"] <- "MASC"
dados_datasetscomp$Gender[dados_datasetscomp$Gender == "f"] <- "FEM"
dados_datasetscomp$Gender[dados_datasetscomp$Gender == "m"] <- "MASC"
#dados_datasetscomp$Gender[dados_datasetscomp$Gender == "u"] <- "U"

dados_datasetscomp <- filter(dados_datasetscomp, !is.nan(Age), !is.na(Age), Gender != "u") %>%
  droplevels()


rm(
  all_sessions,
  data_caltechucsf,
  data_lobes_caltechucsf,
  part_ses,
  participants,
  tabela_sujeitos,
  tabela_sujeitos_caltechucsf,
  UCSF_CALTECH_IDOR_AgCCDatabase,
  data,
  data_lobes,
  decay_AvgThickness,
  decay_logExposedArea,
  decay_logTotalArea
)
