# importa o arquivo com as informacoes dos mamiferos
dados_MH2015 <- read_excel("~/idor/Gyrification/data/amostras/dados_Bruno_etal_Nature_UniversalScaling.xls")

## Renomeia as colunas
colnames(dados_MH2015)[3] <- "TotalArea"
colnames(dados_MH2015)[5] <- "AvgThickness"

# insere o nome da Sample Mota & Houzel 2015

dados_MH2015 <-
  mutate(
    dados_MH2015,
    Sample = ifelse(Dataset == "Kamilla", "Kamilla", "Mota&Houzel2015"),
    ROI = "hemisphere",
    Diagnostic = "CTL"
  )

dados_MH2015 <-
  mutate(dados_MH2015,
         Sample = "Mota&Houzel2015",
         ROI = "hemisphere",
         Diagnostic = "CTL")

dados_MH2015 <-
  dados_MH2015 %>% mutate(
    ExposedArea = TotalArea / FI,
    localGI = FI,
    logAvgThickness = log10(AvgThickness),
    logTotalArea = log10(TotalArea),
    logExposedArea = log10(ExposedArea),
    k = sqrt(AvgThickness) * TotalArea / (ExposedArea ^ 1.25),
    K = 1 / 4 * log10(AvgThickness^2)  + log10(TotalArea) - 5 / 4 * log10(ExposedArea),
    S = 3 / 2 * log10(TotalArea) + 3 / 4 * log10(ExposedArea) - 9 /  4 * log10(AvgThickness^2) ,
    I = log10(TotalArea) + log10(ExposedArea) + log10(AvgThickness^2),
    K_age_decay = K,
    S_age_decay = S,
    Knorm = K/sqrt(1 + (1/4)^2 + (5/4)^2),
    Snorm = S/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2),
    Inorm = I/sqrt(1^2 + 1^2 + 1^2),
    Knorm_age_decay = K_age_decay/sqrt(1 + (1/4)^2 + (5/4)^2),
    Snorm_age_decay = S_age_decay/sqrt((3/2)^2 + (3/4)^2 + (9/4)^2))

# write.csv(dados_MH2015, "dados_MH2015.csv")

