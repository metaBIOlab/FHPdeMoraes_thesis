# importa o arquivo com as informacoes dos mamiferos
dados_MH2015 <- read_delim(
  "C:/Users/ferna/Documents/idor/Gyrification/data/amostras/amostraMH2015.csv",
  ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ",",
                  grouping_mark = "."),
  trim_ws = TRUE
)
dados_MH2015

colnames(dados_MH2015)[which(names(dados_MH2015) == "GM_surfarea")] <-
  "TotalArea"
colnames(dados_MH2015)[which(names(dados_MH2015) == "thickness")] <-
  "AvgThickness"
colnames(dados_MH2015)[which(names(dados_MH2015) == "GM_volume")] <-
  "GMvolume"

# insere o nome da Sample Mota & Houzel 2015

dados_MH2015 <-
  mutate(dados_MH2015, Sample = "Mota&Houzel2015")

dados_MH2015 <-
  dados_MH2015 %>% mutate(
    ExposedArea = TotalArea / localGI,
    logAvgThickness = log10(AvgThickness),
    logTotalArea = log10(TotalArea),
    logExposedArea = log10(ExposedArea),
    K = 1 / 2 * logAvgThickness + logTotalArea - 5 / 4 * logExposedArea,
    I = logTotalArea + logExposedArea + logAvgThickness^2,
    S = 3/2 * logTotalArea + 3/4 * logExposedArea - 9/4*logAvgThickness^2
  )
