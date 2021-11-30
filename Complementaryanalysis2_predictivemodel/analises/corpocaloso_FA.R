# FA CORPUS CALLOSUM ----

FA_CC <- read_excel(str_c(path_FA,"FA_CC.xlsx"))

dados <- full_join(dados, FA_CC)