# IMPORT WHITE MATTER TOTAL AREA #

lh_white_surfarea_1 <- read.delim("data/resultados/lh_white_surfarea_table.txt")

rh_white_surfarea_1 <- read.delim("data/resultados/rh_white_surfarea_table.txt")

lh_white_surfarea_2 <- read.delim("data/resultados/lh_white_surfarea_table_subjs_novos.txt")

rh_white_surfarea_2 <- read.delim("data/resultados/rh_white_surfarea_table_subjs_novos.txt")

lh_white_surfarea_3 <- read.delim("data/resultados/lh_white_surfarea_table_novos3.txt")

rh_white_surfarea_3 <- read.delim("data/resultados/rh_white_surfarea_table_novos3.txt")

## JUNTA OS ARQUIVOS ####

lh_white_surfarea <- import_variable_bindmultiplesoucer(lh_white_surfarea, lh_white_surfarea_1, lh_white_surfarea_2, lh_white_surfarea_3)
rh_white_surfarea <- import_variable_bindmultiplesoucer(rh_white_surfarea, rh_white_surfarea_1, rh_white_surfarea_2, rh_white_surfarea_3)


colnames(lh_white_surfarea)[which(names(lh_white_surfarea) == "lh.aparc.area")] <-
  "SUBJ_ses"
colnames(rh_white_surfarea)[which(names(rh_white_surfarea) == "rh.aparc.area")] <-
  "SUBJ_ses"

## COLOCA O HEMISFERIO #### 

lh_white_surfarea <- mutate(lh_white_surfarea, hemi = "L")
rh_white_surfarea <- mutate(rh_white_surfarea, hemi = "R")

##JUNTA EM UM ARQUIVO OS DOIS HEMISFERIOS ####
WhiteSurfArea_all <- full_join(lh_white_surfarea, rh_white_surfarea)
WhiteSurfArea <- dplyr::select(WhiteSurfArea_all, c(SUBJ_ses, hemi,WhiteSurfArea))

data_h <- full_join(data_h, WhiteSurfArea)

rm(lh_white_surfarea_1)
rm(rh_white_surfarea_1)
rm(lh_white_surfarea_2)
rm(rh_white_surfarea_2)
rm(lh_white_surfarea_3)
rm(rh_white_surfarea_3)
rm(lh_white_surfarea)
rm(rh_white_surfarea)