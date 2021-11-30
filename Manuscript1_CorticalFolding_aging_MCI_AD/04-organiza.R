# ORGANIZING ALL DATA AND INCLUDING VARIABLES ####

# remove extra info
tabela_sujeitos <- dplyr::select(tabela_sujeitos, -c(participant_id, session_id))

# ----
data_Y_T_script$Session <- as.double(data_Y_T_script$Session)
data_lobes$Session <- as.double(data_lobes$Session)

data <- full_join(data_Y_T_script, data_lobes)

# passa as informacoes de area dos hemisferios para a coluna de area corrigida ----

data$TotalArea_corrected[data$ROI == "hemisphere"] <- data$TotalArea[data$ROI == "hemisphere"]
data$ExposedArea_corrected[data$ROI == "hemisphere"] <- data$ExposedArea[data$ROI == "hemisphere"]
data$K_corrected[data$ROI == "hemisphere"] <- data$K[data$ROI == "hemisphere"]
data$localGI_corrected[data$ROI == "hemisphere"] <- data$localGI[data$ROI == "hemisphere"]
data$logTotalArea_corrected[data$ROI == "hemisphere"] <- data$logTotalArea[data$ROI == "hemisphere"]
data$logExposedArea_corrected[data$ROI == "hemisphere"] <- data$logExposedArea[data$ROI == "hemisphere"]
data$S_corrected[data$ROI == "hemisphere"] <- data$S[data$ROI == "hemisphere"]
data$I_corrected[data$ROI == "hemisphere"] <- data$I[data$ROI == "hemisphere"]


# combina as tabelas geradas (Surfarea + Thickness + Volume + lGI = data) através do sujeito
dados <- inner_join(tabela_sujeitos, data)

dados$Age <- as.numeric(dados$Age)
dados$Diagnostic <- as.factor(dados$Diagnostic)
# dados$Age_interval10 <- as.factor(dados$Age_interval10)
dados$Gender <- as.factor(dados$Gender)
dados$ROI <- as.factor(dados$ROI)
dados$Session <- as.double(dados$Session)

#-------LIMPA AS COISAS REPETIDAS--------
#dados <- dados %>% dplyr::select(-c(SUBJ_antigo))
#dados <- filter(dados, Longitudinal_correction =="yes")
#dados <- dados %>% dplyr::select(-c(Longitudinal_correction, method)) %>% unique()
