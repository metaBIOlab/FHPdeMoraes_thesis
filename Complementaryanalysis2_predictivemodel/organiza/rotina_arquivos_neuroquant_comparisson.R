# PREPARO --------
# run import_vol_asegstats_FS
source("preprao/import_files/import_vol_asegstats_FS.R")

# combina a tabelas gerada (Volume) através do sujeito
# provavelmente algumas linhas estarão em branco, sujeitos que tiveram algum erro

dados_FSasNQ <- right_join(tabela_sujeitos, FreeSurfer_as_NQ)

# bota visita como fator (mais fácil para fazer alguns gráficos)

dados_FSasNQ$Session <- as.factor(dados_FSasNQ$Session)

# deleta os sujeitos sem resultados
# dados_FSasNQ <- na.exclude(dados_FSasNQ)