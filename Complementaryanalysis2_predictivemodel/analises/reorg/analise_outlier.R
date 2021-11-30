# RETIRAR OS OUTLIERS

dados_hemi_v1_Philips <-
  filter(dados_hemi_v1, RM_Maquina == "Philips")

boxplot.with.outlier.label((filter(dados_hemi_v1_Philips,  Diagnostic == "CONTROLE"))$K ~ (filter(dados_hemi_v1_Philips, Diagnostic == "CONTROLE"))$Age_interval10,
                           label_name = (filter(dados_hemi_v1_Philips, Diagnostic == "CONTROLE")$SUBJ_ses)) # GRAFICO OUTLIER CONTROLES

boxplot.with.outlier.label((filter(dados_hemi_v1_Philips, Diagnostic == "CCL"))$K ~ (filter(dados_hemi_v1_Philips, Diagnostic == "CCL"))$Age_interval10,
                           label_name = (filter(dados_hemi_v1_Philips, Diagnostic == "CCL"))$SUBJ_ses) # GRAFICO OUTLIER CCLs

boxplot.with.outlier.label((filter(dados_hemi_v1_Philips, Diagnostic == "ALZ"))$K ~ (filter(dados_hemi_v1_Philips, Diagnostic == "ALZ"))$Age_interval10,
                           label_name = (filter(dados_hemi_v1_Philips, Diagnostic == "ALZ"))$SUBJ_ses) # GRAFICO OUTLIER DAs
