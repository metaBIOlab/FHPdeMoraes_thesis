#### COMPARAR HEMIFERIOS ####

dados_hemi_comp <-
  reshape(
    as.data.frame(filter(dados, ROI == "hemisphere", method == "FreeSurferStandard", Longitudinal_correction == "yes", Session == 1)),
    idvar = 'SUBJ_ID',
    timevar = 'hemi',
    direction = 'wide'
  )

dados_hemi_comp <-
  dplyr::select(
    dados_hemi_comp,-c(
      K_ds.L,
      ExposedArea_ds.L,
      K_CHwoB.L,
      ExposedArea_CHwoB.L,
      c.L,
      SUBJ.R,
      ROI.R,
      Gender.R,
      Birthdate.R,
      Session.R,
      Age.R,
      Diagnostic.R,
      ESC.R,
      machine.R,
      acq_date.R,
      NeuroQuant.R,
      Lipoxina.R,
      Age_interval.R,
      Age_interval10.R,
      K_ds.R,
      ExposedArea_ds.R,
      K_CHwoB.R,
      ExposedArea_CHwoB.R,
      c.R,
      WM_volume_CC.R,
      Longitudinal_correction.R,
      WM_volume_CC_age_decay.R
    )
  )

colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "SUBJ.L")] <- "SUBJ"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "ROI.L")] <- "ROI"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Gender.L")] <- "Gender"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Birthdate.L")] <- "Birthdate"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Session.L")] <- "Session"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Age.L")] <- "Age"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Diagnostic.L")] <- "Diagnostic"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "ESC.L")] <- "ESC"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "machine.L")] <- "machine"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "acq_date.L")] <- "acq_date"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "NeuroQuant.L")] <- "NeuroQuant"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Lipoxina.L")] <- "Lipoxina"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Age_interval.L")] <- "Age_interval"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Age_interval10.L")] <- "Age_interval10"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "WM_volume_CC.L")] <- "WM_volume_CC"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "Longitudinal_correction.L")] <- "Longitudinal_correction"
colnames(dados_hemi_comp)[which(names(dados_hemi_comp) == "WM_volume_CC_age_decay.L")] <- "WM_volume_CC_age_decay"


dados_hemi_comp <-
  dados_hemi_comp %>% dplyr::select(SUBJ_ID, SUBJ, Session, Longitudinal_correction, ROI, AvgThickness.L, eTIV.L, TotalArea.L, GMvolume.L, WhiteSurfArea.L, localGI.L, ExposedArea.L, logAvgThickness.L, logTotalArea.L, logGMvolume.L, logWhiteSurfArea.L, logExposedArea.L, K.L, TotalArea_corrected.L, ExposedArea_corrected.L, localGI_corrected.L, logTotalArea_corrected.L, logExposedArea_corrected.L, K_corrected.L, K_age_decay.L, logWhiteSurfArea.L, WM_volume_CC, AvgThickness.R, eTIV.R, TotalArea.R, GMvolume.R, WhiteSurfArea.R, localGI.R, ExposedArea.R, logAvgThickness.R, logTotalArea.R, logGMvolume.R, logWhiteSurfArea.R, logExposedArea.R, K.R, TotalArea_corrected.R, ExposedArea_corrected.R, localGI_corrected.R, logTotalArea_corrected.R, logExposedArea_corrected.R, K_corrected.R, K_age_decay.R, logWhiteSurfArea.R, logConvexHullArea_age_decay.L, logConvexHullArea_age_decay.R)

dados <- full_join(dados, dados_hemi_comp)
