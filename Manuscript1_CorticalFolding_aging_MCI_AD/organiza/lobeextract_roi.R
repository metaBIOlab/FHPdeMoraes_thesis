# NOMEIA AS ROIS

LobesExtract_ALL_results$nROI[LobesExtract_ALL_results$nROI == 1] <-
  "CC"
LobesExtract_ALL_results$nROI[LobesExtract_ALL_results$nROI == 2] <-
  "F"
LobesExtract_ALL_results$nROI[LobesExtract_ALL_results$nROI == 3] <-
  "P"
LobesExtract_ALL_results$nROI[LobesExtract_ALL_results$nROI == 4] <-
  "T"
LobesExtract_ALL_results$nROI[LobesExtract_ALL_results$nROI == 5] <-
  "O"
LobesExtract_ALL_results$nROI[LobesExtract_ALL_results$nROI == 6] <-
  "Insula"

names(LobesExtract_ALL_results)[2] <- "ROI"

LobesExtract_ALL_results$ROI <-
  as.factor(LobesExtract_ALL_results$ROI)