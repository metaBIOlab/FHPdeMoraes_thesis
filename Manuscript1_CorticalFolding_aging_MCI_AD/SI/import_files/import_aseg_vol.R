# WHITE MATTER VOLUME  #####

# DATA IMPORT ####

aseg_vol <-
  unique(read_delim(
    str_c(path, "aseg_vol.txt"),
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE
  ))

# DATA ORGANIZING ####

names(aseg_vol)[1] <- "SUBJ_ID"

aseg_vol <- mutate(
  aseg_vol,
  SUBJ = str_sub(aseg_vol$SUBJ_ID, 1, 7),
  Session = as.double(str_sub(aseg_vol$SUBJ_ID, 21, 21)),
  Longitudinal_correction = ifelse(str_sub(aseg_vol$SUBJ_ID, 23, 26) == "long", "yes", "no"),
  SUBJ_ID = str_sub(aseg_vol$SUBJ_ID, 9, 38),
  method = "FreeSurferStandard"
)

# FreeSurfer names ####

FreeSurfer_as_NQ <- aseg_vol

oldnames = c(
  "EstimatedTotalIntraCranialVol",
  "SupraTentorialVolNotVent",
  "lhCerebralWhiteMatterVol",
  "rhCerebralWhiteMatterVol",
  "Left-Lateral-Ventricle",
  "Right-Lateral-Ventricle",
  "Left-Inf-Lat-Vent",
  "Right-Inf-Lat-Vent",
  "4th-Ventricle",
  "Left-Caudate",
  "Right-Caudate",
  "Left-Putamen",
  "Right-Putamen",
  "Left-Pallidum",
  "Right-Pallidum",
  "Left-Thalamus-Proper",
  "Right-Thalamus-Proper",
  "Left-Amygdala",
  "Right-Amygdala",
  "Left-Hippocampus",
  "Right-Hippocampus",
  "Left-VentralDC",
  "Right-VentralDC",
  "Left-Cerebellum-White-Matter",
  "Right-Cerebellum-White-Matter",
  "Left-Cerebellum-Cortex",
  "Right-Cerebellum-Cortex",
  "Brain-Stem",
  "Left-Accumbens-area",
  "Right-Accumbens-area",
  "Left-vessel",
  "Right-vessel",
  "Left-choroid-plexus",
  "Right-choroid-plexus",
  "5th-Ventricle",
  "WM-hypointensities",
  "Left-WM-hypointensities",
  "Right-WM-hypointensities",
  "non-WM-hypointensities",
  "Left-non-WM-hypointensities",
  "Right-non-WM-hypointensities",
  "Optic-Chiasm",
  "BrainSegVol-to-eTIV",
  "MaskVol-to-eTIV",
  "3rd-Ventricle"
)
newnames <- c(
  "IntraCranialVolume",
  "ForebrainParenchyma",
  "LeftCorticalWhiteMatter",
  "RightCorticalWhiteMatter",
  "LeftLateralVentricle",
  "RightLateralVentricle",
  "LeftInferiorLateralVentricle",
  "RightInferiorLateralVentricle",
  "4thVentricle",
  "LeftCaudate",
  "RightCaudate",
  "LeftPutamen",
  "RightPutamen",
  "LeftPallidum",
  "RightPallidum",
  "LeftThalamus",
  "RightThalamus",
  "LeftAmygdala",
  "RightAmygdala",
  "LeftHippocampus",
  "RightHippocampus",
  "LeftVentralDiencephalon",
  "RightVentralDiencephalon",
  "LeftCerebellarWhiteMatter",
  "RightCerebellarWhiteMatter",
  "LeftCerebellarGrayMatter",
  "RightCerebellarGrayMatter",
  "Brainstem",
  "LeftAccumbensarea",
  "RightAccumbensarea",
  "Leftvessel",
  "Rightvessel",
  "Leftchoroidplexus",
  "Rightchoroidplexus",
  "5thVentricle",
  "WM_hypointensities",
  "Left_WM_hypointensities",
  "Right_WM_hypointensities",
  "non_WM_hypointensities",
  "Left_non_WM_hypointensities",
  "Right_non_WM_hypointensities",
  "Optic_Chiasm",
  "BrainSegVol_to_eTIV",
  "MaskVol_to_eTIV",
  "3rd_Ventricle"
)

FreeSurfer_as_NQ <-
  FreeSurfer_as_NQ %>% rename_at(vars(oldnames), ~ newnames)

rm(newnames)
rm(oldnames)

# novas variaveis (combinacoes de vaiaveis do FS para o NQ)

WholeBrainParenchyma <-
  FreeSurfer_as_NQ$ForebrainParenchyma +
  FreeSurfer_as_NQ$LeftCerebellarGrayMatter +
  FreeSurfer_as_NQ$RightCerebellarGrayMatter +
  FreeSurfer_as_NQ$LeftCerebellarGrayMatter +
  FreeSurfer_as_NQ$RightCerebellarWhiteMatter +
  FreeSurfer_as_NQ$Brainstem

CorticalGrayMatter <-
  FreeSurfer_as_NQ$TotalGrayVol -
  FreeSurfer_as_NQ$SubCortGrayVol

LeftCerebellum <-
  FreeSurfer_as_NQ$LeftCerebellarGrayMatter +
  FreeSurfer_as_NQ$LeftCerebellarWhiteMatter
RightCerebellum <-
  FreeSurfer_as_NQ$RightCerebellarGrayMatter +
  FreeSurfer_as_NQ$RightCerebellarWhiteMatter

`3rdVentricle` <-
  FreeSurfer_as_NQ$`3rd_Ventricle` +
  FreeSurfer_as_NQ$CSF

TotalCerebrospinalFluid <-
  FreeSurfer_as_NQ$LeftLateralVentricle +
  FreeSurfer_as_NQ$RightLateralVentricle +
  FreeSurfer_as_NQ$LeftInferiorLateralVentricle +
  FreeSurfer_as_NQ$RightInferiorLateralVentricle +
  FreeSurfer_as_NQ$`3rd_Ventricle` +
  FreeSurfer_as_NQ$`4thVentricle` +
  FreeSurfer_as_NQ$`5thVentricle` +
  FreeSurfer_as_NQ$CSF +
  FreeSurfer_as_NQ$Leftchoroidplexus +
  FreeSurfer_as_NQ$Rightchoroidplexus

FreeSurfer_as_NQ <-
  as_tibble(
    cbind(
      FreeSurfer_as_NQ,
      CorticalGrayMatter,
      LeftCerebellum,
      RightCerebellum,
      `3rdVentricle`,
      TotalCerebrospinalFluid
    )
  )


