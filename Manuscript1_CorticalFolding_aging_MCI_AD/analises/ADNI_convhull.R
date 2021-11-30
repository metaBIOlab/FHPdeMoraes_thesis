ADNIAD_mat <- readMat(str_c(path, "ADNI_AD.mat"))

ADNIControl_mat <- readMat(str_c(path, "ADNI_Ctrl.mat"))

ADNIAD_unlist <- matrix(unlist(ADNIAD_mat), ncol = 21, byrow = FALSE)

ADNIControl_unlist <-
  matrix(unlist(ADNIControl_mat), ncol = 21, byrow = FALSE)

ADNIAD_L <- data.frame(cbind(
  ADNIAD_unlist[, 1],
  hemi = "L",
  diag = "AD",
  AvgCortThickness = ADNIAD_mat$ADNI.AD[[2]][, 1],
  PialArea = ADNIAD_mat$ADNI.AD[[3]][, 1],
  SmoothPialArea = ADNIAD_mat$ADNI.AD[[4]][, 1],
  WhiteArea = ADNIAD_mat$ADNI.AD[[5]][, 1],
  PialFullArea = ADNIAD_mat$ADNI.AD[[6]][, 1],
  WhiteFullArea = ADNIAD_mat$ADNI.AD[[7]][, 1],
  SmoothPialFullArea = ADNIAD_mat$ADNI.AD[[8]][, 1],
  ConvexHullFullArea = ADNIAD_mat$ADNI.AD[[9]][, 1],
  Age = ADNIAD_mat$ADNI.AD[[10]],
  Gender = ADNIAD_unlist[, 19]
))

colnames(ADNIAD_L)[1] <- "SubjID"
colnames(ADNIAD_L)[12] <- "Age"
colnames(ADNIAD_L)[13] <- "Gender"

ADNIAD_R <- data.frame(cbind(
  SubjID = ADNIAD_unlist[, 1],
  hemi = "R",
  diag = "AD",
  AvgCortThickness = ADNIAD_mat$ADNI.AD[[2]][, 2],
  PialArea = ADNIAD_mat$ADNI.AD[[3]][, 2],
  SmoothPialArea = ADNIAD_mat$ADNI.AD[[4]][, 2],
  WhiteArea = ADNIAD_mat$ADNI.AD[[5]][, 2],
  PialFullArea = ADNIAD_mat$ADNI.AD[[6]][, 2],
  WhiteFullArea = ADNIAD_mat$ADNI.AD[[7]][, 2],
  SmoothPialFullArea = ADNIAD_mat$ADNI.AD[[8]][, 2],
  ConvexHullFullArea = ADNIAD_mat$ADNI.AD[[9]][, 2],
  Age = ADNIAD_mat$ADNI.AD[[10]],
  ADNIAD_unlist[, 19]
))

colnames(ADNIAD_R)[1] <- "SubjID"
colnames(ADNIAD_R)[12] <- "Age"
colnames(ADNIAD_R)[13] <- "Gender"

ADNIAD <- as_tibble(full_join(ADNIAD_L, ADNIAD_R))

ADNIControl_L <- data.frame(cbind(
  ADNIControl_unlist[, 1],
  hemi = "L",
  diag = "Control",
  AvgCortThickness = ADNIControl_mat$ADNI.Ctrl[[2]][, 1],
  PialArea = ADNIControl_mat$ADNI.Ctrl[[3]][, 1],
  SmoothPialArea = ADNIControl_mat$ADNI.Ctrl[[4]][, 1],
  WhiteArea = ADNIControl_mat$ADNI.Ctrl[[5]][, 1],
  PialFullArea = ADNIControl_mat$ADNI.Ctrl[[6]][, 1],
  WhiteFullArea = ADNIControl_mat$ADNI.Ctrl[[7]][, 1],
  SmoothPialFullArea = ADNIControl_mat$ADNI.Ctrl[[8]][, 1],
  ConvexHullFullArea = ADNIControl_mat$ADNI.Ctrl[[9]][, 1],
  Age = ADNIControl_mat$ADNI.Ctrl[[10]],
  ADNIControl_unlist[, 19]
))

colnames(ADNIControl_L)[1] <- "SubjID"
colnames(ADNIControl_L)[12] <- "Age"
colnames(ADNIControl_L)[13] <- "Gender"

ADNIControl_R <- data.frame(cbind(
  SubjID = ADNIControl_unlist[, 1],
  hemi = "R",
  diag = "Control",
  AvgCortThickness = ADNIControl_mat$ADNI.Ctrl[[2]][, 2],
  PialArea = ADNIControl_mat$ADNI.Ctrl[[3]][, 2],
  SmoothPialArea = ADNIControl_mat$ADNI.Ctrl[[4]][, 2],
  WhiteArea = ADNIControl_mat$ADNI.Ctrl[[5]][, 2],
  PialFullArea = ADNIControl_mat$ADNI.Ctrl[[6]][, 2],
  WhiteFullArea = ADNIControl_mat$ADNI.Ctrl[[7]][, 2],
  SmoothPialFullArea = ADNIControl_mat$ADNI.Ctrl[[8]][, 2],
  ConvexHullFullArea = ADNIControl_mat$ADNI.Ctrl[[9]][, 2],
  Age = ADNIControl_mat$ADNI.Ctrl[[10]],
  ADNIControl_unlist[, 19]
))

colnames(ADNIControl_R)[1] <- "SubjID"
colnames(ADNIControl_R)[12] <- "Age"
colnames(ADNIControl_R)[13] <- "Gender"

ADNIControl <- as_tibble(full_join(ADNIControl_L, ADNIControl_R))

ADNI <- full_join(ADNIAD, ADNIControl)

ADNI$AvgCortThickness <- as.double(ADNI$AvgCortThickness)
ADNI$PialArea <- as.double(ADNI$PialArea)
ADNI$SmoothPialArea <- as.double(ADNI$SmoothPialArea)
ADNI$WhiteArea <- as.double(ADNI$WhiteArea)
ADNI$PialFullArea <- as.double(ADNI$PialFullArea)
ADNI$WhiteFullArea <- as.double(ADNI$WhiteFullArea)
ADNI$SmoothPialFullArea <- as.double(ADNI$SmoothPialFullArea)
ADNI$ConvexHullFullArea <- as.double(ADNI$ConvexHullFullArea)
ADNI$Age <- as.double(ADNI$Age)

ADNI <- filter(ADNI, AvgCortThickness != 0, !is.na(AvgCortThickness))

ADNI <- ADNI %>%
  mutate(
    ConvexHullArea = ConvexHullFullArea - (SmoothPialFullArea -
                                             SmoothPialArea),
    aqc_date = as_date(str_sub(SubjID, 12, 25)),
    SUBJ = str_sub(SubjID, 1, 10)
  ) 

ADNI <-
  ADNI %>% group_by(SUBJ, hemi) %>% filter(aqc_date == min(aqc_date)) %>% mutate(
    Diagnostic = diag,
    Sample = "ADNI",
    AvgThickness = AvgCortThickness,
    logAvgThickness = log10(AvgCortThickness),
    TotalArea = PialArea,
    logTotalArea = log10(PialArea),
    ExposedArea = SmoothPialArea,
    logExposedArea = log10(SmoothPialArea),
    WhiteSurfArea = WhiteArea,
    logWhiteSurfArea = log10(WhiteArea),
    logConvexHullArea = log10(ConvexHullArea),
    localGI = TotalArea / ExposedArea,
    K = 1 / 2 * logAvgThickness + logTotalArea - 5 / 4 * logExposedArea
  ) %>% dplyr::select(-c(SubjID, diag, AvgCortThickness, PialArea, SmoothPialArea, WhiteArea, PialFullArea, WhiteFullArea, SmoothPialFullArea, ConvexHullFullArea ))


# Age correction ----

# logTotalArea

decay_ADNI_TotalArea <- ADNI  %>%
  group_by(Diagnostic) %>%
  do(fit_decay_ADNI_TotalArea = rlm(logTotalArea ~ Age, data = .))

# get the coefficients por intervalo de mean(Age) ----
decay_ADNI_logTotalArea_Coef = tidy(
  decay_ADNI_TotalArea,
  fit_decay_ADNI_TotalArea,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda----

lambda_At_ADNIAD <-
  decay_ADNI_logTotalArea_Coef$estimate[decay_ADNI_logTotalArea_Coef$Diagnostic == "AD" &
                                       decay_ADNI_logTotalArea_Coef$term == "Age"]

lambda_At_ADNIControl <-
  decay_ADNI_logTotalArea_Coef$estimate[decay_ADNI_logTotalArea_Coef$Diagnostic == "Control" &
                                       decay_ADNI_logTotalArea_Coef$term == "Age"]

b_At_ADNIAD <-
  decay_ADNI_logTotalArea_Coef$estimate[decay_ADNI_logTotalArea_Coef$Diagnostic == "AD" &
                                          decay_ADNI_logTotalArea_Coef$term == "(Intercept)"]

b_At_ADNIControl <-
  decay_ADNI_logTotalArea_Coef$estimate[decay_ADNI_logTotalArea_Coef$Diagnostic == "Control" &
                                          decay_ADNI_logTotalArea_Coef$term == "(Intercept)"]


# logAvgThickness

decay_ADNI_logAvgThickness <- ADNI  %>%
  group_by(Diagnostic) %>%
  do(fit_decay_ADNI_logAvgThickness = rlm(logAvgThickness ~ Age, data = .))

# get the coefficients por intervalo de mean(Age) ----
decay_ADNI_logAvgThickness_Coef = tidy(
  decay_ADNI_logAvgThickness,
  fit_decay_ADNI_logAvgThickness,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda----

lambda_t_ADNIAD <-
  decay_ADNI_logAvgThickness_Coef$estimate[decay_ADNI_logAvgThickness_Coef$Diagnostic == "AD" &
                                           decay_ADNI_logAvgThickness_Coef$term == "Age"]

lambda_t_ADNIControl <-
  decay_ADNI_logAvgThickness_Coef$estimate[decay_ADNI_logAvgThickness_Coef$Diagnostic == "Control" &
                                           decay_ADNI_logAvgThickness_Coef$term == "Age"]

b_t_ADNIAD <-
  decay_ADNI_logAvgThickness_Coef$estimate[decay_ADNI_logAvgThickness_Coef$Diagnostic == "AD" &
                                             decay_ADNI_logAvgThickness_Coef$term == "(Intercept)"]

b_t_ADNIControl <-
  decay_ADNI_logAvgThickness_Coef$estimate[decay_ADNI_logAvgThickness_Coef$Diagnostic == "Control" &
                                             decay_ADNI_logAvgThickness_Coef$term == "(Intercept)"]
# logExposedArea

decay_ADNI_logExposedArea <- ADNI  %>%
  group_by(Diagnostic) %>%
  do(fit_decay_ADNI_logExposedArea = rlm(logExposedArea ~ Age, data = .))

# get the coefficients por intervalo de mean(Age) ----
decay_ADNI_logExposedArea_Coef = tidy(
  decay_ADNI_logExposedArea,
  fit_decay_ADNI_logExposedArea,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda----

lambda_Ae_ADNIAD <-
  decay_ADNI_logExposedArea_Coef$estimate[decay_ADNI_logExposedArea_Coef$Diagnostic == "AD" &
                                          decay_ADNI_logExposedArea_Coef$term == "Age"]

lambda_Ae_ADNIControl <-
  decay_ADNI_logExposedArea_Coef$estimate[decay_ADNI_logExposedArea_Coef$Diagnostic == "Control" &
                                          decay_ADNI_logExposedArea_Coef$term == "Age"]

b_Ae_ADNIAD <-
  decay_ADNI_logExposedArea_Coef$estimate[decay_ADNI_logExposedArea_Coef$Diagnostic == "AD" &
                                            decay_ADNI_logExposedArea_Coef$term == "(Intercept)"]

b_Ae_ADNIControl <-
  decay_ADNI_logExposedArea_Coef$estimate[decay_ADNI_logExposedArea_Coef$Diagnostic == "Control" &
                                            decay_ADNI_logExposedArea_Coef$term == "(Intercept)"]

# ConvexHullArea

decay_ADNI_logConvexHullArea <- ADNI  %>%
  group_by(Diagnostic) %>%
  do(fit_decay_ADNI_logConvexHullArea = rlm(logConvexHullArea ~ Age, data = .))

# get the coefficients por intervalo de mean(Age) ----
decay_ADNI_logConvexHullArea_Coef = tidy(
  decay_ADNI_logConvexHullArea,
  fit_decay_ADNI_logConvexHullArea,
  conf.int = TRUE,
  conf.level = 0.95
)

# lambda----

lambda_CH_ADNIAD <-
  decay_ADNI_logConvexHullArea_Coef$estimate[decay_ADNI_logConvexHullArea_Coef$Diagnostic == "AD" &
                                               decay_ADNI_logConvexHullArea_Coef$term == "Age"]

lambda_CH_ADNIControl <-
  decay_ADNI_logConvexHullArea_Coef$estimate[decay_ADNI_logConvexHullArea_Coef$Diagnostic == "Control" &
                                            decay_ADNI_logConvexHullArea_Coef$term == "Age"]


b_CH_ADNIAD <-
  decay_ADNI_logConvexHullArea_Coef$estimate[decay_ADNI_logConvexHullArea_Coef$Diagnostic == "AD" &
                                               decay_ADNI_logConvexHullArea_Coef$term == "(Intercept)"]

b_CH_ADNIControl <-
  decay_ADNI_logConvexHullArea_Coef$estimate[decay_ADNI_logConvexHullArea_Coef$Diagnostic == "Control" &
                                               decay_ADNI_logConvexHullArea_Coef$term == "(Intercept)"]


# mutate new decayed variables ----

ADNI <-
  ADNI %>% mutate(logAvgThickness_age_decay = as.numeric(
          ifelse(
           Diagnostic == "AD",
            logAvgThickness - b_t_ADNIAD - lambda_t_ADNIAD * Age,
            ifelse(
              Diagnostic == "Control",
              logAvgThickness - b_t_ADNIControl - lambda_t_ADNIControl * Age,
              ""
            )
          )
        ),
    logTotalArea_age_decay = as.numeric(
            ifelse(
              Diagnostic == "AD",
              logTotalArea - b_At_ADNIAD  - lambda_At_ADNIAD * Age,
              ifelse(
                Diagnostic == "Control",
                logTotalArea - b_At_ADNIControl - lambda_At_ADNIControl * Age,
                ""
              )
            )
          ),
    logExposedArea_age_decay = as.numeric(
            ifelse(
              Diagnostic == "AD",
              logExposedArea - b_Ae_ADNIAD - lambda_Ae_ADNIAD * Age,
              ifelse(
                Diagnostic == "Control",
                logExposedArea - b_Ae_ADNIControl - lambda_Ae_ADNIControl * Age,
                ""
              )
            )
          ),
    logConvexHullArea_age_decay = as.numeric(
      ifelse(
        Diagnostic == "AD",
        logConvexHullArea - b_CH_ADNIAD - lambda_CH_ADNIAD * Age,
        ifelse(
          Diagnostic == "Control",
          logConvexHullArea - b_CH_ADNIControl - lambda_CH_ADNIControl * Age,
          ""
        )
      )
    )
        )

# K ----

ADNI <- ADNI %>%
  mutate(
    K_age_decay = (
      logTotalArea_age_decay + (1 / 2) * logAvgThickness_age_decay -
        ((5 / 4) * logExposedArea_age_decay)
    )
  )
