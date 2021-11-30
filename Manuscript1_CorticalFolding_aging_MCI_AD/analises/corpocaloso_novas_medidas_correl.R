#### DESVIO INTERHEMISFERICO, COMPRIMENTO AXONAL MEDIO PONDERADO  e FRACAO CALOSA####

dados <-
  dados %>% mutate(
    left_minus_right = K.L - K.R,
    interhemispheric_deviation = abs((K.L - K.R) / (
      var(K.L, na.rm = TRUE) + var(K.R, na.rm = TRUE) - 2 *
        cov(K.L, K.R, use = "complete")
    ) ^ (1 / 2)) ,
    l = WM_volume_CC / TotalArea_CC,
    fC = WhiteSurfArea / TotalArea_CC,
    left_minus_right_agedecay = K_age_decay.L - K_age_decay.R,
    interhemispheric_deviation_agedecay = abs((K_age_decay.L - K_age_decay.R) / (
      var(K_age_decay.L, na.rm = TRUE) + var(K_age_decay.R, na.rm = TRUE) - 2 *
        cov(K_age_decay.L, K_age_decay.R, use = "complete")
    ) ^ (1 / 2)) ,
    l_agedecay = WM_volume_CC_age_decay / TotalArea_age_decay_CC,
    fC_agedecay = WhiteSurfArea_age_decay / TotalArea_age_decay_CC,
    logfC_agedecay = logWhiteSurfArea_age_decay / logTotalArea_age_decay_CC,
    l_eTIV = (WM_volume_CC/eTIV) / (TotalArea_CC/(eTIV^(2/3))),
    fC_eTIV = (WhiteSurfArea/(eTIV^(2/3))) / (TotalArea_CC/(eTIV^(2/3)))
  )