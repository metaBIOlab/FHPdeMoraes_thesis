# 3 regressoes multiplas para cada grupo
dados_lm_age_diag <- dados  %>%
  group_by(Diagnostic, Age_interval10, ROI) %>%
  do(fit_age_diag = lm(log(sqrt(AvgThickness) * TotalArea_corrected) ~ log(SmoothArea_corrected), data = .))

# get the coefficients by group in a tidy data_frame
age_diag_Coef = tidy(dados_lm_age_diag,
                     fit_age_diag,
                     conf.int = TRUE,
                     conf.level = 0.95)
age_diag_Coef

age_diag_Pred = augment(dados_lm_age_diag, fit_age_diag)
age_diag_Pred

age_diag_R2 = glance(dados_lm_age_diag, fit_age_diag)
age_diag_R2

# calcual o chi quadarado das regressoes

dados_age_diag <- dados %>%
  group_by(Diagnostic, Age_interval10, ROI) %>%
  do(chit = chisq.test(xtabs(log(sqrt(AvgThickness) * TotalArea) ~ log(TotalArea/localGI), data = .)))

age_diag_Chisq = tidy(dados_age_diag, chit)
age_diag_Chisq

# # separa as colunas
# dados_lm_age_diag %>%
#   unnest(fit_age_diag)
#
# dados_age_diag %>%
#   unnest(chit)


