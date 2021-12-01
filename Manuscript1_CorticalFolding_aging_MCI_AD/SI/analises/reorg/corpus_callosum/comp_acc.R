# DADOS + ACC -----

# DADOS ACC PREP

dados_acc$Age_interval[dados_acc$Age >= 0 &
                               dados_acc$Age < 5] <- "00-05"
dados_acc$Age_interval[dados_acc$Age >= 5 &
                               dados_acc$Age < 10] <- "05-10"
dados_acc$Age_interval[dados_acc$Age >= 10 &
                               dados_acc$Age < 15] <- "10-15"
dados_acc$Age_interval[dados_acc$Age >= 15 &
                               dados_acc$Age < 20] <- "15-20"
dados_acc$Age_interval[dados_acc$Age >= 20 &
                               dados_acc$Age < 25] <- "20-25"
dados_acc$Age_interval[dados_acc$Age >= 25 &
                         dados_acc$Age < 30] <- "25-30"
dados_acc$Age_interval[dados_acc$Age >= 30 &
                         dados_acc$Age < 35] <- "30-35"
dados_acc$Age_interval[dados_acc$Age >= 35 &
                         dados_acc$Age < 40] <- "35-40"
dados_acc$Age_interval[dados_acc$Age >= 40 &
                         dados_acc$Age < 45] <- "40-45"

dados_acc$Age_interval10[dados_acc$Age >= 0 &
                         dados_acc$Age < 10] <- "00"
dados_acc$Age_interval10[dados_acc$Age >= 10 &
                         dados_acc$Age < 20] <- "10"
dados_acc$Age_interval10[dados_acc$Age >= 20 &
                         dados_acc$Age < 30] <- "20"
dados_acc$Age_interval10[dados_acc$Age >= 30 &
                         dados_acc$Age < 40] <- "30"
dados_acc$Age_interval10[dados_acc$Age >= 40 &
                         dados_acc$Age < 50] <- "40"

dados_acc$Diagnostic[dados_acc$Diagnostic == "CONTROLE"] <- "CTL"

log_kteorico <- log(sqrt(dados_acc$AvgThickness) * dados_acc$TotalArea) - (log(dados_acc$TotalArea / dados_acc$localGI) * alphateorico)

names(log_kteorico)[1] <- "log_teorico"
dados_acc <-    cbind(dados_acc, log_kteorico)

dados$Diagnostic[dados$Diagnostic == "CONTROLE"] <- "CTL"
dados$Diagnostic[dados$Diagnostic == "CCL"] <- "MCI"

dados <- filter(visit == 1, RM_Maquina == "Philips")
dados <- filter(
  dados,
  Diagnostic == "CTL"|
    Diagnostic == "MCI"|
    Diagnostic == "ALZ")

dados_all <- full_join(dados, dados_acc, by = c("SUBJ", "hemi", "Gender", "Age", "Diagnostic", "Age_interval", "TotalArea", "AvgThickness", "GM_volume", "localGI","Age_interval10", "log_kteorico"))

dados_all <- select(dados, `SUBJ`,`Age`,`Age_interval`, `Diagnostic`, `hemi`, `Gender`, `TotalArea`, `AvgThickness`, `localGI`, `Age_interval10`, `log_kteorico`)

# GRAFICOS ----
ggplot(
  data = filter(dados_all, Diagnostic == "CTL"), aes(
    x = (log(TotalArea / localGI)),
    y = (log(sqrt(AvgThickness) * TotalArea)),
    color = Age_interval10
  )
) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Gyrification through lifespan",
    x = "Log Exposed Area",
    y = "Log T^(1/2)*At",
    colour = "Age interval"
  ) +
  theme_pubclean()

ggplot(
  data = dados_all, aes(
    x = (log(TotalArea / localGI)),
    y = (log(sqrt(AvgThickness) * TotalArea)),
    color = Diagnostic
    
  )
) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Gyrification through lifespan",
    x = "Log Exposed Area",
    y = "Log T^(1/2)*At",
    colour = "Age interval"
  ) +
  theme_pubclean()


# 3 regressoes multiplas para cada grupo
dados_lm_age_diag <- filter(
  dados_all,  Diagnostic == "CTL"|
    Diagnostic == "DCC"
)  %>%
  group_by(Diagnostic, Age_interval10) %>%
  do(fit_age_diag = lm(log(sqrt(AvgThickness) * TotalArea) ~ log(TotalArea/localGI), data = .))

# get the coefficients by group in a tidy data_frame
age_diag_Coef <- tidy(dados_lm_age_diag,
                     fit_age_diag,
                     conf.int = TRUE,
                     conf.level = 0.95)
age_diag_Coef

age_diag_Pred <- augment(dados_lm_age_diag, fit_age_diag)
age_diag_Pred

age_diag_R2 <- glance(dados_lm_age_diag, fit_age_diag)
age_diag_R2

# calcual o chi quadarado das regressoes

dados_age_diag <-
  filter(
    dados_all,  Diagnostic == "CTL"|
      Diagnostic == "DCC"
  )   %>%
  group_by(Diagnostic, Age_interval10) %>%
  do(chit = chisq.test(xtabs(log(sqrt(AvgThickness) * TotalArea) ~ log(TotalArea/localGI), data = .)))

age_diag_Chisq <- tidy(dados_age_diag, chit)
age_diag_Chisq

ggplot(
  data = filter(
    age_diag_Coef,
    term == "log(TotalArea/localGI)"),
  aes(x = Age_interval10, y = estimate, group = Diagnostic)
) +
  geom_point(aes(color = Diagnostic)) +
  geom_line(aes(color = Diagnostic)) +
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low, color = Diagnostic)) +
  geom_hline(yintercept = 1.25, linetype = "dashed") +
  labs(
    title = "Comparando o slope alpha para os diagnosticos",
    x = "Age Interval",
    y = "Slope alpha",
    colour = "Diagnostico"
  ) +
  theme_pubclean()


dados_kteorico_age_diag <-
  filter(
    dados_all,  Diagnostic == "CTL"|
      Diagnostic == "DCC"
  )   %>%
  group_by(Diagnostic, Age_interval10) %>%
  dplyr::summarise(
    avg_log_kteorico = mean(log_kteorico),
    uci_log_kteorico = CI(log_kteorico)[1],
    lci_log_kteorico = CI(log_kteorico)[3]
  ) %>%
  mutate(Age_interval10 = Age_interval10 %>% as.factor())



ggplot(
  data = filter(dados_kteorico_age_diag, Age_interval10 < 50),
  aes(
    x = Age_interval10,
    y = avg_log_kteorico,
    color = Diagnostic,
    group = Diagnostic
  )
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbar(aes(ymax = uci_log_kteorico, ymin = lci_log_kteorico, color = Diagnostic)) +
  labs(
    title = "Comparing Offset log(k) for diseased and healthy subjects",
    subtitle = "Sessiona 1",
    x = "Age range",
    y = "Offset log(k)",
    colour = "Diagnostic"
  ) +
  theme_pubclean()

ggplot(data = filter(dados_all, Diagnostic == "CTL"| Diagnostic == "DCC", Age < 50), mapping = aes(x = Age, y = log_kteorico, color = Diagnostic)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(
    title = "Comparing Offset log(k) for diseased and healthy subjects",
    x = "Age",
    y = "Offset log(k)",
    colour = "Diagnostic"
  ) +
  theme_pubclean()
