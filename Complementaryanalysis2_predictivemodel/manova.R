# K ----

dados_hemi_v1 %>%
  group_by(Diagnostic) %>%
  summarise(N_SUBJ = n_distinct(SUBJ))

dados_lobos_v1 %>%
  group_by(Diagnostic) %>%
  summarise(N_SUBJ = n_distinct(SUBJ))

a <- aov(K ~ Diagnostic, data = dados_hemi_v1)
summary(a)
TukeyHSD(a)
a.TukeyHSD <- as.data.frame(TukeyHSD(a)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a)$`Diagnostic`))
a.TukeyHSD <- as_tibble(cbind(Contrasts, a.TukeyHSD))
rownames(a.TukeyHSD) <- NULL
colnames(a.TukeyHSD)[1] <- c("Contrast")
a.TukeyHSD <- a.TukeyHSD %>% mutate(ROI = "Hemisphere", variable = "K", agecorrection = "no")

a.F <- aov(K ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "F"))
summary(a.F)
TukeyHSD(a.F)
a.F.TukeyHSD <- as.data.frame(TukeyHSD(a.F)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.F)$`Diagnostic`))
a.F.TukeyHSD <- as_tibble(cbind(Contrasts, a.F.TukeyHSD))
rownames(a.F.TukeyHSD) <- NULL
colnames(a.F.TukeyHSD)[1] <- c("Contrast")
a.F.TukeyHSD <- a.F.TukeyHSD %>% mutate(ROI = "Frontal Lobe", variable = "K", agecorrection = "no")

a.O <- aov(K ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "O"))
summary(a.O)
TukeyHSD(a.O)
a.O.TukeyHSD <- as.data.frame(TukeyHSD(a.O)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.O)$`Diagnostic`))
a.O.TukeyHSD <- as_tibble(cbind(Contrasts, a.O.TukeyHSD))
rownames(a.O.TukeyHSD) <- NULL
colnames(a.O.TukeyHSD)[1] <- c("Contrast")
a.O.TukeyHSD <-a.O.TukeyHSD %>% mutate(ROI = "Occipital Lobe", variable = "K", agecorrection = "no")

a.P <- aov(K ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "P"))
summary(a.P)
TukeyHSD(a.P)
a.P.TukeyHSD <- as.data.frame(TukeyHSD(a.P)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.P)$`Diagnostic`))
a.P.TukeyHSD <- as_tibble(cbind(Contrasts, a.P.TukeyHSD))
rownames(a.P.TukeyHSD) <- NULL
colnames(a.P.TukeyHSD)[1] <- c("Contrast")
a.P.TukeyHSD <- a.P.TukeyHSD %>% mutate(ROI = "Parietal Lobe", variable = "K", agecorrection = "no")

a.T <- aov(K ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "T"))
summary(a.T)
TukeyHSD(a.T)
a.T.TukeyHSD <- as.data.frame(TukeyHSD(a.T)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.T)$`Diagnostic`))
a.T.TukeyHSD <- as_tibble(cbind(Contrasts, a.T.TukeyHSD))
rownames(a.T.TukeyHSD) <- NULL
colnames(a.T.TukeyHSD)[1] <- c("Contrast")
a.T.TukeyHSD <- a.T.TukeyHSD %>% mutate(ROI = "Temporal Lobe", variable = "K", agecorrection = "no")

aov_summary <- full_join(a.TukeyHSD, a.F.TukeyHSD) %>% full_join(a.O.TukeyHSD) %>% full_join(a.P.TukeyHSD) %>% full_join(a.T.TukeyHSD)

# K  age decay ----

a <- aov(K_age_decay ~ Diagnostic, data = dados_hemi_v1)
summary(a)
TukeyHSD(a)
a.TukeyHSD <- as.data.frame(TukeyHSD(a)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a)$`Diagnostic`))
a.TukeyHSD <- as_tibble(cbind(Contrasts, a.TukeyHSD))
rownames(a.TukeyHSD) <- NULL
colnames(a.TukeyHSD)[1] <- c("Contrast")
a.TukeyHSD <- a.TukeyHSD %>% mutate(ROI = "Hemisphere", variable = "K", agecorrection = "yes")

a.F <- aov(K_age_decay ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "F"))
summary(a.F)
TukeyHSD(a.F)
a.F.TukeyHSD <- as.data.frame(TukeyHSD(a.F)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.F)$`Diagnostic`))
a.F.TukeyHSD <- as_tibble(cbind(Contrasts, a.F.TukeyHSD))
rownames(a.F.TukeyHSD) <- NULL
colnames(a.F.TukeyHSD)[1] <- c("Contrast")
a.F.TukeyHSD <- a.F.TukeyHSD %>% mutate(ROI = "Frontal Lobe", variable = "K", agecorrection = "yes")

a.O <- aov(K_age_decay ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "O"))
summary(a.O)
TukeyHSD(a.O)
a.O.TukeyHSD <- as.data.frame(TukeyHSD(a.O)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.O)$`Diagnostic`))
a.O.TukeyHSD <- as_tibble(cbind(Contrasts, a.O.TukeyHSD))
rownames(a.O.TukeyHSD) <- NULL
colnames(a.O.TukeyHSD)[1] <- c("Contrast")
a.O.TukeyHSD <-a.O.TukeyHSD %>% mutate(ROI = "Occipital Lobe", variable = "K", agecorrection = "yes")

a.P <- aov(K_age_decay ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "P"))
summary(a.P)
TukeyHSD(a.P)
a.P.TukeyHSD <- as.data.frame(TukeyHSD(a.P)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.P)$`Diagnostic`))
a.P.TukeyHSD <- as_tibble(cbind(Contrasts, a.P.TukeyHSD))
rownames(a.P.TukeyHSD) <- NULL
colnames(a.P.TukeyHSD)[1] <- c("Contrast")
a.P.TukeyHSD <- a.P.TukeyHSD %>% mutate(ROI = "Parietal Lobe", variable = "K", agecorrection = "yes")

a.T <- aov(K_age_decay ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "T"))
summary(a.T)
TukeyHSD(a.T)
a.T.TukeyHSD <- as.data.frame(TukeyHSD(a.T)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.T)$`Diagnostic`))
a.T.TukeyHSD <- as_tibble(cbind(Contrasts, a.T.TukeyHSD))
rownames(a.T.TukeyHSD) <- NULL
colnames(a.T.TukeyHSD)[1] <- c("Contrast")
a.T.TukeyHSD <- a.T.TukeyHSD %>% mutate(ROI = "Temporal Lobe", variable = "K", agecorrection = "yes")

aov_summary <- full_join(aov_summary, a.TukeyHSD) %>% full_join(a.F.TukeyHSD) %>% full_join(a.O.TukeyHSD) %>% full_join(a.P.TukeyHSD) %>% full_join(a.T.TukeyHSD)

# logAvgThickness ----

a <- aov(logAvgThickness ~ Diagnostic, data = dados_hemi_v1)
summary(a)
a.TukeyHSD <- as.data.frame(TukeyHSD(a)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a)$`Diagnostic`))
a.TukeyHSD <- as_tibble(cbind(Contrasts, a.TukeyHSD))
rownames(a.TukeyHSD) <- NULL
colnames(a.TukeyHSD)[1] <- c("Contrast")
a.TukeyHSD <- a.TukeyHSD %>% mutate(ROI = "Hemisphere", variable = "T", agecorrection = "no")

a.F <- aov(logAvgThickness ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "F"))
summary(a.F)
a.F.TukeyHSD <- as.data.frame(TukeyHSD(a.F)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.F)$`Diagnostic`))
a.F.TukeyHSD <- as_tibble(cbind(Contrasts, a.F.TukeyHSD))
rownames(a.F.TukeyHSD) <- NULL
colnames(a.F.TukeyHSD)[1] <- c("Contrast")
a.F.TukeyHSD <- a.F.TukeyHSD %>% mutate(ROI = "Frontal Lobe", variable = "T", agecorrection = "no")

a.O <- aov(logAvgThickness ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "O"))
summary(a.O)
a.O.TukeyHSD <- as.data.frame(TukeyHSD(a.O)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.O)$`Diagnostic`))
a.O.TukeyHSD <- as_tibble(cbind(Contrasts, a.O.TukeyHSD))
rownames(a.O.TukeyHSD) <- NULL
colnames(a.O.TukeyHSD)[1] <- c("Contrast")
a.O.TukeyHSD <-a.O.TukeyHSD %>% mutate(ROI = "Occipital Lobe", variable = "T", agecorrection = "no")

a.P <- aov(logAvgThickness ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "P"))
summary(a.P)
a.P.TukeyHSD <- as.data.frame(TukeyHSD(a.P)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.P)$`Diagnostic`))
a.P.TukeyHSD <- as_tibble(cbind(Contrasts, a.P.TukeyHSD))
rownames(a.P.TukeyHSD) <- NULL
colnames(a.P.TukeyHSD)[1] <- c("Contrast")
a.P.TukeyHSD <- a.P.TukeyHSD %>% mutate(ROI = "Parietal Lobe", variable = "T", agecorrection = "no")

a.T <- aov(logAvgThickness ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "T"))
summary(a.T)
a.T.TukeyHSD <- as.data.frame(TukeyHSD(a.T)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.T)$`Diagnostic`))
a.T.TukeyHSD <- as_tibble(cbind(Contrasts, a.T.TukeyHSD))
rownames(a.T.TukeyHSD) <- NULL
colnames(a.T.TukeyHSD)[1] <- c("Contrast")
a.T.TukeyHSD <- a.T.TukeyHSD %>% mutate(ROI = "Temporal Lobe", variable = "T", agecorrection = "no")

aov_summary <- full_join(aov_summary, a.TukeyHSD) %>% full_join(a.F.TukeyHSD) %>% full_join(a.O.TukeyHSD) %>% full_join(a.P.TukeyHSD) %>% full_join(a.T.TukeyHSD)

# logAvgThickness  age decay ----

a <- aov(logAvgThickness_age_decay ~ Diagnostic, data = dados_hemi_v1)
summary(a)
a.TukeyHSD <- as.data.frame(TukeyHSD(a)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a)$`Diagnostic`))
a.TukeyHSD <- as_tibble(cbind(Contrasts, a.TukeyHSD))
rownames(a.TukeyHSD) <- NULL
colnames(a.TukeyHSD)[1] <- c("Contrast")
a.TukeyHSD <- a.TukeyHSD %>% mutate(ROI = "Hemisphere", variable = "T", agecorrection = "yes")

a.F <- aov(logAvgThickness_age_decay ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "F"))
summary(a.F)
a.F.TukeyHSD <- as.data.frame(TukeyHSD(a.F)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.F)$`Diagnostic`))
a.F.TukeyHSD <- as_tibble(cbind(Contrasts, a.F.TukeyHSD))
rownames(a.F.TukeyHSD) <- NULL
colnames(a.F.TukeyHSD)[1] <- c("Contrast")
a.F.TukeyHSD <- a.F.TukeyHSD %>% mutate(ROI = "Frontal Lobe", variable = "T", agecorrection = "yes")

a.O <- aov(logAvgThickness_age_decay ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "O"))
summary(a.O)
a.O.TukeyHSD <- as.data.frame(TukeyHSD(a.O)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.O)$`Diagnostic`))
a.O.TukeyHSD <- as_tibble(cbind(Contrasts, a.O.TukeyHSD))
rownames(a.O.TukeyHSD) <- NULL
colnames(a.O.TukeyHSD)[1] <- c("Contrast")
a.O.TukeyHSD <-a.O.TukeyHSD %>% mutate(ROI = "Occipital Lobe", variable = "T", agecorrection = "yes")

a.P <- aov(logAvgThickness_age_decay ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "P"))
summary(a.P)
a.P.TukeyHSD <- as.data.frame(TukeyHSD(a.P)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.P)$`Diagnostic`))
a.P.TukeyHSD <- as_tibble(cbind(Contrasts, a.P.TukeyHSD))
rownames(a.P.TukeyHSD) <- NULL
colnames(a.P.TukeyHSD)[1] <- c("Contrast")
a.P.TukeyHSD <- a.P.TukeyHSD %>% mutate(ROI = "Parietal Lobe", variable = "T", agecorrection = "yes")

a.T <- aov(logAvgThickness_age_decay ~ Diagnostic, data = filter(dados_lobos_v1, ROI == "T"))
summary(a.T)
a.T.TukeyHSD <- as.data.frame(TukeyHSD(a.T)$`Diagnostic`)
Contrasts <- as.data.frame(rownames(TukeyHSD(a.T)$`Diagnostic`))
a.T.TukeyHSD <- as_tibble(cbind(Contrasts, a.T.TukeyHSD))
rownames(a.T.TukeyHSD) <- NULL
colnames(a.T.TukeyHSD)[1] <- c("Contrast")
a.T.TukeyHSD <- a.T.TukeyHSD %>% mutate(ROI = "Temporal Lobe", variable = "T", agecorrection = "yes")

aov_summary <- full_join(aov_summary, a.TukeyHSD) %>% full_join(a.F.TukeyHSD) %>% full_join(a.O.TukeyHSD) %>% full_join(a.P.TukeyHSD) %>% full_join(a.T.TukeyHSD)

agecorrection <- c(
  "no" = "No age correction",
  "yes" = "After age correction"
)

ggplot(data = filter(aov_summary, `p adj` < 0.05 | `p adj` == 0.05), aes(
  x = reorder(ROI, desc(ROI)),
  y = diff,
  ymin = lwr,
  ymax = upr, color = Contrast)) +
  geom_hline(yintercept = 0,
             linetype = "11",
             colour = "grey60") +
  geom_pointrange( position = position_dodge(width = 0.3)) +
  #  geom_text(aes(label = str_c("p adj = ", signif(`p adj`, digits = 2))), nudge_x = 0.3) +
  coord_flip() + 
  labs(y =  "Differences in mean levels of Diagnostic", x = "ROI") + facet_grid(variable ~ agecorrection, labeller = labeller(agecorrection.1 = agecorrection)) +
  theme_pubr() + 
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10), text = element_text(size = 10))

aov_summary[order(aov_summary$diff, decreasing = TRUE)),]

aov_summary %>% order(diff, decreasing = TRUE) %>% kable(digits = 3) %>% kable_styling()
# aov_summary <- aov_summary %>% mutate(p.adjust.bonferroni = p.adjust(`p adj`, method = "bonferroni")