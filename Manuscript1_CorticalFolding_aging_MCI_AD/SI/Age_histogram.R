dados_datasetscomp %>%
  filter(ROI == "hemisphere", Diagnostic == "CTL") %>%
  dplyr::select(c(SUBJ,Age, Sample)) %>%
  unique() %>%
  ggplot(data =. ,aes(Age, color =Sample, fill = Sample, alpha = 0.4))  +
  geom_histogram(binwidth = 1) +
  theme_pubr() + guides(alpha = FALSE) +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    text = element_text(size = 10)
  ) 


dados_datasetscomp %>%
  filter(ROI == "hemisphere", Diagnostic == "CTL", Age < 20) %>%
  dplyr::select(c(SUBJ,Age, Sample)) %>%
  unique() %>%
  ggplot(data =. ,aes(Age, color =Sample, fill = Sample, alpha = 0.4))  +
  geom_histogram(binwidth = 1) +
  theme_pubr() + guides(alpha = FALSE) +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 20, by = 1))