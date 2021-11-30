# funcao decaimento idade ----

decay <- function(dataset, grupo, medida, Age) {
  decay_dataset <- dataset)  %>%
  group_by(grupo) %>%
  do(fit_decay = rlm(medida ~ Age, data = .))

# get the coefficients por intervalo de idade ----
decay_Coef = tidy(decay_dataset,
                  fit_decay,
                  conf.int = TRUE,
                  conf.level = 0.95)
}