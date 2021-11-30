# PREPARACAO ----

### AGE INTERVAL ####

dados$Age_interval <- cut(dados$Age,
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100),
                                       right = FALSE,
                                       include.lowest = TRUE)

dados$Age_interval10 <- cut(dados$Age,
                                         breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                                         right = FALSE,
                                         include.lowest = TRUE)


# decaimento da idade ----
source("analises/decaimento_idade_25.R")

# dados clínicos ---
source("analises/clinic_data.R")
source("analises/alpha_beta.R")

# corpo caloso ---
# source("analises/corpocaloso.R")
# source("analises/corpocaloso_FA.R")
## decaimento idade volume corpo caloso ----

# source("analises/decaimento_idade_WMvolume_CC.R")

## comparar hemisferios ----

# source("analises/comparar_hemisferios.R")