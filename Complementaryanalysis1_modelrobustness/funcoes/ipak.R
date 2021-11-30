ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <-
  c(
    "plyr",
    "dplyr",
    "ggplot2",
    "tidyr",
    "tidyverse",
    "modelr",
    "ggpubr",
    "data.table",
    "jtools",
    "broom",
    "purrr",
    "funModeling",
    "Hmisc",
    "lubridate",
    "ggrepel",
    "magrittr",
    "ggridges",
    "Rmisc",
    "boostrap",
    "WRS2"
  )
ipak(packages)


library("TeachingDemos", lib.loc = "~/R/win-library/3.5")
