### Validação Definitiva Modelo 1 #####

setwd("D:/Users/User/Documents/MEGA/Doutorado/CHAOS/ValidacaoCHAOS/AnalisesDefinitivasR")
getwd()

if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman:: p_load(dplyr,
                readxl,
                xlsx,
                arsenal,
                tibble,
                corrplot,
                tidyverse,
                tidySEM,
                ggplot2,
                knitr,
                MVN,
                psych,
                ggpubr,
                rstatix,
                stats)

chaosdic <- read_xlsx("ChaosDicPronto.xlsx")
