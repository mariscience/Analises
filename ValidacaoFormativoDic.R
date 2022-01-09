###### Validação FORMATIVO SeminR #######

setwd("D:/Users/User/Documents/MEGA/Doutorado/ValidacaoCHAOS/Dicotomico")
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
                psych,
                ggpubr,
                rstatix,
                stats,
                semPlot,
                seminr)

bancodic <- read_xlsx("bancodicProntoForm.xlsx")
str(bancodic)
colnames(bancodic)[29] <-"TDAHtotal"
bancodic[,22:27] <- bancodic[,22:27] %>% mutate_if(is.character,as.numeric) 
bancodic <- na.omit(bancodic)
bancodicIn <-bancodic[,7:22]
bancodicEx <-bancodic[,c(7:21,24)]
bancodicTDAH <- bancodic[,c(7:21,29)]

#  Analise com o Pacote SEMinR ####
##### Dicotomicas ##### 
### Desfecho Internalizante ####
library(seminr)
names(bancodic)
measurements <- constructs(composite("Chaos", colnames(bancodicIn)[1:15]),
                           composite("CBCL",colnames(bancodicIn)[16]))

structure <- relationships(
  paths(from = c("Chaos"), to = "CBCL")
)

# Estimando o Modelo: desfecho Internalizante

pls_model <- estimate_pls(data = bancodicIn, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings

# Desfecho Externalizante

measurements <- constructs(composite("Chaos", colnames(bancodicEx)[1:15]),
                           composite("CBCL",colnames(bancodicEx)[16]))

structure <- relationships(
  paths(from = c("Chaos"), to = "CBCL")
)

# Estimando o Modelo: desfecho Externalizante

pls_model <- estimate_pls(data = bancodicEx, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings

#  Analise com o Pacote SEMinR ####
##### Dicotomicas ##### 
### Desfecho TDAH ####
measurements <- constructs(composite("Chaos", colnames(bancodicTDAH)[1:15]),
                           composite("CBCL",colnames(bancodicTDAH)[16]))

structure <- relationships(
  paths(from = c("Chaos"), to = "CBCL")
)

# Estimando o Modelo: desfecho TDAH

pls_model <- estimate_pls(data = bancodicTDAH, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings

####################################################
## Dois Fatores Shervey com desfecho Externalizante
# Fator 1 - Questoes 1,2,4,7,12,14,15
# Fator 2 - Questoes 3,5,6,8,9,10,11,13

measurements <- constructs(composite("Chaos1", colnames(bancodicEx)[c(1,2,4,7,12,14,15)]),
                           composite("Chaos2", colnames(bancodicEx)[c(3,5,6,8,9,10,11,13)]),
                           composite("CBCL",colnames(bancodicEx)[16]))

structure <- relationships(
  paths(from = c("Chaos1","Chaos2"), to = "CBCL")
)

# Estimando o Modelo: desfecho Externalizante

pls_model <- estimate_pls(data = bancodicEx, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings

######### *******#######
##### Dois fatores Retirando as Cargas Fatoriais Baixas 

measurements <- constructs(composite("Chaos1", colnames(bancodicEx)[c(2,7,12,14)]),
                           composite("Chaos2", colnames(bancodicEx)[c(3,5,8,11)]),
                           composite("CBCL",colnames(bancodicEx)[16]))

structure <- relationships(
  paths(from = c("Chaos1","Chaos2"), to = "CBCL"))

pls_model <- estimate_pls(data = bancodicEx, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)
summary(pls_model)$loadings

##############################################
## Tres Fatores  com desfecho Externalizante
## Fator 1 - Questões 1,2,4,12,14,15
## Fator 2 - Questões 3,5,6,8,9,11
## Fator 3 - Questões 7,10,13

measurements <- constructs(composite("Chaos1", colnames(bancodicEx)[c(1,2,4,12,14,15)]),
                           composite("Chaos2", colnames(bancodicEx)[c(3,5,6,8,9,11)]),
                           composite("Chaos3", colnames(bancodicEx)[c(7,10,13)]),
                           composite("CBCL",colnames(bancodicEx)[16]))

structure <- relationships(
  paths(from = c("Chaos1","Chaos2","Chaos3" ), to = "CBCL")
)

# Estimando o Modelo: desfecho Externalizante

pls_model <- estimate_pls(data = bancodicEx, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings



# Salvando Output no word 
write2word(resultado1, "D:/Users/User/Documents/MEGA/Doutorado/CHAOS/ValidacaoCHAOS/ModeloLavaan.doc", title="ModeloLavan")


# Grafico de Um fator dicotômica
# graph_sem(model = fitdic)

semPaths(fitdic, "std")

# ordered = "TF_Q1","TF_Q2","TF_Q3","TF_Q4","TF_Q5","TF_Q6","TF_Q7","TF_Q7","TF_Q8","TF_Q9","TF_Q10","TF_Q11","TF_Q12","TF_Q13","TF_Q14","TF_Q15")
