###### Validação da CHAOS FORMATIVO #######
### Análises Baseada no site http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/

###### Omiti valores Missing verificar Intputação #########


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

chaosdic <- read_xlsx("ChaosDicProntoForm.xlsx")
str(chaosdic)
colnames(chaosdic)[29] <-"TDAHtotal"
chaosdic[,22:27] <- chaosdic[,22:27] %>% mutate_if(is.character,as.numeric) 
chaosdic <- na.omit(chaosdic)
chaosdicIn <-chaosdic[,7:22]
chaosdicEx <-chaosdic[,c(7:21,24)]
chaosdicTDAH <- chaosdic[,c(7:21,29)]

#  Analise com o Pacote SEMinR ####
##### Dicotomicas ##### 
### Desfecho Internalizante ####
library(seminr)
names(chaosdic)
measurements <- constructs(composite("Chaos", colnames(chaosdicIn)[1:15]),
                           composite("CBCL",colnames(chaosdicIn)[16]))

structure <- relationships(
  paths(from = c("Chaos"), to = "CBCL")
)

# Estimando o Modelo: desfecho Internalizante

pls_model <- estimate_pls(data = chaosdicIn, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings

# Desfecho Externalizante

measurements <- constructs(composite("Chaos", colnames(chaosdicEx)[1:15]),
                           composite("CBCL",colnames(chaosdicEx)[16]))

structure <- relationships(
  paths(from = c("Chaos"), to = "CBCL")
)

# Estimando o Modelo: desfecho Externalizante

pls_model <- estimate_pls(data = chaosdicEx, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings

#  Analise com o Pacote SEMinR ####
##### Dicotomicas ##### 
### Desfecho TDAH ####
measurements <- constructs(composite("Chaos", colnames(chaosdicTDAH)[1:15]),
                           composite("CBCL",colnames(chaosdicTDAH)[16]))

structure <- relationships(
  paths(from = c("Chaos"), to = "CBCL")
)

# Estimando o Modelo: desfecho TDAH

pls_model <- estimate_pls(data = chaosdicTDAH, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings

####################################################
## Dois Fatores Shervey com desfecho Externalizante
# Fator 1 - Questoes 1,2,4,7,12,14,15
# Fator 2 - Questoes 3,5,6,8,9,10,11,13

measurements <- constructs(composite("Chaos1", colnames(chaosdicEx)[c(1,2,4,7,12,14,15)]),
                           composite("Chaos2", colnames(chaosdicEx)[c(3,5,6,8,9,10,11,13)]),
                           composite("CBCL",colnames(chaosdicEx)[16]))

structure <- relationships(
  paths(from = c("Chaos1","Chaos2"), to = "CBCL")
)

# Estimando o Modelo: desfecho Externalizante

pls_model <- estimate_pls(data = chaosdicEx, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)


summary(pls_model)$loadings

######### *******#######
##### Dois fatores Retirando as Cargas Fatoriais Baixas 

measurements <- constructs(composite("Chaos1", colnames(chaosdicEx)[c(2,7,12,14)]),
                           composite("Chaos2", colnames(chaosdicEx)[c(3,5,8,11)]),
                           composite("CBCL",colnames(chaosdicEx)[16]))

structure <- relationships(
  paths(from = c("Chaos1","Chaos2"), to = "CBCL"))

pls_model <- estimate_pls(data = chaosdicEx, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)
summary(pls_model)$loadings

##############################################
## Tres Fatores  com desfecho Externalizante
## Fator 1 - Questões 1,2,4,12,14,15
## Fator 2 - Questões 3,5,6,8,9,11
## Fator 3 - Questões 7,10,13

measurements <- constructs(composite("Chaos1", colnames(chaosdicEx)[c(1,2,4,12,14,15)]),
                           composite("Chaos2", colnames(chaosdicEx)[c(3,5,6,8,9,11)]),
                           composite("Chaos3", colnames(chaosdicEx)[c(7,10,13)]),
                           composite("CBCL",colnames(chaosdicEx)[16]))

structure <- relationships(
  paths(from = c("Chaos1","Chaos2","Chaos3" ), to = "CBCL")
)

# Estimando o Modelo: desfecho Externalizante

pls_model <- estimate_pls(data = chaosdicEx, 
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
