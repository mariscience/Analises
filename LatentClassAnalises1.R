######## Análises Preliminares Questionário Uso de Alcool- Questões Unificadas #########

# Início dia 09/10/2021
# Análises com as questões unidas:
# Q1 e Q2
# Q3 e Q4
# Q7, Q8, Q9 Scores do Audit
# Mudanças no Banco de Dados ver script Manipulação Banco de Dados

###### TRADUZIR O SCRIPT ########

setwd("D:/Users/User/Documents/MEGA/Doutorado/Bancos_de_dados/Alcool")
getwd()

library(pacman)
pacman::p_load(dplyr,
               readxl,
               plyr,
               arsenal,
               tibble,
               tidyverse,
               arsenal,
               tibble,
               writexl,
               poLCA)

alcool<- read_xlsx("BancoAlcool2.xlsx", sheet = 1)
alcooluni <- read_xlsx("BancoAlcool2.xlsx", sheet = 2)

alcool1 <- subset(alcool[c(2:8,22:23)])

alcooluni2 <- bind_cols(alcool1,alcooluni)
alcooluni <- alcooluni2

str(alcooluni)
write_xlsx(alcooluni, "TabelaUni.xlsx")

names(alcooluni)

count(alcooluni$Total_Audit_C)
# Transformando em Fatores

names <- c(1,5:11)
alcooluni[,names] <- lapply(alcooluni[,names] , factor)
str(alcooluni)

# Subset das variaveis para o LCA
alcooluni1 <- alcooluni[,10:12]
names(alcooluni1)
count(alcooluni1$Q1eQ2)
count(alcooluni1$Q3eQ4)
count(alcooluni1$Total_Audit_C)

# Analises poLCA Q1eQ2 e Q3eQ4
f <- cbind(Q1eQ2, Q3eQ4)~1
lca2 <- poLCA(f,alcooluni1, nclass = 1, maxiter = 5000)
lca2 <- poLCA(f,alcooluni1, nclass = 2, maxiter = 5000)
lca3 <- poLCA(f,alcooluni1, nclass = 3, maxiter = 5000)
lca4 <- poLCA(f,alcooluni1, nclass = 4, maxiter = 5000)

# Analises poLCA Q1eQ2 e Q3eQ4 covariando AuditC
f2 <- cbind(Q1eQ2, Q3eQ4)~Total_Audit_C
lcacov1 <- poLCA(f2,alcooluni1, nclass = 1, maxiter = 5000)
lcacov2 <- poLCA(f2,alcooluni1, nclass = 2, maxiter = 5000)
lcacov3 <- poLCA(f2,alcooluni1, nclass = 3, maxiter = 5000)
lcacov4 <- poLCA(f2,alcooluni1, nclass = 4, maxiter = 5000)






