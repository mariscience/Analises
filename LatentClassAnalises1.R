######## Análises Preliminares #########

###### TRADUZIR O SCRIPT ########

setwd("D:/Users/User/Documents")
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

banco<- read_xlsx("BancoAlcool2.xlsx", sheet = 1)
uni1 <- read_xlsx("BancoAlcool2.xlsx", sheet = 2)

banco1 <- subset(banco[c(2:8,22:23)])

uni2 <- bind_cols(banco1,uni)
uni <- uni2

str(uni)
write_xlsx(uni, "TabelaUni.xlsx")

names(uni)

count(uni$Total_Audit_C)
# Transformando em Fatores

names <- c(1,5:11)
uni[,names] <- lapply(uni[,names] , factor)
str(uni)

# Subset das variaveis para o LCA
uni1 <- uni1[,10:12]
names(uni11)
count(uni11$Q1eQ2)
count(uni11$Q3eQ4)
count(uni11$Total_Audit_C)

# Analises poLCA Q1eQ2 e Q3eQ4
f <- cbind(Q1eQ2, Q3eQ4)~1
lca2 <- poLCA(f,uni11, nclass = 1, maxiter = 5000)
lca2 <- poLCA(f,uni11, nclass = 2, maxiter = 5000)
lca3 <- poLCA(f,uni11, nclass = 3, maxiter = 5000)
lca4 <- poLCA(f,uni11, nclass = 4, maxiter = 5000)

# Analises poLCA Q1eQ2 e Q3eQ4 covariando AuditC
f2 <- cbind(Q1eQ2, Q3eQ4)~Total_Audit_C
lcacov1 <- poLCA(f2,uni11, nclass = 1, maxiter = 5000)
lcacov2 <- poLCA(f2,uni11, nclass = 2, maxiter = 5000)
lcacov3 <- poLCA(f2,uni11, nclass = 3, maxiter = 5000)
lcacov4 <- poLCA(f2,uni11, nclass = 4, maxiter = 5000)






