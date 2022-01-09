######## Análises Preliminares #########
setwd("D:/Users/User/Documents/")
getwd()

library(pacman)
pacman::p_load(dplyr,plyr,arsenal,gtsummary,tidyverse,openxlsx,arsenal,tibble,readxl,writexl)

banco <- read_xlsx("BancoAlcool2.xlsx", sheet = 1)
uni <- read_xlsx("BancoAlcool2.xlsx", sheet = 2)

View(uni)
str(banco)

# Transformando em Fatores

View(banco)
names <- c(2,6:8,10,12:13,15:16,18:23)
banco[,names] <- lapply(banco[,names] , factor)
str(banco)

# Tabela de Frequencia 1 - Have a drink at home and Gendr
# Banco Alcool
library(questionr)
tab1 <- freq(banco$Heavy_drinker_at_home, sort = "inc",valid = FALSE, total = TRUE)
tab2 <- freq(banco$Gender, sort = "inc",valid = FALSE, total = TRUE)
print(tab1)
print(tab2)
freq(uni$Q1eQ2,sort = "inc",valid = FALSE, total = TRUE)
freq(uni$Q3eQ4, sort = "inc",valid = FALSE, total = TRUE)
freq(uni$Total_Audit_C, sort = "inc",valid = FALSE, total = TRUE)
questionr::freq(banco$Age_years, sort = "inc", total = TRUE)
?freq()


# Pode ser util
library(openxlsx) # loads library and doesn't require Java installed

your_df_list <- c("df1", "df2", ..., "dfn")

for(name in your_df_list){
  write.xlsx(x = get(name), 
             file = "your_spreadsheet_name.xlsx", 
             sheetName = name)
}


# Estatisticas Descritiva de todo o Banco
install.packages('labelled ')
library(Rcmdr)

# https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html
require(knitr)
require(survival)
names(banco)


# continuar o codigo abaixo com todas as variaveis
# colocar no markdown e mostra pra Sabine
count
names(banco)
tab1 <- tableby(~ Gender +
                  Heavy_drinker_at_home+
                  Schooling.Father+
                  Schooling.Mother+
                  Age_years + 
                  AuditC_Q1_B_age+
                  AuditC_Q1_A_firsttime +
                  AuditC_Q2_whooffered + 
                  AuditC_Q3_entireglass +
                  AuditC_Q3_B_Age +
                  AuditC_Q4_whowherewithyou +
                  AuditC_Q5_drunk + 
                  AuditC_Q5_B_age +
                  AuditC_Q6_whowherewithyou+
                  AuditC_Q7_howoften + 
                  AuditC_Q8_howmany + 
                  AuditC_Q9_sixormore +
                  AuditC_Q10_A_4bestfriends +
                AuditC_Q10_B_theirparents,
                data=banco)

tabela1 <- summary(tab1, text=TRUE)
tabela1 <- data.frame(tabela1)

write_xlsx(tabela1, "Tabela1Revista.xlsx")

tab2 <- tableby(Gender ~ 
                  Heavy_drinker_at_home+
                  Schooling.Father+
                  Schooling.Mother+
                  Age_years + 
                  AuditC_Q1_B_age+
                  AuditC_Q1_A_firsttime +
                  AuditC_Q2_whooffered + 
                  AuditC_Q3_entireglass +
                  AuditC_Q3_B_Age +
                  AuditC_Q4_whowherewithyou +
                  AuditC_Q5_drunk + 
                  AuditC_Q5_B_age +
                  AuditC_Q6_whowherewithyou+
                  AuditC_Q7_howoften + 
                  AuditC_Q8_howmany + 
                  AuditC_Q9_sixormore +
                  AuditC_Q10_A_4bestfriends +
                  AuditC_Q10_B_theirparents,
                data=banco)

tabela2 <- summary(tab2, text=TRUE)
tabela2 <- data.frame(tabela2)
write_xlsx(tabela2, "Tabela2.xlsx")
?write_xlsx
summary(tab1, text=TRUE)
library(writexl)

#### Testar
#### http://www.est.ufmg.br/~monitoria/Material/Manual_Rcmdr.pdf

install.packages("seminr")
library(seminr)
