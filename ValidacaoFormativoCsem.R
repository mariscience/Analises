setwd("D:/Users/User/Documents/MEGA/Doutorado/ValidacaoCHAOS/Dicotomico/AnalisesDefinitivasR")
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
chaosdic[,22:27] <- chaosdic[,22:27] %>% mutate_if(is.character,as.numeric) 

chaosdic <- chaosdic[complete.cases(chaosdic),]
install.packages("cSEM")
library(cSEM)

colnames(chaosdic) <- str_replace_all(colnames(chaosdic), "[.]", "-")
  
model<-"
Chaos1 <~ TF_Q1 + TF_Q2 + TF_Q4 + TF_Q7 + TF_Q12 + TF_Q1 + TF_Q15
Chaos2 <~ TF_Q3 + TF_Q5 + TF_Q6 + TF_Q8 + TF_Q9 + TF_Q10 + TF_Q11 + TF_Q13

Chaos1 ~~ Chaos2

"
res <- csem(.data = chaosdicEx, .model = model, .PLS_weight_scheme_inner = "factorial")
summarize(res)
verify(res)
testOMF(res)
assess(res)
b <- resamplecSEMResults(res)
summarize(b)

## Dois Fatores Shervey com desfecho Externalizante
# Fator 1 - Questoes 1,2,4,7,12,14,15
# Fator 2 - Questoes 3,5,6,8,9,10,11,13


