setwd("D:/Users/User/Documents")
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
bancodic[,22:27] <- bancodic[,22:27] %>% mutate_if(is.character,as.numeric) 

bancodic <- bancodic[complete.cases(bancodic),]
install.packages("cSEM")
library(cSEM)

colnames(bancodic) <- str_replace_all(colnames(bancodic), "[.]", "-")
  
model<-"
Chaos1 <~ TF_Q1 + TF_Q2 + TF_Q4 + TF_Q7 + TF_Q12 + TF_Q1 + TF_Q15
Chaos2 <~ TF_Q3 + TF_Q5 + TF_Q6 + TF_Q8 + TF_Q9 + TF_Q10 + TF_Q11 + TF_Q13

Chaos1 ~~ Chaos2

"
res <- csem(.data = bancodicEx, .model = model, .PLS_weight_scheme_inner = "factorial")
summarize(res)
verify(res)
testOMF(res)
assess(res)
b <- resamplecSEMResults(res)
summarize(b)
