###### Validação FORMATIVO #######

setwd("D:/Users/User/Documents")
getwd()

if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman:: p_load(dplyr,
                readxl,
                xlsx,
                arsenal,
                tibble,
                tidyverse,
                tidySEM,
                psych,
                ggpubr,
                rstatix,
                stats)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

bancodic <- read_xlsx("bancodicProntoForm.xlsx")
bancodic <- bancodic[,7:21]
bancodic<- na.omit(bancodic)
bancodic <- bancodic %>% mutate_if(is.numeric, as.factor)

MCA(bancodic, ncp = 5, graph = TRUE)
res.mca <- MCA(bancodic, graph = FALSE)
get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
res = dimdesc(res.mca, axes=1:2, proba=0.05)
var <- get_mca_var(res.mca)
var


# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

