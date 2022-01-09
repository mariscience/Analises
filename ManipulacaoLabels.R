######## Análises Preliminares Questionário Uso de Alcool- Auditi-C Modificado #########

# Início dia 28/04/2021
# Término dia

# Mudanças no Banco de Dados:
# Foram selecionadas somente as variáveis relativo ao uso de alcool
# para manipulações dessas variáveis

# A variavel AudticC firsttime foi dividida em 
# AuditC_Q1_A_firsttime e AuditC_Q1_B_age, pois a idade estava juntamente
# com as respostas binarias de yes ou no

# Na variável AuditC_Q1_B_age (a) havia 5 observações com resposta "No especific
# age". Essas respostas foram imputadas pela média de idade que (10,72 arredondada
# para 11).
# Essas alterações foram feitas no Excel e são de
# números 6,113,135,138,185 no excel BancoAlcool1.

# O mesmo procedimento acima foi feito na Variavel AuditC_Q3_entireglass
# foi criada a variavel AuditC_Q3_B_Age. Algumas respostas
# continha somente YES sem idade então foi imputado pela média das
# idades contidas no banco (13,1 arredondado para 13)
# as observações são A115,A153,A156,A186 da coluna ID

# Foi feito filtro a partir da variaveis AuditC_Q1_A_firsttime NO e criada
# o nivel de mensuração NuncaBebeuNada em todas as outras variveis ordinais

# Foi feito filtro a partir da variaveis AuditC_Q3_entireglass NO e criada
# o nivel de mensuração NaoBebeuDoseIn em todas as outras variveis ordinais

# A variavel AuditC 5 foi quebrada em duas e surgiu também a AuditC 5 B 
# "a" foi codificado como 99

# Dia 29/06/2021 foi inserida as variaveis escolaridade materna e paterna

###### TRADUZIR O SCRIPT ########
setwd("D:/Users/User/Documents/MEGA/Doutorado/Bancos_de_dados/Alcool")
getwd()

library(pacman)
pacman::p_load(dplyr,readxl,plyr,arsenal,tibble,gtsummary,tidyverse,arsenal,tibble,writexl)


alcool <- read_xlsx("BancoAlcool1.xlsx", sheet = 1)
View(alcool)


# Limpando os seguintes valores missing por linha:
# A138 A143,A176,A252,A267,A282,A283,A288,289

alcool <- alcool[-c(138,143,176,252,267,282,283,288,289),]
View(alcool)

# Verificando as estruturas das variáveis
str(alcool)

# Excluindo School
alcool[,6] <- NULL
alcool[,24] <- NULL

#Renomeando as Variaveis

colnames(alcool)[1] <-"ID"
colnames(alcool)[2] <-"Heavy_drinker_at_home"
colnames(alcool)[3] <-"Age_years"
colnames(alcool)[4] <-"Schooling.Father"
colnames(alcool)[5] <-"Schooling.Mother"
colnames(alcool)[6] <-"Type_of_School"
colnames(alcool)[7] <-"Gender"
colnames(alcool)[8] <-"Guardian"
colnames(alcool)[9] <-"How_Often"
colnames(alcool)[10] <-"AuditC_Q1_A_firsttime"
colnames(alcool)[11] <-"AuditC_Q1_B_age"
colnames(alcool)[12] <-"AuditC_Q2_whooffered"
colnames(alcool)[13] <-"AuditC_Q3_entireglass"
colnames(alcool)[14] <-"AuditC_Q3_B_Age"
colnames(alcool)[15] <-"AuditC_Q4_whowherewithyou"
colnames(alcool)[16] <-"AuditC_Q5_drunk"
colnames(alcool)[17] <-"AuditC_Q5_B_age"
colnames(alcool)[18] <-"AuditC_Q6_whowherewithyou"
colnames(alcool)[19] <-"AuditC_Q7_howoften"
colnames(alcool)[20] <-"AuditC_Q8_howmany"
colnames(alcool)[21] <-"AuditC_Q9_sixormore"
colnames(alcool)[22] <-"AuditC_Q10_A_4bestfriends"
colnames(alcool)[23] <-"AuditC_Q10_B_theirparents"

View(alcool)
str(alcool)
names(alcool)

# Q5 ficou bebado:colocado numero 99 para fazer a filtragem de quem colocou idade


######## Análises Preliminares Questionário Uso de Alcool- Auditi-C Modificado #########

# Início dia 21/05/2021
# Término dia

# Mudanças no Banco de Dados ver script Manipulação Banco de Dados

#######Mudando Para fator algumas colunas#####


# Gender
alcool$Gender <- ifelse(alcool$Gender == 1, "Female", "Male")

# Q2
count(alcool$AuditC_Q2_whooffered)
alcool$AuditC_Q2_whooffered <- factor(alcool$AuditC_Q2_whooffered)
alcool$AuditC_Q2_whooffered <- revalue(alcool$AuditC_Q2_whooffered, c("a" = "Sozinho",
                                                                      "b" = "Familiares",
                                                                      "c" = "Familiares",
                                                                      "d" = "Familiares",
                                                                      "e" = "Amigos", 
                                                                      "f" = "Outros",
                                                                      "g" = "Outros",

                                                                                                                                            "NuncaBebeuNada" = "NuncaBebeuNada"))
count(alcool$AuditC_Q2_whooffered)

#Q6
count(alcool$AuditC_Q6_whowherewithyou)
alcool$AuditC_Q6_whowherewithyou <- revalue(alcool$AuditC_Q6_whowherewithyou, 
                                                                    c("a" = "Sozinho",
                                                                      "b" = "Familiares",
                                                                      "c" = "Familiares",
                                                                      "d" = "Familiares",
                                                                      "e" = "Amigos", 
                                                                      "Abstemico" = "Abstemico"))
#Q4
count(alcool$AuditC_Q4_whowherewithyou)
alcool$AuditC_Q4_whowherewithyou <- revalue(alcool$AuditC_Q4_whowherewithyou,
                                                                                c("a" = "Sozinho",
                                                                                   "b" = "Familiares",
                                                                                   "c" = "Familiares",
                                                                                   "d" = "Familiares",
                                                                                   "e" = "Amigos",
                                                                                   "Abstemico" = "Abstemico"))

View(alcool)

#Q7

alcool$AuditC_Q7_howoften <- revalue(alcool$AuditC_Q7_howoften, c("a" = "Nunca",
                                                                   "b" = "Mensalmente ou Menos",
                                                                  "c" = "De 2 a 4 vezes por mês",
                                                              "d" = "De 2 a 3 vezes por semana"))


?count
View(alcool)
#Q8
str(alcool$AuditC_Q8_howmany)
count(alcool$AuditC_Q8_howmany)
alcool$AuditC_Q8_howmany <- revalue(alcool$AuditC_Q8_howmany, c("0" = "EPG",
                                                                "0.5" = "EPG",
                                                                "1" = "De uma a 2 Doses",
                                                                "1.5" = "De uma a 2 Doses",
                                                                "1/2 ou 1" = "De uma a 2 Doses",
                                                                "2" = "De uma a 2 Doses",
                                                                "3" = "Mais de 3 Doses",
                                                                "4" = "Mais de 3 Doses",
                                                                "5" = "Mais de 3 Doses",
                                                                "10" = "Mais de 3 Doses"))




# alcool %>% count(AuditC_Q8_howmany,sort = TRUE)
# Q9
count(alcool$AuditC_Q9_sixormore)
alcool$AuditC_Q9_sixormore <- revalue(alcool$AuditC_Q9_sixormore, c("a" = "Nunca",
                                                                    "b" = "Menos que 1 vez ao mês",
                                                                    "c" = "Mensalmente",
                                                                    "d" = "Semanalmente"))
count(alcool$AuditC_Q9_sixormore)
View(alcool)

# Q10A
count(alcool$AuditC_Q10_A_4bestfriends)
str(alcool$AuditC_Q10_A_4bestfriends)
alcool$AuditC_Q10_A_4bestfriends <- revalue(alcool$AuditC_Q10_A_4bestfriends, c("0" ="Nenhum",
                                                                                "-" ="Um",
                                                                                "1" ="Um",
                                                                                "2" = "Dois",
                                                                                "3" = "Três",
                                                                                "4" = "Quatro ou Mais",
                                                                                "4+" = "Quatro ou Mais"))

# Q10B
str(alcool$AuditC_Q10_B_theirparents)
View(alcool)
count(alcool$AuditC_Q10_B_theirparents)
alcool[c(22,31),23]<- NA
alcool$AuditC_Q10_B_theirparents <- revalue(alcool$AuditC_Q10_B_theirparents,c("NS" = "Não Sabe"))

# Banco Gerado dia 26/07/2021
write_xlsx(alcool,"BancoAlcool2.xlsx")








