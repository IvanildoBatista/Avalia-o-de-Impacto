library(readxl)
library(tidyverse)
library(dplyr)
library(lfe)
library(randomizr)
library(RCT)
library(tidyr)
library(xtable)
library(stargazer)
library(rdpower)
library(psych)
library(rdrobust)
library(rdd)
library(data.table)
library(broom)
#install.packages('MatchIt')
library(rddensity)
library(MatchIt)

#Definindo o diretório ----
setwd('C:\\Users\\Ivanildo Batista\\Desktop\\Cursos FGV\\Avaliação de Impacto')

#Lendo a base de dados ----
df <- read.delim('base_psm_educ.csv', sep = ',')
summary(df$T)

#Teste de balanceamento
df_balance <- df %>% 
  select(nota_ideb2009, p_exp_prof2009, p_meninas2009, alunos2009, y2009, T)

teste_bal <- balance_table(df_balance, "T")
teste_bal
#resultado: deu que as diferenças de médias são estatisticamente diferentes de zero

#Realizando o pareamento
#Calculando o primeiro estágio

pscore_fs <- glm(T ~ nota_ideb2009 + p_exp_prof2009+ p_meninas2009+ 
                   alunos2009+ y2009+ factor(id_mun), family=binomial(link=logit),
                 data=df)

summary(pscore_fs)
#criar uma variavel com o score de propensão
df<- df %>% mutate(pscore = predict(pscore_fs, type='response'))


#Usando o pacote MatchIt
#Modelo 1: pareamento por vizinho mais próximo
psm1 <- matchit(T ~ nota_ideb2009 + p_exp_prof2009+ p_meninas2009+ 
                  alunos2009+ y2009+ factor(id_mun),
                method = "nearest",
                data=df, 
                ratio = 1, #1 só vizinho
                discard='both')

summary(psm1)
plot(psm1, type='hist')
df_matched1 <- match.data(psm1)

#Modelo 2: pareamento por vizinho mais próximo com caliper de 0.05
psm2 <- matchit(T ~ nota_ideb2009 + p_exp_prof2009+ p_meninas2009+ 
                  alunos2009+ y2009+ factor(id_mun),
                method = "nearest",
                data=df, caliper = 0.05,
                ratio = 1, #1 só vizinho
                discard='both')

summary(psm2)
plot(psm2, type='hist')
df_matched2 <- match.data(psm2)

#Modelo 3: pareamento por vizinho mais próximo com caliper de 0.005
psm3 <- matchit(T ~ nota_ideb2009 + p_exp_prof2009+ p_meninas2009+ 
                  alunos2009+ y2009+ factor(id_mun),
                method = "nearest",
                data=df, caliper = 0.005,
                ratio = 1, #1 só vizinho
                discard='both')

summary(psm3)
plot(psm3, type='hist')
df_matched3 <- match.data(psm3)


#Teste de balanceamento pós pareamento
df_balance1 <- df_matched1 %>% 
  select(nota_ideb2009, p_exp_prof2009, p_meninas2009, alunos2009, y2009, T)

df_balance2 <- df_matched2 %>% 
  select(nota_ideb2009, p_exp_prof2009, p_meninas2009, alunos2009, y2009, T)

df_balance3 <- df_matched3 %>% 
  select(nota_ideb2009, p_exp_prof2009, p_meninas2009, alunos2009, y2009, T)

teste_bal1 <- balance_table(df_balance1, "T")
teste_bal1

teste_bal2 <- balance_table(df_balance2, "T")
teste_bal2

teste_bal3 <- balance_table(df_balance3, "T")
teste_bal3
#Agora, pós pareamento, as diferenças deram estatisticamente significativas para

#regressão que estima o impacto causal

reg0 <- felm(y2010~T+nota_ideb2009+p_exp_prof2009+p_meninas2009+alunos2009+y2009|id_mun|0|id_escola,
             data=df)
reg1 <- felm(y2010~T+nota_ideb2009+p_exp_prof2009+p_meninas2009+alunos2009+y2009|id_mun|0|id_escola,
             data=df_matched1)
reg2 <- felm(y2010~T+nota_ideb2009+p_exp_prof2009+p_meninas2009+alunos2009+y2009|id_mun|0|id_escola,
             data=df_matched2)
reg3 <- felm(y2010~T+nota_ideb2009+p_exp_prof2009+p_meninas2009+alunos2009+y2009|id_mun|0|id_escola,
             data=df_matched3)

summary(reg0)
summary(reg1)
summary(reg2)
summary(reg3)