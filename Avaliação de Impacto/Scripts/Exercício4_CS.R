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
#install.packages('rddensity')
library(rddensity)
library(Synth)

#Definindo o diretório ----
setwd('C:\\Users\\Ivanildo Batista\\Desktop\\Cursos FGV\\Avaliação de Impacto')

#Lendo a base de dados ----
base_health <- read.delim('base_sc_health.csv', sep = ',')
set.seed(2025)

base_health<- base_health %>% 
  mutate(pos = ifelse(ano >= 2011,1,0),
         pos_T = pos*T)

#a quantidade de profissionais da saúde (médicos e enfermeiros) por hospital;
#a renda média per capita do distrito em que o hospital está localizado;
#o número médio de equipamentos utilizados pelos médicos;
#o número de pessoas na fila de espera por consultas e exames.

colnames(base_health)

base_health_pre <- base_health %>% filter(ano==2009)#pré
base_health_pre_trat <- base_health %>% filter(ano==2009 & T==1)#tratamento
base_health_pre_contr <- base_health %>% filter(ano==2009 & T==0)#controle

#Estatísticas descritivas
describeBy(base_health_pre[c("profiss_saude","n_equipamentos",
                             "renda","y","T")], 
           group="T")


#Controle sintético

base_health <- base_health %>% 
  mutate(id_char = as.character(id_mun))


data <- dataprep(foo = data.frame(base_health),
                 predictors = "y",
                 dependent = "y",
                 time.variable = "ano",
                 unit.variable = "id_mun",
                 unit.names.variable = "id_char",
                 treatment.identifier = 2,
                 controls.identifier = c(1,c(3:38)),
                 time.predictors.prior = c(2005:2009),
                 time.optimize.ssr = c(2005:2009))

peso <- synth(data)$solution.w
peso<- data.frame(peso) %>%
  mutate(id_mun = rownames(.))
colnames(peso)[1] <- 'w.weight'

base_health <- merge(base_health, peso, by='id_mun',all=T)


aux <- base_health %>% 
  mutate(peso=ifelse(T==1,1,peso)) %>% 
  group_by(T, ano) %>% 
  summarise(y=sum(y*peso)/sum(peso))












