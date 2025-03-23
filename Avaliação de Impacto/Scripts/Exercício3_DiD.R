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

#Definindo o diretório ----
setwd('C:\\Users\\Ivanildo Batista\\Desktop\\Cursos FGV\\Avaliação de Impacto')

#Lendo a base de dados ----
base_health <- read.delim('base_did_health.csv', sep = ',')
set.seed(2025)

#criando a variável de tratamento e de interação
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


# Regressão em DID
#Sem efeito fixo
reg1 <- felm(y~T+pos+pos_T|0|0|id_hosp, data=base_health)

#incluindo o efeito fixo com o efeito do municipio
reg2 <- felm(y~pos_T|ano+id_hosp|0|id_hosp, data=base_health)

#controlando por covariadas
reg3 <- felm(y~pos_T+profiss_saude+renda+n_equipamentos|ano+id_hosp|0|id_hosp, data=base_health)

summary(reg3)

#teste de tendencias paralelas

aux <- base_health %>% 
  group_by(ano,T) %>% 
  dplyr::summarise(estimate = mean(y))

#plotar um gráfico de linhas no ggplot


#efeito da intervenção para cada um dos anos seguintes

reg4 <- felm(y~T+pos+pos_T|0|0|id_hosp, data=base_health)

aux <- broom::tidy(felm(y~T*factor(ano)-factor(ano)|ano+id_hosp, 
                        data=base_health)) %>% 
  mutate(ano = c(2005:2014))


ggplot(aux, aes(x=ano, y=estimate))+
  geom_vline(xintercept = 2009.5, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  annotate("text",x=2009.7, y=0.3, label="tratamento",hjust=0, angle=90, size=3)+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = estimate-1.96*std.error, 
                    ymax = estimate+1.96*std.error),alpha=0.5,
                width=.5)+
  scale_x_continuous(breaks = seq(2005,2015))+
  theme_classic()+
  theme(legend.position = "bottom")
  
  
  
  
  
  
  








