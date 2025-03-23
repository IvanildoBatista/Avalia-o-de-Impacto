library(readxl)
library(tidyverse)
library(dplyr)
library(lfe)
library(randomizr)
library(RCT)
library(xtable)
library(stargazer)
library(rdpower)
library(psych)
library(rdrobust)
library(rdd)
library(data.table)
#install.packages('rddensity')
library(rddensity)

#Definindo o diretório ----
setwd('C:\\Users\\Ivanildo Batista\\Desktop\\Cursos FGV\\Avaliação de Impacto')

#Lendo a base de dados ----
base_educ <- read.delim('base_RD_educ.csv', sep = ',')
colnames(base_educ)
base_educ %>% view()


#Apresentar estatísticas descritivas para a amostra, 
#separando o grupo de escolas tratadas (as que possuem 
#550 estudantes ou mais) e o grupo de controle (menos de 550 estudantes).
#Escreva um breve comentário sobre os resultados da tabela.

base_educ <- base_educ %>% mutate(T = ifelse(alunos>=350, 1, 0))
base_educ %>% filter(alunos >=350)

describeBy(base_educ[c("p_exp_prof","p_meninas",
                       "nota_ideb","alunos","y","T")], 
           group="T")

#apresentar o gráfico de descontinuidade na variável dependente;

ggplot()+
  geom_point(data=base_educ2, aes(x=alunos, y=y))+
  geom_vline(xintercept = 350, colour = 'red', linetype = 'dashed', linewidth = 1)+
  geom_smooth(data = base_educ, aes(x=alunos, y=y,group = T),method = 'lm')+
  theme_bw()

#estimar a regressão descontínua, utilizando uma regressão linear local 
#(você pode utilizar uma janela escolhida pelo método de Calonico, Cattaneo e Titiunik (2014)). 
#Adicionar:
# -- especificações com uma janela menor e outra maior, em comparação com a janela ótima;
# -- como quarta especificação na tabela, a regressão quadrática em vez da local linear.

IKbandwidth(base_educ$y, 
            base_educ$alunos, 
            cutpoint = 350, 
            verbose = TRUE, 
            kernel = "triangular")

summary(est1 <- RDestimate(base_educ$y ~ base_educ$alunos))
plot(est1)

rdplot(y=base_educ$y, 
       x=base_educ$alunos ,
       c=350)
rdbwselect(y=base_educ$y, 
           x=base_educ$alunos ,
           c=350, 
           fuzzy = NULL)
#
reg<-rdrobust(y=base_educ$y, x=base_educ$alunos ,c=350)
summary(reg)

#O tratamento teve um efeito positivo de 1,463 na nota do ENEM da escola
#=============================================================================
#  Method     Coef. Std. Err.         z     P>|z|      [ 95% C.I. ]       
#=============================================================================
#  Conventional     1.463     0.041    35.692     0.000     [1.383 , 1.544]     
#       Robust         -         -    29.853     0.000     [1.361 , 1.553]     
#=============================================================================

#Salvando as estimativas
Resultado <- matrix(NA,6,5)
Resultado[1,1] <- c('Estimativa')
Resultado[2,1] <- c('Erro-padrão')
Resultado[3,1] <- c('P-valor')
Resultado[4,1] <- c('Janela')
Resultado[5,1] <- c('Nobs esquerda')
Resultado[6,1] <- c('Nobs direita')

#função para salvar os resultados
salvar_resultado <- function(reg){
  vetor <- matrix(NA,7,1)
  Resultado[1,1] <- reg$Estimate[1,1]
  Resultado[2,1] <- reg$Estimate[1,4]
  Resultado[3,1] <- reg$pv[3,1]
  Resultado[4,1] <- reg$bws[1,1] #esse aqui se refere a janela
  Resultado[5,1] <- reg$N_h[1]
  Resultado[6,1] <- reg$N_h[2]
  return(vetor)
}

Resultado[1:6,2] <- salvar_resultado(reg)

reg$bws[1,1]
#[1] 30.34029 o cuttoff mais ou menos esse valor


ggplot()+
  geom_point(data=base_educ2, aes(x=alunos, y=y))+
  geom_vline(xintercept = 350, colour = 'blue', linetype = 'dashed', linewidth = 1)+
  geom_vline(xintercept = 350-reg$bws[1,1], colour = 'red', linetype = 'dashed', linewidth = 1)+
  geom_vline(xintercept = 350+reg$bws[1,1], colour = 'red', linetype = 'dashed', linewidth = 1)+
  geom_smooth(data = base_educ, aes(x=alunos, y=y,group = T),method = 'lm')+
  theme_bw()

#robustez
#utilizando janelas arbitrárias

reg1<- rdrobust(y=base_educ$y, x=base_educ$alunos ,c=350, h=50) #janela = 50
reg2<- rdrobust(y=base_educ$y, x=base_educ$alunos ,c=350, h=100) #janela = 100
reg3<- rdrobust(y=base_educ$y, x=base_educ$alunos ,c=350, p=2) #regressão quadratica

reg$Estimate[1,1]
reg1$Estimate[1,1]
reg2$Estimate[1,1]
reg3$Estimate[1,1]

ggplot()+
  geom_point(data=base_educ2, aes(x=alunos, y=y))+
  geom_vline(xintercept = 350, colour = 'blue', linetype = 'dashed', linewidth = 1)+
  geom_vline(xintercept = 350-reg$bws[1,1], colour = 'red', linetype = 'dashed', linewidth = 1)+
  geom_vline(xintercept = 350+reg$bws[1,1], colour = 'red', linetype = 'dashed', linewidth = 1)+
  geom_vline(xintercept = 350-reg1$bws[1,1], colour = 'green', linetype = 'dashed', linewidth = 1)+
  geom_vline(xintercept = 350+reg1$bws[1,1], colour = 'green', linetype = 'dashed', linewidth = 1)+
  geom_vline(xintercept = 350-reg2$bws[1,1], colour = 'purple', linetype = 'dashed', linewidth = 1)+
  geom_vline(xintercept = 350+reg2$bws[1,1], colour = 'purple', linetype = 'dashed', linewidth = 1)+
  geom_smooth(data = base_educ, aes(x=alunos, y=y,group = T),method = 'lm')+
  theme_bw()

#Considerando outras janelas

#estimar e comentar os seguintes resultados:
# -- teste de placebo;
## definindo outros critérios de corte
reg4<- rdrobust(y=base_educ$y, x=base_educ$alunos ,c=250) #janela = 250
reg5<- rdrobust(y=base_educ$y, x=base_educ$alunos ,c=500) #janela = 500
# -- testes de balanceamento (covariadas como variável dependente);
reg6<- rdrobust(y=base_educ$p_meninas, x=base_educ$alunos ,c=350, p=2) #usando uma covariavel
#espera-se que não haja nenhum efeito significativo

# -- teste de manipulação e continuidade da densidade.

densidade <- rddensity(base_educ$alunos, c=350)
densidade

rdplotdensity(densidade, base_educ$alunos)
densidade$test$p_jk #p-valor do teste
#H0: há continuidade










