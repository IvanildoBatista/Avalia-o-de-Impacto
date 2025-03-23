library(readxl)
library(tidyverse)
library(dplyr)
#install.packages(c('lfe','randomizr','RCT','xtable','stargazer'))
library(lfe)
library(randomizr)
library(RCT)
library(xtable)
library(stargazer)
#install.packages('rdpower')
library(rdpower)

#Definindo o diretório ----
setwd('C:\\Users\\Ivanildo Batista\\Desktop\\Cursos FGV\\Avaliação de Impacto')

#Lendo a base de dados ----
base_educ <- read.delim('base_rct_educ.csv', sep = ',')
base_educ %>% as.tibble() %>% filter(ano==2009)
colnames(base_educ)

#Definindo a semente ----
set.seed(2025)

#Calculando o Efeito Mínimo Detectável ----

#Forma analítica
tk = 0.84
ta = 1.96
n  = 6000

# Criando uma função
MDE <- function(p) {
  (tk+ta)*((1/(p*(1-p)))^0.5*((1/n)^0.5))
}

#valor do MDE para p = 50%
MDE(.5)

#Gráfico do MDE
plot(MDE, 
     0.01, 
     0.99,
     xlab='P',
     ylab='MDE', 
     tck=1, 
     col='blue')

# Simulando o MDE com 100 regressões
df_simul <- base_educ %>% filter(ano==2009)

se <- c()
se_c <- c()
i <- 0 #quando inicia as interações

while (i < 100) {
  df_simul <- df_simul %>% 
    dplyr::select(-T) %>% 
    mutate(T = block_ra(blocks = id_mun, prob=0.5))
  
  reg <- felm(y~ T|id_mun|0|id_escola, data=df_simul) #calculando a regressão
  se<- c(se, reg$se[1]) #armazenando o desvio padrão simulado
  
  #com variaveis de controle
  reg_c <- felm(y~ T+p_meninas+p_exp_prof+nota_ideb+alunos|id_mun|0|id_escola, 
                data=df_simul) #calculando a regressão
  sec_c <- c(se, reg_c$se[1]) #armazenando o desvio padrão simulado
  
  i <- i+1
}

#Obtendo o MDE sem controle nas regressões
(1.96+.84)*mean(c(se))
(1.96+.84)*mean(c(se))/sd(df_simul$y)

# Testes de balanceamento ----

#Filtrar para o ano de 2009 (anterior ao programa) e variáveis selecionadas:
# - Proporção de meninas na escola (p_meninas)
# - Proporção de professores com mais de 8 anos de experiência (p_exp_prof)
# - número total de alunos terceiro ano (alunos)
# - ideb da escola (nota_ideb)
# - nota media do ENEM, variável de interesse (y)
# - variável indicadora do status tratamento (T)

df_balance <- base_educ %>% 
  filter(ano==2009) %>% 
  select(id_mun, id_escola, p_meninas, p_exp_prof, nota_ideb, alunos, y, T)

#Gerando tabela de balanceamento
#Isso é o equivalente a fazer o teste T para as medias de cada grupo
teste_balanceamento <- balance_table(df_balance, "T")

#Exemplo com o teste t-student
#se maior que 0.05, então as médias são iguais
t.test(df_balance %>% filter(T==0) %>% select(alunos),
       df_balance %>% filter(T==1) %>% select(alunos))

#Obter p-valor da diferença de médias entre os grupos, incluindo efeito fixo de
#estado

xbal1 <- felm(p_meninas~T|id_mun|0|id_escola, data=df_balance)
xbal2 <- felm(p_exp_prof~T|id_mun|0|id_escola, data=df_balance)
xbal3 <- felm(nota_ideb~T|id_mun|0|id_escola, data=df_balance)
xbal4 <- felm(alunos~T|id_mun|0|id_escola, data=df_balance)
xbal5 <- felm(y~T|id_mun|0|id_escola, data=df_balance)

pval <- c(xbal1$pval, xbal2$pval, xbal3$pval, xbal4$pval, xbal5$pval)

teste_bal <- teste_balanceamento[-c(2,3)]

# Regressoes ----

df <- base_educ %>% filter(ano==2010)

#tratamento e efeito fixo
reg1 <- felm(y~T|id_mun|0|id_escola, data=df)

#controlando para renda, escolaridade e população
reg2 <- felm(y~T+p_meninas+alunos+p_exp_prof+nota_ideb|id_mun|0|id_escola, data=df)


summary(reg1)
summary(reg2)















