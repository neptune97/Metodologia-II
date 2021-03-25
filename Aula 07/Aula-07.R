####################################################################
############################################################ AULA 07
########################################### AUTOR: FERNANDO DE SOUZA

---
  
# ANOVA

bd <- PlantGrowth # carregando banco de dados

a <- aov(weight ~ group, data = data) # anova

TukeyHSD(a) #Tukey Honest Significant Differences


---
  
# REGRESSÃO LINEAR

#install.packages("datarium")
library (datarium) ## carregando pacote
bd <- marketing # abrindo banco de dados
cor (bd) # checando correlações
plot (bd) # plotando gráficos de dispersão

#metodo 2 para checagem de correlação 
library (corrplot)
x <- cor(bd)
corrplot (x)

### REGRESSÃO
reg_lin <- lm (sales ~ youtube, data = bd) #criando o modelo
summary (reg_lin)
plot (reg_lin)


## PLOT GRÁFICO REGRESSÃO ---
library (tidyverse)

bd %>% 
  ggplot(aes(youtube, sales)) +
  geom_point() +
  labs (
    x = "Youtube",
    y = "Vendas (em milhões)",
    title = "Relação entre Investimento em anúncios no Youtube e Vendas"
  ) +
  stat_smooth(method = lm) +
  theme_minimal()



