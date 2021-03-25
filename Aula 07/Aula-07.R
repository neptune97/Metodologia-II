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

reg_lin <- lm (sales ~ youtube, data = bd) #criando o modelo
summary (reg_lin)
plot (reg_lin)



