####################################################################
############################################################ AULA 04
########################################### AUTOR: FERNANDO DE SOUZA

library (tidyverse)
library (readr)

#ler banco de dados
setwd ("~/PNAD_COVID_112020")
pnad_covid <- read_csv("PNAD_COVID_112020.csv", col_types = cols(.default = "d"))
dt <- subset (pnad_covid, pnad_covid$UF == "31")

#plots no R

plot (dt$A005) #pontos
plot (table (dt$A005)) #barras
hist (dt$A005) # histograma

barplot(table(dt$A005)) #barras
pie(table(dt$A005)) #torta

barplot(table (dt$A004, dt$C013), main = "Relação entre Raça e Home Office", 
        sub = "Fonte: Microdados da Pnad Covid19 - IBGE. Novembro 2020.",
        ylab = "Frequência", xlab = "Home Office")


#recodificando os dados
dt <- dt %>%
  mutate(Sexo = ifelse(A003 == 1, "Homem", "Mulher"),
         Cor = case_when(
           A004 == 1 ~ "Branca", 
           A004 == 2 ~ "Preta", 
           A004 == 4 ~ "Parda"),
         home_office = ifelse(C013 == 1, "Home Office", "Presencial"),
         mao_de_obra = ifelse(C001 == 1, "Sim", "Nao"),
         Escolaridade = factor(case_when( 
           A005 %in% 1:2 ~ "Sem Instrução ou Fundamental Incompleto", 
           A005 %in% 3:4 ~ "Fundamental completo ou Médio Incompleto", 
           A005 %in% 5:6 ~ "Médio completo ou Superior Incompleto", 
           A005 == 7 ~ "Superior completo", 
           A005 == 8 ~ "Pós-graduação"), 
           levels = c( "Sem Instrução ou Fundamental Incompleto",
                       "Fundamental completo ou Médio Incompleto", 
                       "Médio completo ou Superior Incompleto",
                       "Superior completo",
                       "Pós-graduação")), 
  )

#########################################################
# pacote para gráficos

install.packages("ggplot2")
library (ggplot2)

##########################################################

# criação da tabela
esc_ho <- dt %>% 
  group_by(Escolaridade, home_office) %>% 
  count() %>% 
  drop_na ()

# criação do gráfico
ggplot (esc_ho, aes (fill = home_office, x = Escolaridade, y = n)) +
  geom_bar(stat = "identity", position = "dodge") +
theme_minimal() +
  labs(x = "Sexo", 
       y = "Frequência",
       fill = "Cor/Raça: ", 
       caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Novembro 2020.",
       title = "Pessoas em home office, por cor/raça - Belo Horizonte/MG")


###########################################################
# criação da tabela
home_c <- dt %>% 
  group_by(Cor, home_office) %>%
  count() %>% 
  drop_na()

# criação do gráfico
ggplot (home_c, aes(fill = Cor, x = home_office, y = n)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_classic() +
  labs(x = "Sexo", 
       y = "Frequência",
       fill = "Cor/Raça: ", 
       caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Novembro 2020.",
       title = "Pessoas em home office, por cor/raça - Belo Horizonte/MG") +
  theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", 
                                                               size=0.7, linetype="blank")) +
  scale_fill_manual(values = c("#9370db","#da70d6","#dda0dd","#e6e6fa"))
