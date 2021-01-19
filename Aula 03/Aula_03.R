####################################################################
############################################################ AULA 03
########################################### AUTOR: FERNANDO DE SOUZA

####################################################################
######################################## Criando banco de dados no R

# E possivel mesclar dois vetores do mesmo tamanho para gerar um banco de 
# dados a partir dos comandos: cbind, rbind e data.frame

a <- c("A", "B", "C", "D", "E")
b <- c(65.8, 60, 41.8, 30, 23)

rbind (a, b) #cria tabela na horizontal 
cbind (a, b) #cria tabela na vertical
data.frame (a, b) #cria uma tabela convencional 

# Para poder usar a tabela criada e preciso enderecar ela a um objeto

dados <- data.frame (a, b)

# Outra forma de criar data.frames é a partir do comando "tibble" no 
# pacote "dplyr"

install.packages("tidyverse")
library (tidyverse)
dados2 <- tibble (a, b)

# Caso seja necessário é possível trocar o nome das colunas e das linhas
# a partir dos comandos "colnames" e "rownames"

colnames(dados)[1] <- "nomes"
rownames(dados)[1] <- "Pedro"

# Agora que sabemos como criar um banco de dados dentro do R vamos aprender 
# a importar um ja existente. 
# Duas formas podem ser usadas: por comando ou pelo botão "Import Dataset".

read.csv ()
read.csv2 ()
read.txt ()

# Bancos de dados em outros formatos (.xlsx, .spss, .dat) podem ser abertos 
# com ajuda de outros pacotes como o "haven".
install.packages("haven")
library (haven)
bd <- read_sav ("https://github.com/neptune97/Metodologia-II/blob/main/Aula%2002/Latinobarometro_2018_Esp_Spss_v20190303.sav?raw=true")

####################################################################
######################################### Explorando banco de dados

# 1. seleção de colunas, linhas e elementos
# Podemos selecionar colunas a partir do uso de colchetes ao lado do nome 
# do banco de dados. Caso queria mais de uma coluna use o intervalo de colunas 
# desejado e escreva-o dentro dos colchetes usando ":" como separador.
bd [5]

bd [5:10]

# para a seleçao de uma linha colocasse dentro dos colchetes o número da 
# linha seguido das colunas que devem entrar na seleção

bd [1, 5:8] #intervalo
bd [1, c(5:8)] #intervalo
bd [1, c(5,8)] #específico

# para selecionar elementos específicos usamos o comando c após a virgula e dentro dos
# parenteses utilizamos a localização do elemento

bd [2, c(8:4)]


# outra forma de solucionar uma coluna completa é a partir do comando banco de dados 
# seguido de cifrão

bd$S5

# para selecionar elementos dentro do banco de dados que sejam semelhantes ou obedeçam
# a uma regra/padrão utilizamos a seguinte forma:
# banco de dados$coluna operador elemento 

dados$b != 30

# similar a essa solução existe a função subset que cria bancos de dados com especifidades
# basta que vc assinale o banco de dados e qual a regra para extrair o dado

dt <- subset (dados, b >= 40)
dt2 <- subset (bd, bd$TAMCIUD >= 3)

# é possível ainda extrair dados sobre o banco utilizando alguns do comandos que
# já sabemos

sum (bd$EDAD)
mean (bd$EDAD)
median (bd$EDAD)
sd (bd$EDAD)

# novos comandos
length(bd) #número de colunas
summary (bd$TAMCIUD) #resumo dos dados
head (bd) # mostra as primeiras linhas 
tail (bd) # mostra as últimas linhas 
glimpse (bd)


#natureza das variáveis
# números
# caracteres
# lógicos
# fatores

# descobrindo a classe da variável
class ()

# transformação de classes
as.factor ()
as.character ()
as.numeric ()

dados$nomes <- as.numeric(dados$nomes)
dados$nomes <- as.character(dados$nomes)

####################################################################
#################################################### Extraindo dados 

install.packages("descr")
library (descr)
freq (bd$TAMCIUD)
freq (bd$TAMCIUD, plot = F)

####################################################################
######################################### Intervalos de Confiança

t.test(bd$EDAD, conf.level = 0.90)
t.test(bd$EDAD)
t.test(bd$EDAD, conf.level = 0.99)
