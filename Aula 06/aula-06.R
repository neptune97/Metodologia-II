####################################################################
############################################################ AULA 06
########################################### AUTOR: FERNANDO DE SOUZA

library (readr)
library (tidyverse)

# teste - t para duas médias

# amostras independentes (objetos independentes entre si/ condições diferentes) [NÃO PAREADO]
# amostras dependentes (mesmo objeto/universo em ocasiões diferentes) [PAREADO]
# única amostra (média universal = mu)


# PAREADO

antes  <- c(21, 28, 24, 23, 23, 19, 28, 20, 22, 20, 26, 26)
depois <- c(26, 27, 23, 25, 25, 29, 30, 31, 36, 23, 32, 22)

t.test (antes, depois, paired = TRUE)

# NÃO - PAREADO
# (numérico - binário)

notas <- c(7, 4, 10, 5, 8, 1, 9, 3, 8, 4, 10, 2)
disc <- rep(c("a", "b"), 6)

t.test (notas ~ disc)


# (numérico - numérico)

controle     <- c(21, 28, 24, 23, 23, 19, 28, 20, 22, 20, 26, 26)
experimental <- c(26, 27, 23, 25, 25, 29, 30, 31, 36, 23, 32, 22)

t.test (controle, experimental)


# boxplot
boxplot (controle, experimental)
boxplot (notas ~ disc)

x <- data.frame (notas, disc)

x %>% 
  ggplot (aes (x = notas, y = disc)) +
  geom_boxplot() + 
  coord_flip() +
  labs (
    x = "Notas",
    y = "Disciplinas",
    title = "Diferenças entre as notas nas disciplinas A e B",
    subtitle = "Dados fictícios"
  ) +
  theme_light()

 

# diferenças de proporção
survivors <- matrix(c(1781,1443,135,47), ncol=2)
colnames(survivors) <- c('survived','died')
rownames(survivors) <- c('no seat belt','seat belt')

prop.test(survivors)

# Dois grupos (A e B), cada um com 500 indivíduos, possuem pessoas que fumam com bastante frequência. Apesar disso
# somente os fumantes do grupo A desenvolveram câncer de pulmão.
# Numa tentativa de entender a relação do fumo com a doença foi-se 
# contado quantas pessoas de cada grupo fumavam.
# Ao total 490 indivíduos do grupo A fumavam contra 320 do grupo B.
# Existe alguma diferença significativa entre as proporções?

prop.test(x = c(490, 320), n = c(500, 500))


# tabelas de contingência
table(infert$education, infert$spontaneous)

# x²
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))

x <- chisq.test(M)

####################################################################
############################################ ANALISANDO UM CASO REAL
################################################### NETFLIX x AMAZON

dt <- read_csv("https://github.com/neptune97/Metodologia-II/blob/main/Aula%2006/MoviesOnStreamingPlatforms_updated.csv?raw=true")

## criando um banco de dados apenas com filmes da Netflix e da Amazon
bd <- dt %>% 
  filter(Hulu == 0 & `Disney+` == 0) %>% 
  mutate ( origem = case_when (
    Netflix == 1 | `Prime Video` == 0  ~ "Netflix",
    Netflix == 0 | `Prime Video` == 1  ~ "Prime"
  ))

bd$`Rotten Tomatoes` <- gsub("%", "", dt2$`Rotten Tomatoes`)
bd$`Rotten Tomatoes` <- as.numeric(dt2$`Rotten Tomatoes`)

## estabilização da aleatorização
set.seed(123)

## coleta de amostras aleatórias do banco de dados
b <- bd %>%
  group_by (origem) %>% 
  sample_n(100)

## a hora da verdade
t.test (b$IMDb ~ b$origem)

## testes adicionais: quem tem os maiores filmes?
t.test(b$Runtime ~ b$origem)

## melhor performance no RT
t.test(b$`Rotten Tomatoes` ~ b$origem)

## boxplot IMDB
b %>% 
  ggplot(aes (x= IMDb, y = origem )) +
  geom_boxplot(color = c("#eb787d", "#86dbf7"), fill = c("#E50914", "#00A8E1")) +
  labs (
    x = "IMDB",
    y = "Empresa",
    title = "Média do IMDB entre Netflix e Amazon Prime",
    subtitle = "Filmes datados de 1933 à 2020"
  ) +
  coord_flip() +
  theme_minimal()

## nota RT
b %>% 
  ggplot(aes (x= `Rotten Tomatoes`, y = origem )) +
  geom_boxplot(color = c("#eb787d", "#86dbf7"), fill = c("#E50914", "#00A8E1")) +
  labs (
    x = "Rotten Tomatoes",
    y = "Empresa",
    title = "Média de % no Rotten Tomatoes entre Netflix e Amazon Prime",
    subtitle = "Filmes datados de 1933 à 2020"
  ) +
  coord_flip() +
  theme_minimal()


## boxplot runtime
b %>% 
  ggplot(aes (x= Runtime, y = origem)) +
  geom_boxplot(color = c("#eb787d", "#86dbf7"), fill = c("#E50914", "#00A8E1")) +
  labs (
    x = "Duração do filme (em minutos)",
    y = "Empresa",
    title = "Média de duração do filme (em minutos) entre Netflix e Amazon Prime",
    subtitle = "Filmes datados de 1933 à 2020"
  ) +
  coord_flip() +
  theme_minimal()


