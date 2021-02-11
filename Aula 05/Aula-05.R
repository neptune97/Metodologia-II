####################################################################
############################################################ AULA 05
########################################### AUTOR: FERNANDO DE SOUZA

library (readr)
pnad_mg <- read_csv("https://github.com/neptune97/Metodologia-II/blob/main/Aula%2005/pnad_mg2.csv?raw=true", col_types = cols(.default = "d"))

#teste t
t.test(pnad_covid$C009, mu = 39.69) #bicaudal
t.test(pnad_covid$C009, mu = 39.69, alternative = "less") #unicaudal
t.test(pnad_covid$C009, mu = 39.69, alternative = "greater") #unicaudal

#usando banco de dados aleatório

dt <- data.frame (
P = paste0(rep("X_", 10), 1:10),
A1 =  sample (1:10, 10, replace = FALSE)
)

dt$A2 <- dt$A1 - 2

mean (dt$A1)
t.test(dt$A2, mu = 3)
t.test(dt$A2, mu = 5, conf.level = 0.90)
t.test (dt$A2, mu = 5, alternative = "less")


# teste de proporção
dt <- infert
table (dt$spontaneous)
prop.test(x = c(141, 107), n = c(248,248), alternative = "two.sided",
          correct = TRUE)


#chi²

dt2 <- data.frame(
  FX_i =  c("adolescente", "adulto", "idoso"), 
  Hr_T =  sample(1:24, 60, replace = T))

dt2 <- dt2 %>% 
  mutate (Hr_C = ifelse (Hr_T > 9, "alto", "baixo"))

a <- chisq.test(dt2$FX_i, dt2$Hr_C)

#casos reais
file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)
b <- chisq.test(housetasks)
