# EXEMPLO: NETFLIX x AMAZON no IMDB

dt2 <- dt %>% 
  filter(Hulu == 0 & Disney. == 0) %>% 
  mutate ( origem = case_when(
      Netflix == 1 | Prime.Video == 0  ~ "Netflix",
      Netflix == 0 | Prime.Video == 1  ~ "Prime"
    )
  )
  
b <- dt2 %>%
  group_by (origem) %>% 
  sample_n(100)

t.test (b$IMDb ~ b$origem)

t.test(b$Runtime ~ b$origem)

b %>% 
  ggplot(aes (x= IMDb, y = origem )) +
  geom_boxplot() +
  labs (
    x = "IMDB",
    y = "Empresa",
    title = "Média do IMDB entre Netflix e Amazon Prime",
    subtitle = "Filmes datados de 1933 à 2020"
  ) +
  coord_flip() +
  theme_minimal()

b %>% 
  ggplot(aes (x= Runtime, y = origem)) +
  geom_boxplot() +
  labs (
    x = "Duração do filme (em minutos)",
    y = "Empresa",
    title = "Média de duração do filme (em minutos) entre Netflix e Amazon Prime",
    subtitle = "Filmes datados de 1933 à 2020"
  ) +
  coord_flip() +
  theme_minimal()
