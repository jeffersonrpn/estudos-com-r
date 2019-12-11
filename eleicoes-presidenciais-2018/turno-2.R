library(tidyverse)
library(jsonlite)
library(purrr)
library(tidyr)

ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO");

votos_haddad <- map_df(ufs, function(x) {
  uf <- fromJSON(paste0("https://eleicoes.datapedia.info/api/votes/bystate/244/2261538/", x)) %>% 
    mutate(uf = x)
})


votos_bolsonaro <- map_df(ufs, function(x) {
  uf <- fromJSON(paste0("https://eleicoes.datapedia.info/api/votes/bystate/244/2254251/", x)) %>% 
    mutate(uf = x)
})

votos_haddad_por_uf <- votos_haddad %>% 
  filter(phase == 2) %>% 
  group_by(uf) %>% 
  summarise(total_votos_haddad = sum(votable_votes), total_votos_validos = sum(total_votes)) %>% 
  mutate(candidato = 'Fernando Haddad', proporcao_haddad = round(total_votos_haddad / total_votos_validos, 3))

votos_bolsonaro_por_uf <- votos_bolsonaro %>% 
  filter(phase == 2) %>% 
  group_by(uf) %>% 
  summarise(total_votos_bolsonaro = sum(votable_votes), total_votos_validos = sum(total_votes)) %>% 
  mutate(candidato = 'Jair Bolsonaro', proporcao_bolsonaro = round(total_votos_bolsonaro / total_votos_validos, 3))

eleicoes_presidenciais_2018_2_turno <- votos_bolsonaro_por_uf %>% 
  inner_join(votos_haddad_por_uf, by = "uf") %>% 
  select(uf, total_votos_validos = total_votos_validos.x, total_votos_haddad, total_votos_bolsonaro, proporcao_haddad, proporcao_bolsonaro)

write_csv(eleicoes_presidenciais_2018_2_turno, "eleicoes_presidenciais_2018_2_turno.csv")
