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

votos_haddad_2_turno <- votos_haddad %>% 
  filter(phase == 2) %>% 
  mutate(candidato = 'Fernando Haddad', proporcao_haddad = round(votable_votes / total_votes, 3)) %>% 
  select(candidato, codigo_municipio = location_code, municipio = location, uf, proporcao_haddad, votos_haddad = votable_votes, votos_validos = total_votes)

votos_bolsonaro_2_turno <- votos_bolsonaro %>% 
  filter(phase == 2) %>% 
  mutate(candidato = 'Jair Bolsonaro', proporcao_bolsonaro = round(votable_votes / total_votes, 3)) %>% 
  select(candidato, codigo_municipio = location_code, municipio = location, uf, proporcao_bolsonaro, votos_bolsonaro = votable_votes, votos_validos = total_votes)

eleicoes_presidenciais_2018_2_turno <- votos_bolsonaro_2_turno %>% 
  inner_join(votos_haddad_2_turno, by = "codigo_municipio") %>% 
  select(codigo_municipio, municipio = municipio.x, uf = uf.x, proporcao_haddad, proporcao_bolsonaro, votos_haddad, votos_bolsonaro, votos_validos = votos_validos.x)

write_csv(eleicoes_presidenciais_2018_2_turno, "eleicoes_presidenciais_2018_2_turno.csv")
