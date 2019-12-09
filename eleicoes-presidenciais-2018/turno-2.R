library(tidyverse)
library(jsonlite)
library(purrr)

ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MA", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO");

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
  summarise(total_votos_validos = sum(votable_votes), total_votos = sum(total_votes)) %>% 
  mutate(candidato = 'Fernando Haddad', proporcao_haddad = total_votos_validos / total_votos)

votos_bolsonaro_por_uf <- votos_bolsonaro %>% 
  filter(phase == 2) %>% 
  group_by(uf) %>% 
  summarise(total_votos_validos = sum(votable_votes), total_votos = sum(total_votes)) %>% 
  mutate(candidato = 'Jair Bolsonaro')

eleicoes_presidenciais_2018_2_turno = rbind(votos_bolsonaro_por_uf, votos_haddad_por_uf)

write_csv(eleicoes_presidenciais_2018_2_turno, "eleicoes_presidenciais_2018_2_turno.csv")
