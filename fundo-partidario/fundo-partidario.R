library("tidyverse")

gastos <- read.csv("despesas_2018_fundo-partidario_BRASIL.csv", header = TRUE, stringsAsFactors = FALSE)

gastos_por_partido <- gastos %>% 
  group_by(SG_PARTIDO) %>% 
  summarise(totalGasto = sum(VR_GASTO))

names(gastos_por_partido) = c("partido", "totalGasto")
  
write_csv(gastos_por_partido, "total_gastos_por_partido.csv")


# --------------------------------
library(jsonlite)

gastos_por_municipio <- gastos %>% 
  group_by(SG_PARTIDO, NM_MUNICIPIO) %>% 
  summarise(totalGasto = sum(VR_GASTO)) %>% 
  ungroup() %>% 
  filter(totalGasto > 5000) %>% 
  filter(NM_MUNICIPIO != '#NULO#') %>% 
  select(partido = SG_PARTIDO, municipio = NM_MUNICIPIO, totalGasto)

gastos_por_municipio_filtrado <- gastos_por_municipio %>% 
  distinct(partido)

lista_partidos_municipios <- as.list(t(gastos_por_municipio_filtrado$partido)) %>% 
  map(function(x) {
    lista <- gastos_por_municipio %>% 
      filter(partido == x) %>% 
      select(municipio, totalGasto)
    retorno <- list(name = x, children = as.data.frame(lista))
    return(retorno)
  })

write_json(lista_partidos_municipios, "gastos_por_municipio.json")
