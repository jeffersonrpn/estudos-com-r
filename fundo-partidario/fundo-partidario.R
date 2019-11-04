library("tidyverse")

gastos <- read.csv("despesas_2018_fundo-partidario_BRASIL.csv", header = TRUE, stringsAsFactors = FALSE)

gastos_por_partido <- gastos %>% 
  group_by(SG_PARTIDO) %>% 
  summarise(totalGasto = sum(VR_GASTO))

names(gastos_por_partido) = c("partido", "totalGasto")
  
write_csv(gastos_por_partido, "total_gastos_por_partido.csv")


library(jsonlite)

gastos_por_municipio <- gastos %>% 
  group_by(SG_PARTIDO, NM_MUNICIPIO) %>% 
  summarise(totalGasto = sum(VR_GASTO)) %>% 
  filter(totalGasto > 5000) %>% 
  filter(NM_MUNICIPIO != '#NULO#')

names(gastos_por_municipio) = c("partido", "municipio", "totalGasto")

lista_partidos_municipios <- data.frame(name = gastos_por_municipio$partido,
                                  children = data.frame(name = gastos_por_municipio$municipio, 
                                                  value = gastos_por_municipio$totalGasto))

toJSON(lista_partidos_municipios)

write_csv(gastos_por_municipio, "total_gastos_por_municipio_partido.csv")

