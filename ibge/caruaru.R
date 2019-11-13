library(readr)
library(dplyr)

setores_caruaru <- read_csv2("PE/BASE/CSV/Basico_PE.csv", locale = locale(encoding = "latin1")) %>% 
  filter(Nome_do_municipio == "CARUARU") %>% 
  select(Cod_setor, Cod_municipio, Nome_do_distrito, Cod_bairro, Nome_do_bairro, V009)

dados <- read_csv2("PE/BASE/CSV/Domicilio01_PE.csv", locale = locale(encoding = "latin1")) %>%
  filter(Cod_setor %in% setores_caruaru$Cod_setor) %>%
  mutate_at(.funs = list(~if_else(. == 'X', 0, as.double(.)) ), .vars = vars(V062, V063, V064, V065, V066, V067, V068, V081, V082, V083, V084, V085, V086, V087)) %>% 
  select(Cod_setor, V062, V063, V064, V065, V066, V067, V068, V081, V082, V083, V084, V085, V086, V087)

domicilio <- dados %>% 
  mutate(responsavelHomem = V062 + V063 + V064 + V065 + V066 + V067 + V068, responsavelMulher = V081 + V082 + V083 + V084 + V085 + V086 + V087) %>% 
  select(Cod_setor, responsavelHomem, responsavelMulher) %>% 
  mutate(proporcaoHomem = responsavelHomem / (responsavelHomem + responsavelMulher), proporcaoMulher = responsavelMulher / (responsavelHomem + responsavelMulher))
