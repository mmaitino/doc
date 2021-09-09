library(tidyverse)
library(lubridate)

# Limpando os dados --------- 
deleg <- deleg_2021_07_22 %>% select(nome, conf, id_org_dupla)
org <- orgs_2021_08_13 %>% select(id_org_dupla, id_org_unica)
eventos <- eventos_v3 %>% rename(conf = `Nome do evento`, data = Data
                                 tema = `Regime/Tema`, tipo_evento = `Tipo evento`, 
                                 mea = `Lista MEA?`,
                                 main = `Principais convenções + gdes conf`) %>% 
  select(conf, Data)


min_grupo <- deleg %>% 
  left_join(org) %>% select(-id_org_dupla) %>% left_join(eventos) %>% 
  mutate(# Organização das datas
    data = if_else(str_count(data)==4, #se falta o mês (só ano)
                   paste0(data, "-01"), #padroniza como janeiro
                   data)) %>% 
  mutate(
    data = if_else(is.na(data)== F,
                   paste0(data, "-01"), #padroniza data no dia 1 do mês
                   data)
  ) %>% mutate(data = ymd(data), ano = year(ymd(data))) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  filter(ano >= 1970 & ano <= 2019)

# Agrupando os dados --------------
min_grupo <- min_grupo %>% 
  filter(id_org_unica %in% orgs_ministeriais$id_org_unica) %>% #apenas ministérios
  group_by(ano, id_org_unica) %>% summarise(part_count = n())
  
part_min_ano <- min_grupo  %>% 
  # incluir contagem de observações nulas (freq = 0)
  complete(id_org_unica, ano = 1970:2019,
           fill = list(part_count = 0)) %>% 
  mutate(# criar binária
  part_binaria = if_else(part_count >=1, 1, 0)
) %>% 
  left_join(orgs_ministeriais)


# Salvando o arquivo -----------

save_updated <- function(nome_base){
  write.csv2(get(paste0(nome_base), globalenv()),
             paste0(nome_base,"-",Sys.Date(),".csv"), row.names = F
  )
}
save_updated("part_min_ano")
