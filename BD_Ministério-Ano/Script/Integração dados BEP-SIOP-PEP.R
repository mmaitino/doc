library(tidyverse)

bep <- bep_servidores %>% rename(BEP_orgao = orgao) %>% right_join(dic_bep) %>% 
  select(-c(BEP_orgao, org_limpo))

pep <- pep_superior %>% rename(PEP_orgao_superior = orgao_superior) %>% 
  # select(-X1) %>% 
  right_join(dic_pep)

siop <- siop_orcamento %>% rename(SIOP_orgao = orgao) %>% 
  right_join(dic_siop)
  

# Há múltiplas observações por ano para alguns dos ministérios nos dados originais
# Somar os duplicados
pep <- pep %>% select(-c(org_limpo, PEP_orgao_superior)) %>% 
  group_by(ano, id_org_limpo) %>% 
  summarise(across(everything(), .f = sum, na.rm = T)) #%>% 
  # right_join(dic_pep) %>% select(-PEP_orgao_superior)


# SIOP código do ministério mudou no ano p/ alguns; MCT tem parte sob outro código
# Necessário somar os códigos equivalentes no mesmo ano
siop <- siop %>% 
  select(ano, id_org_limpo, 
         dotacao_atual_total, pago_total, dotacao_atual_invest, pago_invest) %>% 
  group_by(ano, id_org_limpo) %>% 
  summarise(across(everything(), .f = sum, na.rm = T)) #%>% 
  # right_join(dic_siop)

# Perdi algumas informações nessas transformações: 
# em alguns casos, valor era missing, em outros =0. Agora, todos esses são 0.



ministerios_ano <- full_join(siop, pep) %>% full_join(bep) %>%
  rename(id_org_unica = id_org_limpo) %>% 
  left_join(orgs_ministeriais)



save_updated <- function(nome_base){
  write.csv2(get(paste0(nome_base), globalenv()),
             paste0(nome_base,"-",Sys.Date(),".csv"), row.names = F
  )
}
save_updated("ministerios_ano")


# Tem vários cuja série histórica fica errada pq o nome muda em um mas não no outro
# (ex.: MFaz no BEP, mas M Econ no PEP e SIOP)
# Isso vai ter que ser corrigido de alguma forma
# provavelmente levando em conta a base que montei de mudança de nomes