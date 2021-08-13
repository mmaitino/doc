library(tidyverse)

bep <- bep_servidores %>% rename(BEP_orgao = orgao) %>% right_join(dic_bep)
pep <- pep_superior %>% rename(PEP_orgao_superior = orgao_superior) %>% 
  select(-X1) %>% right_join(dic_pep)
siop <- siop_orcamento %>% rename(SIOP_orgao = orgao) %>% right_join(dic_siop)

ministerios_ano <- full_join(siop, pep) %>% full_join(bep) %>% 
  select(-c(SIOP_orgao, BEP_orgao, PEP_orgao_superior))



save_updated <- function(nome_base){
  write.csv2(get(paste0(nome_base), globalenv()),
             paste0(nome_base,"-",Sys.Date(),".csv"), row.names = F
  )
}
save_updated("ministerios_ano")
