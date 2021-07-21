library(tidyverse)
library(here)
# setwd(here("Doutorado","Dados", "Listas participantes"))

# importar o complemento das delegs e a versao atual de lista_orgs

# complementos <- complemento_delegs_v2_1
# lista_orgs <- lista_orgs_v2_2


#cria comb 
lista_orgs <- lista_orgs %>% mutate(comb = paste(org_sujo, 
                                                 org_detalhe_sujo))

complementos <- complementos %>% mutate(
  comb = paste(org, org_detalhe)
)

return_id_dupla <- function(i, df_complemento = complementos, 
                            df_orig = lista_orgs){
  if(df_complemento$comb[i] %in% df_orig$comb){
    return(lista_orgs[df_complemento$comb[i] == df_orig$comb,"id_org_dupla"])
  }else{
    return(NA)
  }
}


# Incluir o id_org_dupla --------
  # Incluir os j? existentes
complementos$id_org_dupla <- unlist(sapply(1:nrow(complementos), return_id_dupla))

  # Criar os novos ids
ultimo_id <- lista_orgs$id_org_dupla %>% max() #ultimo id usado
combs_parcial <- complementos %>% filter(is.na(id_org_dupla)==T) %>%
  select(comb, id_org_dupla) %>% distinct()
  
combs_parcial <-  mutate(combs_parcial,
                         id_org_dupla = row_number() + ultimo_id
                         ) 

  # Incluir os novos ids na lista geral
incluir_id <- function(numero_row){
  if(is.na(complementos[numero_row,]$id_org_dupla)){#olhar para os NA
    return(combs_parcial[combs_parcial$comb == complementos[numero_row,]$comb,
                         "id_org_dupla"])
  }else{
    return(complementos[numero_row,]$id_org_dupla)
  }
  
}

complementos$id_org_dupla <- unlist(sapply(1:nrow(complementos), incluir_id))


# retirar comb
complementos$comb <- NULL

# Salvar com o id_org_dupla
# new_vers <- "2.2"
write.csv2(complementos, 
           str_replace("complemento_delegs_vNUMEROVERSAO.csv",
                       "NUMEROVERSAO", new_vers)
)




# *************************************************



# Complemento planilha orgs ------

# Retirar apenas as orgs e detalhe p/ fazer complemento orgs
complementos %>% select(org, org_detalhe, id_org_dupla) %>% 
rename(org_sujo = org, org_detalhe_sujo = org_detalhe) %>% 
  distinct() -> complemento_orgs


#ARRUMAR ESSA GAMBIARRA NAS PR?X ATUALIZA??ES (FAZER UM JOIN S?)
complemento_orgs <- left_join(complemento_orgs, lista_orgs)
complemento_orgs$comb <- NULL

# como fiz em dois momentos, um com tudo, tive que fazer umas altera??es nesse segundo join usando s? os sujos
# complemento_orgs <- complemento_orgs %>% mutate(org_sujo = stringr::str_trim(org_sujo))

df <- lista_orgs %>% select(c(org_sujo, org_limpo, id_org_unica)) %>% distinct
df <- df %>% filter(is.na(org_sujo) == F)#retirar valores org_sujo = NA p/ evitar erros
# df <- df %>% mutate(org_sujo = stringr::str_trim(org_sujo))

complemento_orgs <- complemento_orgs %>% select(-c(org_limpo, id_org_unica)) %>% 
  left_join(df, by = "org_sujo")

# inclusao do id_org_dupla = 24 (sem informa??o), que foi eliminado ao tirar os sujos = NA
complemento_orgs[complemento_orgs$id_org_dupla==24,]$org_limpo <- "Sem informação"
complemento_orgs[complemento_orgs$id_org_dupla==24,]$id_org_unica <- 565



# write.csv2(complemento_orgs, "complemento_orgs_v[NUMEROVERSAO].csv")
new_vers <- "2"
write.csv2(complemento_orgs, 
           str_replace("complemento_orgs_vNUMEROVERSAO.csv",
                       "NUMEROVERSAO", new_vers)
)


# Incluir id_org_unica
