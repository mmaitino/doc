library(tidyverse)
library(here)
setwd(here("Doutorado","Dados", "Listas participantes"))
# 
# # importar o complemento das delegs e a versao atual de lista_orgs
# 
complementos <- complemento_delegs_v5
lista_orgs <- orgs_2023_07_25


#cria comb 
lista_orgs <- lista_orgs %>% mutate(comb = paste(org_sujo, 
                                                 org_detalhe_sujo))
lista_orgs <- lista_orgs %>% distinct()

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
  # Incluir os ja existentes
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



# Testar se gerou algum id_org_dupla errado -----
# checo id_org_dupla presente em complementos e na listaorgs. 
# Se p/ mesmo id o texto for diferente entre as bases, temos problemas.
teste_duplicados <- function(delegs = complementos, ls = lista_orgs){
  int <- intersect(delegs$id_org_dupla, ls$id_org_dupla)
  db <- bind_rows(
    delegs %>% mutate(fonte = "comp") %>%  filter(id_org_dupla %in% int) %>% 
      select(id_org_dupla, comb, fonte),
    ls %>% mutate(fonte = "orgs") %>% filter(id_org_dupla %in% int) %>% 
      select(id_org_dupla, comb, fonte)
  ) %>% distinct
  
  teste_identico <- function(nrow, data = db){
  linha = data[nrow,]
  data %>% filter(id_org_dupla == linha$id_org_dupla & fonte != linha$fonte) %>% 
    pull("comb") -> comb_compl
  linha$comb == comb_compl
  }
  
  for(i in 1:nrow(db)){
    db$idcorreto[i] <- teste_identico(i)
  }
  db
}

duplicados <- teste_duplicados()
all(duplicados$idcorreto == T) # Se T, pode salvar

# retirar comb
complementos$comb <- NULL
# Salvar com o id_org_dupla ------------
# new_vers <- "5.1"
# write.csv2(complementos,
#            str_replace("complemento_delegs_vNUMEROVERSAO.csv",
#                        "NUMEROVERSAO", new_vers),
#            row.names = F
# )




# *************************************************



# Complemento planilha orgs ------

# Retirar apenas as orgs e detalhe p/ fazer complemento orgs
complementos %>% select(org, org_detalhe, id_org_dupla) %>% 
rename(org_sujo = org, org_detalhe_sujo = org_detalhe) %>% 
  distinct() -> complemento_orgs


# Adicionar os dados de org limpo para os id_org_dupla ja presentes na base
complemento_orgs <- left_join(complemento_orgs, lista_orgs)
complemento_orgs$comb <- NULL


# Adicionar os dados de org limpo para os casos de ORG ja presentes na base
# [nao ha a dupla, mas ha a ORG]

# trim org_sujo p/ evitar erros por whitespace e afins
complemento_orgs <- complemento_orgs %>% mutate(org_sujo = stringr::str_trim(org_sujo))

# adicina os dados ORG UNICA
df <- lista_orgs %>% select(c(org_sujo, org_limpo, id_org_unica)) %>%
  distinct

# retirar valores org_sujo = NA p/ evitar erros
df <- df %>% filter(is.na(org_sujo) == F)
df <- df %>% mutate(org_sujo = stringr::str_trim(org_sujo))

# antes do join, ignorar casos qdo org_sujo d? origem a mais de um org_limpo
# (isso pode ocorrer qdo h? varia??o devido a org_detalhe)
lista_orgsujo_duplicado <- df[duplicated(df$org_sujo),] %>% pull(org_sujo)
df <- df %>% filter(org_sujo %in% lista_orgsujo_duplicado == F)

# Incluir os id_org_unica dos demais casos
complemento_orgs <- complemento_orgs %>% select(-c(org_limpo, id_org_unica)) %>% 
  left_join(df, by = "org_sujo")

# inclusao do id_org_dupla = 24 (sem informa??o), que foi eliminado ao tirar os sujos = NA
complemento_orgs[complemento_orgs$id_org_dupla==24,]$org_limpo <- "Sem informação"
complemento_orgs[complemento_orgs$id_org_dupla==24,]$id_org_unica <- 565



# Salvar complemento_orgs
# 
# new_vers <- "5"
# write.csv2(complemento_orgs,
#            str_replace("complemento_orgs_vNUMEROVERSAO.csv",
#                        "NUMEROVERSAO", new_vers),
#            row.names = FALSE
# )




# Correção jul/21 (ids_dupla gerado errado. corrigir no compl orgs já limpo tb) ----
# comp_orgs <- complemento_orgs_v2_1
# comp_orgs$id_org_dupla <- NULL
# comp_orgs <- comp_orgs %>% distinct %>% 
#   mutate(comb = paste0(org_sujo, org_detalhe_sujo))
# 
# id_correto <- complementos %>% mutate(comb = paste0(org, org_detalhe)) %>% 
#   select(comb, id_org_dupla) %>% distinct
# 
# comp_orgs <- left_join(comp_orgs, id_correto, by = "comb")
# com_orgs$comb <- NULL
# 
# write.csv2(comp_orgs,
#            str_replace("complemento_orgs_vNUMEROVERSAO.csv",
#                                              "NUMEROVERSAO", new_vers)
# )
