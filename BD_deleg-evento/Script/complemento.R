library(tidyverse)
library(here)

# Puxar o historico
delegs <- read_delim(here("Historico","deleg-2021-04-21-corr.csv"),
                     ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)
lista_orgs <- read_delim(here("Historico","orgs-2021-04-21.csv"), 
                         ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)
class_orgs <- read_delim(here("Historico","class-2021-04-21.csv"),
                         ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)

# Puxar os complementos
complemento_deleg <- read_delim("complemento_delegs_v2.2.csv", 
                                ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)
complemento_orgs <- read_delim("complemento_orgs_v2.1.csv", locale = locale(encoding = "UTF-8"), 
                               ";", escape_double = FALSE, trim_ws = TRUE)
complemento_class <- read_delim("complemento_classorgs_v2.csv",
                                ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"),
                                trim_ws = TRUE)

# Atualizar cada um dos arquivos
updated_deleg <- bind_rows(delegs, complemento_deleg)
updated_orgs <- bind_rows(lista_orgs, complemento_orgs)
updated_class <- bind_rows(class_orgs, complemento_class)


# Testes antes de salvar: IDs são únicos? ------------
teste_id_dupla <- function(db = updated_deleg){

  if( db %>% select(org, org_detalhe) %>% 
      distinct() %>% nrow() == 
      db %>% select(id_org_dupla) %>% distinct() %>% nrow()){
    print("Teste bem sucedido")} else {print("Problema em id_org_dupla")}
}

teste_id_unica <- function(db = updated_orgs){
  if( db %>% select(org_limpo) %>% distinct() %>% nrow() == 
      db %>% select(id_org_unica) %>% distinct() %>% nrow()){
    print("Teste bem sucedido")} else {print("Problema em id_org_unica")}
}

teste_id_dupla()
teste_id_unica()

# Se falha, identificar o problema





# #### usado p/ id erros na atualização do compl_delegs 2.1 (jul 21) ####
# delegs %>% select(org, org_detalhe) %>% distinct %>% nrow
# delegs %>% select(id_org_dupla) %>% distinct %>% nrow

# # vetor com id_dupla coincidentes
# int <- intersect(delegs$id_org_dupla, complemento_deleg$id_org_dupla)
# # planilha com os erros e fonte
# erros <- bind_rows(
#   delegs %>% mutate(fonte = "orig") %>%  filter(id_org_dupla %in% int),
#   complemento_deleg %>% mutate(fonte = "comp") %>% filter(id_org_dupla %in% int)
# ) %>% mutate(comb = paste0(org, org_detalhe)) %>% 
#   select(comb, id_org_dupla, fonte) %>% distinct
# 
# 
# # o texto org e org_detalhe é o mesmo para cada id_org_dupla?
# teste_identico <- function(nrow, db = erros){
#   linha = db[nrow,]
#   db %>% filter(id_org_dupla == linha$id_org_dupla & fonte != linha$fonte) %>% 
#     pull("comb") -> comb_compl
#   linha$comb == comb_compl
# }
# 
# for(i in 1:nrow(erros)){
#   erros$idcorreto[i] <- teste_identico(i)
# } 

# idcorreto == F são os errados. 
# Necessário corrigir em complemento_deleg e identificar origem do erro.

# código usado para detectar qual era a comb duplicada, ie, COMB -> 2 ids (07/21)
# list_comb <- delegs %>% mutate(comb = paste0(org, org_detalhe)) %>% 
#   pull(comb) %>% unique()
# 
# db <- delegs %>% mutate(comb = paste0(org, org_detalhe)) %>%
#   select(comb, id_org_dupla) %>% 
#   distinct
# 
# teste_combduplicada <- function(i, data = db){
#   teste <- data %>% filter(comb == list_comb[i]) %>% pull(id_org_dupla)
#   length(teste) != 1
# }
# 
# new_df <- tibble(
#   comb = list_comb,
#   tst = unlist(lapply(seq_along(list_comb), teste_combduplicada))
# )
# 




# Verificar também se encoding correto -----------
View(updated_class)
View(updated_deleg)
View(updated_orgs)

#Salvar versão atual incluindo data de atualização no nome ----------
save_updated <- function(nome_base){
  write.csv2(get(paste0("updated_", nome_base), globalenv()),
             paste0(nome_base,"-",Sys.Date(),".csv")
  )
}

save_updated("deleg")
save_updated("orgs")
save_updated("class")

