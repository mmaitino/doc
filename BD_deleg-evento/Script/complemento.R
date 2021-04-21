library(tidyverse)
library(here)

# Puxar o historico
delegs <- read_delim(here("Historico","delegs_v1.2.csv"),
                     ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)
lista_orgs <- read_delim(here("Historico","lista_orgs_v2.2.csv"), 
                         ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                         trim_ws = TRUE)
class_orgs <- read_delim(here("Historico","classificação_orgs v1.1.csv"),
                         ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                         trim_ws = TRUE)

# Puxar os complementos
complemento_deleg <- read_delim("complemento_delegs_v1.1.csv", 
                                ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)
complemento_orgs <- read_delim("complemento_orgs_v1.1.csv", locale = locale(encoding = "UTF-8"), 
                               ";", escape_double = FALSE, trim_ws = TRUE)
complemento_class <- read_delim("complemento_classorgs_v1.csv",
                                ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"),
                                trim_ws = TRUE)

# Atualizar cada um dos arquivos
updated_deleg <- bind_rows(delegs, complemento_deleg)
updated_orgs <- bind_rows(lista_orgs, complemento_orgs)
updated_class <- bind_rows(class_orgs, complemento_class)

#Salvar versão atual incluindo data de atualização no nome
save_updated <- function(nome_base){
  write.csv2(get(paste0("updated_", nome_base), globalenv()),
             paste0(nome_base,"-",Sys.Date(),".csv")
  )
}

save_updated("deleg")
save_updated("orgs")
save_updated("class")
