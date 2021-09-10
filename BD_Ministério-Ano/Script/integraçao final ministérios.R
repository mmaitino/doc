library(tidyverse)

# importação das bases --------------------
ministerio_ano <- read_delim("~/Doutorado/controle_doc/BD_Ministério-Ano/Planilhas-dados/ministerios_ano-corr.csv",
                             ";", escape_double = FALSE,
                             locale = locale(encoding = "ISO-8859-1"), 
                             trim_ws = TRUE)
part_min_ano <- read_delim("~/Doutorado/controle_doc/BD_Ministério-Ano/Planilhas-dados/part_min_ano-2021-08-20.csv",
                           ";", escape_double = FALSE,
                           col_types = cols(org_limpo = col_skip()), 
                           locale = locale(encoding = "ISO-8859-1"), 
                           trim_ws = TRUE)

duracao <- read_delim("~/Doutorado/controle_doc/BD_Ministério-Ano/Planilhas-dic/min_duracao_2021-09-08.csv",
                      ";", escape_double = FALSE, 
                      col_types = cols(antecessor = col_skip(),
                                       nome_org_original = col_skip(), 
                                       org_limpo = col_skip(), 
                                       sucessor = col_skip()), locale = locale(),
                      trim_ws = TRUE)

min_intl <- read_delim("~/Doutorado/controle_doc/BD_Ministério-Ano/Planilhas-dados/min_intl-2021-09-09.csv",
                       ";", escape_double = FALSE, 
                       col_types = cols(nome_org_original = col_skip(), 
                                        org_limpo = col_skip()), locale = locale(), 
                       trim_ws = TRUE)



# Join ministerios e participaçao ------------
min_ano <- ministerio_ano
min_ano <- mutate(min_ano, id_org_ano = paste0(ano, id_org_unica))

part_min_ano <- mutate(part_min_ano, id_org_ano = paste0(ano, id_org_unica)) %>% 
  distinct() #checar cód que gerou: pq estao duplicadas as observações de part_min_ano?


# Limita observaçoes a partir de 1997 (qdo começa o BEP)
# Join a partir de part pra manter atores completos (evita um 'complete' em min_ano)
min_ano <- part_min_ano %>% filter(ano >= 1997) %>% left_join(min_ano)

# Gerar variável existência min-ano -----------
duracao <- mutate(duracao, 
                  extinçao = if_else(
                    is.na(extinçao), 
                    as.numeric(format(Sys.Date(), "%Y"))+1,
                    extinçao)
)


# Primeiro preciso identificar quando há múltiplas existencias do órgao
# Possibilidade: criar coluna 'duracao' com um vetor de números entre criaçao e ext
# Agrupar por id e adicionar os novos elementos aos vetores
durabup <- duracao



duracao <- duracao %>% rowwise(id_org_unica) %>% 
  summarise(duracao = list(criaçao:(extinçao-1))
  )

duracao %>% group_by(id_org_unica) %>% 
  summarise(duracao_completa = duracao)
names(duracao$duracao) <- duracao$id_org_unica


duracao <- duracao %>% group_by(id_org_unica) %>% summarise(
  lista_duracao = list(c(duracao)))

#.....Criada a coluna com as listas de anos duração, join com df principal ----
min_ano <- left_join(min_ano, duracao)

# Criar a coluna existencia a partir da lista
min_ano <- min_ano %>% rowwise %>% 
  mutate(
    existencia = if_else(ano %in% unlist(lista_duracao), 1, 0)
  ) %>% 
  select(-lista_duracao) #drop da coluna de lista

# Retirar ministerios que nao existem no período 1997-2019
ids_retirar <- min_ano %>% group_by(id_org_unica) %>%
  summarise(exist_count = sum(existencia)) %>% 
  filter(exist_count == 0) %>% pull(id_org_unica)

min_ano <- min_ano %>% filter(id_org_unica %in% ids_retirar == FALSE)


# Gerar variável estrutura intl - ano --------------------

# Primeiro passo é expandir o banco de dados - todos ids em todos anos
min_intl <- min_intl %>% complete(id_org_unica, ano = 1990:2020)
# Em seguida, é preciso unificar anos duplicados

# qdo há dois decretos no mesmo ano, aparece duplicado
# regra escolhida: se há ao menos uma vez 1 --> 1
min_intl <- min_intl %>% group_by(id_org_unica, ano) %>% 
  summarise(estr_intl = if_else(any(estr_intl == 1), 1, 0))

# Função para retornar estrutura nos anos faltantes
retorna_estr <- function(nrow, base = min_intl){
  id <- base[[nrow, "id_org_unica"]]
  ano_preencher <- base[[nrow, "ano"]]
  
  anos_preenchidos <- base %>% 
    filter(id_org_unica == id & is.na(estr_intl) == F) %>% pull(ano)
  if(is_empty(anos_preenchidos)){
    result_ano_preencher <- NA
    return(result_ano_preencher)
    }
  # Se ano_preencher %in% anos_preenchidos, retorna valor original
  if(ano_preencher %in% anos_preenchidos){
    base %>% 
      filter(id_org_unica == id & ano == ano_preencher) %>% 
      pull(estr_intl) -> result_ano_preencher
  # Se ano_preencher < min(anos_preenchidos), retorna NA
  }else if(ano_preencher < min(anos_preenchidos)){
    result_ano_preencher <- NA
  }else{# Demais casos:
    # Encontrar o ano anterior mais próximo de ano_preencher
    dif <- ano_preencher - anos_preenchidos #vetor de diferenças
    names(dif) <- anos_preenchidos # com o ano preenchido como factor
    which(dif[dif > 0] == min(dif[dif > 0])) %>% # dos positivos, qual o menor?
      names() %>% as.numeric() ->  ano_observado_anterior
    
    # Copiar resultado do ano próximo
    base %>% filter(id_org_unica == id & ano == ano_observado_anterior) %>% 
      pull(estr_intl) -> result_ano_preencher
  }
  result_ano_preencher
}

# adicionar em nova coluna
min_intl$full_estr <- map_dbl(1:nrow(min_intl), retorna_estr)
min_intl <- min_intl %>% rename(info_decreto = estr_intl,
                                estr_intl = full_estr)

#.....Criada a coluna estr_intl p/ todo ano, join com base geral ------
min_ano <- min_ano %>% left_join(min_intl) %>% select(-info_decreto)

# Salvar o arquivo --------------
save_updated <- function(nome_base){
  write.csv2(get(paste0(nome_base), globalenv()),
             paste0(nome_base,"-",Sys.Date(),".csv"), row.names = F
  )
}
save_updated("min_ano")




# Brincando com um primeiro modelo ------------
nunca_part <- min_ano %>% group_by(id_org_unica) %>% 
  summarise(soma_parts = sum(part_binaria)) %>%
  filter(soma_parts == 0) %>% pull(id_org_unica)

probit_part <- glm(part_binaria ~ dotacao_atual_total + dotacao_atual_invest +
                     servidores_superior + forca_trabalho, 
                   family = binomial(link = "probit"),
                   data = min_ano %>% filter(existencia ==1 & 
                                               (id_org_unica %in% nunca_part == FALSE)
                                             )
)
summary(probit_part)

#zero infl
library(pscl)
# modelo nao conseguiu estimar erro padrao sem transformar preditores
# provavel que seja um range muito grande quando comparado com o do count
zero_poisson <- zeroinfl(part_count ~ log10(1+dotacao_atual_invest) +
                           log10(1+servidores_superior),
                         data = min_ano %>% filter(existencia ==1 & 
                                                     (id_org_unica %in% nunca_part == FALSE)
                         ))
summary(zero_poisson)
