library(tidyverse)
library(igraph)
library(lubridate)

# Funções ------

gerar_bipartida <- function(){
  # Importação das bases deleg ----
  
  delegs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/deleg-2021-07-22.csv", 
                       ";", escape_double = FALSE, 
                       col_types = cols(X1 = col_skip(), cargo_deleg = col_skip(), 
                                        cargo_org = col_skip(), 
                                        desc = col_skip(), fonte = col_skip(), 
                                        nline = col_skip(), org = col_skip(),
                                        org_detalhe = col_skip(), pais = col_skip(),
                                        titulo = col_skip()), 
                       locale = locale(encoding = "ISO-8859-1"), 
                       trim_ws = TRUE)
  
  
  orgs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/orgs-2021-08-26.csv",
                     ";", escape_double = FALSE, 
                     col_types = cols(org_detalhe_limpo = col_skip(),
                                      org_detalhe_sujo = col_skip(), 
                                      org_sujo = col_skip()),
                     locale = locale(encoding = "ISO-8859-1"),
                     trim_ws = TRUE)
  
  class <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/class-2021-08-26.csv", 
                      ";", escape_double = FALSE, 
                      col_types = cols(org_limpo = col_skip()),
                      locale = locale(encoding = "ISO-8859-1"),
                      trim_ws = TRUE)
  
  
  # Inclusão da organização na base delegs
  # delegs <- left_join(delegs, orgs, by = "id_org_dupla") %>% 
  #   select(-id_org_dupla)
  
  #
  
  # Importar base de indivíduos
  individuos <- read_csv("individuos-2021-09-06.csv", 
                         col_types = cols(nome_padrao_v1 = col_skip(),
                                          nome_padrao_v2 = col_skip(), 
                                          rownum = col_skip()))
  
  # Padronização dos nomes pela base individuos
  matriz_part_evento <- left_join(delegs, individuos)
  matriz_part_evento <- matriz_part_evento %>% select(-nome) %>% 
    filter(is.na(nome_padrao) == F)
  
  
  # Remover arquivos não usados para liberar memória -----
  rm(class, delegs, individuos, orgs)
  
  matriz_part_evento
}

nomear_orgs <- function(matriz_part_evento){
  
  orgs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/orgs-2021-08-26.csv",
                     ";", escape_double = FALSE, 
                     col_types = cols(org_detalhe_limpo = col_skip(),
                                      org_detalhe_sujo = col_skip(), 
                                      org_sujo = col_skip()),
                     locale = locale(encoding = "ISO-8859-1"),
                     trim_ws = TRUE)
  matriz_part_evento <- left_join(matriz_part_evento, orgs) %>% 
    select(-id_org_dupla, id_org_unica)
  matriz_part_evento
}



filtrar_periodo <- function(matriz_part_evento, ano_inicial, ano_final){
  
  # Filtros para reduzir a base ----
  #...Incluir ano na matriz ----
  matriz_part_evento <- left_join(matriz_part_evento,
                                  select(eventos, c(conf, ano)))
  
  #bup <- matriz_part_evento
  #matriz_part_evento <- bup
  
  #...Usar apenas períodos específicos na análise ----
  matriz_part_evento <- filter(matriz_part_evento, 
                               ano %in% ano_inicial:ano_final)
  matriz_part_evento$ano <- NULL
  matriz_part_evento
}

gerar_listas_projecao <- function(matriz_part_evento, node, periodo){
  
  #...Criando matriz de incidencia ----
  if(node == "evento"){
    # evento como row, nomes como colunas
    matriz_incidencia <- matriz_part_evento %>% 
      pivot_wider(id_cols = conf, 
                  names_from = nome_padrao, values_from = nome_padrao,
                  values_fn = length, values_fill = 0)
    
    # passa evento de coluna para rowname
    matriz_incidencia <- as.data.frame(matriz_incidencia) 
    #como tibble nao funciona row.names, tem que forçar dataframe
    row.names(matriz_incidencia) <- matriz_incidencia$conf
    matriz_incidencia <- select(matriz_incidencia, -conf)
    
  } else if (node == "participante"){
    matriz_incidencia <- matriz_part_evento %>% 
      pivot_wider(id_cols = nome_padrao, 
                  names_from = conf, values_from = conf,
                  values_fn = length, values_fill = 0)
    
    # passa nome de coluna para rowname
    matriz_incidencia <- as.data.frame(matriz_incidencia) 
    #como tibble nao funciona row.names, tem que forçar dataframe
    row.names(matriz_incidencia) <- matriz_incidencia$nome_padrao
    matriz_incidencia <- select(matriz_incidencia, -nome_padrao)
  
  } else if (node == "org"){
    matriz_incidencia <- matriz_part_evento %>% 
      pivot_wider(id_cols = org_limpo, 
                  names_from = conf, values_from = conf,
                  values_fn = length, values_fill = 0)
    
    # passa nome de coluna para rowname
    matriz_incidencia <- as.data.frame(matriz_incidencia) 
    #como tibble nao funciona row.names, tem que forçar dataframe
    row.names(matriz_incidencia) <- matriz_incidencia$org_limpo
    matriz_incidencia <- select(matriz_incidencia, -org_limpo)
    
  }else{
    stop("Erro! Definir node como string `evento`, `org` ou `participante`")
  }
  
 
  
  #...Criando objeto igraph ----
  # Cria objeto de rede com o pacote 'igraph' (com opção de pesos ativada)
  rede <- graph.incidence(matriz_incidencia, weighted = TRUE)
  
  # Extrai projeção bipartite da rede
  bi.proj <- bipartite.projection(rede)
  
  # Obtém a lista de díades com pesos
  rede_1 <- cbind(get.edgelist(bi.proj$proj1), E(bi.proj$proj1)$weight)
  colnames(rede_1) <- c("Source", "Target", "Weight")
  
  #...Incluindo atributos dos nós -----
  if(node == "evento"){#incluir atributos dos eventos
    
    df <- as.data.frame(rede_1[,])
    df <- data.frame(Id = c(levels(df$Source), levels(df$Target)) %>% unique)
    
    eventos <- eventos %>% 
      rename(Id = conf) %>%
      select(Id, conference, tema, ano)
    
    df <- left_join(df, eventos)
    
    df <- df %>% mutate(ano_inicio = paste0(ano,"-01-01"),
                 ano_fim = paste0(ano, "-12-31")
                 )
    df <- select(df, c(Id, conference, tema, ano,
                       ano_inicio, ano_fim))
 
  }
  
  #### falta fazer os atributos dos participantes na função!!!
  
  
  #...Exportando os arquivos ----
  name = paste0(node, periodo)
  
  # Exporta díade com pesos
  # write.csv(rede_1, "diade_eventos90-00.csv")
  rio::export(rede_1,file = paste0("edgelist", name, ".xlsx"),
              format = "xlsx")
  if(node == "evento"){
    # Exporta vértices com atributos
    rio::export(df,file = paste0("nodelist", name, ".xlsx"),
                format = "xlsx")
  }
  

}


### |||||||||||||| EXECUÇÃO DO CÓDIGO ||||||||||||||--------------------

#...Organizar primeiro a base eventos ----
eventos <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/Historico/eventos_v3.csv",
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
eventos <- eventos %>%
  rename(#renomeia colunas
    conf = `Nome do evento`,
    conference = `Conf/Conv`,
    tema = `Regime/Tema`,
    data = Data,
    location = Locale,
    tipo_evento = `Tipo evento`,
    infMEA_list = `Lista MEA?`,
    coleta = `Coleta?`,
    proces = `Proces.?`
  ) %>%
  filter(is.na(data)==F) %>%
  select(conf, conference, tema, data) #no momento, só me interessam essas

eventos <- eventos %>% mutate(
  data = if_else(str_count(data)==4, #se falta o mês (só ano)
                 paste0(data, "-01"), #padroniza como janeiro
                 data)) %>%
  mutate(
    data = if_else(is.na(data)== F,
                   paste0(data, "-01"), #padroniza data no dia 1 do mês
                   data)
  ) %>% mutate(data = ymd(data), ano = year(ymd(data)))


#...Gerando as redes -----

gerar_bipartida() %>% 
  filtrar_periodo(1970,1989) %>% 
  gerar_listas_projecao(node = "evento", "70-89")

gerar_bipartida() %>% 
  filtrar_periodo(1990,1999) %>% 
  gerar_listas_projecao(node = "evento", "90-99")

gerar_bipartida() %>% 
  filtrar_periodo(2000,2009) %>% 
  gerar_listas_projecao(node = "evento", "00-09")

gerar_bipartida() %>% 
  filtrar_periodo(2010,2018) %>% 
  gerar_listas_projecao(node = "evento", "10-18")



gerar_bipartida() %>% filtrar_periodo(1970,2018) %>% 
  gerar_listas_projecao(node = "evento", "total")

# participantes
gerar_bipartida() %>% filtrar_periodo(1970,1989) %>% 
  gerar_listas_projecao(node = "participante", "70-89")

gerar_bipartida() %>% filtrar_periodo(1990,1999) %>% 
  gerar_listas_projecao(node = "participante", "90-99")

gerar_bipartida() %>% filtrar_periodo(2000,2009) %>% 
  gerar_listas_projecao(node = "participante", "00-09")

gerar_bipartida() %>% filtrar_periodo(2010,2018) %>% 
  gerar_listas_projecao(node = "participante", "10-18")


# antes de rodar participante com períodos maiores, possibilidades:
  # função filtrar grau p/ reduzir rede (apenas a partir de x participações?)
  # reformular p/ orgs ao inves de indivíduos
  # entender como funciona dinâmica nesse caso
      # o ano do evento pode ser atributo da aresta [aparece no ano] 
      # tb posso ter atributos dinâmicos no nó. aí tem que entender como é


# teste
gerar_bipartida() %>% nomear_orgs() %>% gerar_listas_projecao(node = "org",
                                                              "teste")





gerar_bipartida() %>% filtrar_periodo(1970, 1989) %>% nomear_orgs() %>% 
  gerar_listas_projecao(node = "org", "70-89")
gerar_bipartida() %>% filtrar_periodo(1990, 1999) %>% nomear_orgs() %>% 
  gerar_listas_projecao(node = "org", "90-99")
gerar_bipartida() %>% filtrar_periodo(2000, 2009) %>% nomear_orgs() %>% 
  gerar_listas_projecao(node = "org", "00-09")
gerar_bipartida() %>% filtrar_periodo(2010, 2018) %>% nomear_orgs() %>% 
  gerar_listas_projecao(node = "org", "10-18")



# método dicas rogério

##########################################################################
#
# Formato 1 (Condensado):
# Díades com pesos baseados no número de edges entre a mesma díade;
# Cada díada aparece apenas uma vez na lista final
#
##########################################################################

# código rogério direto - se não estiver funcionando, verificar se tem
# que colocar function 'length' no dcast
# código abaixo usa nomes como node


# 'Alargar' o formato do dataframe (wide df)
library(reshape2)
matriz_incidencia <- dcast(matriz_part_evento, nome_padrao ~ conf, 
                           value.var = "conf")

# nomeia as linhas e retira coluna de nomes
row.names(matriz_incidencia) <- matriz_incidencia$nome_padrao
matriz_incidencia <- select(matriz_incidencia, -nome_padrao)

# Cria objeto de rede com o pacote 'igraph' (com opção de pesos ativada)
rede <- graph.incidence(matriz_incidencia, weighted = TRUE)

# Extrai projeção bipartite da rede
bi.proj <- bipartite.projection(rede)

# Obtém a lista de díades com pesos
rede_1 <- cbind(get.edgelist(bi.proj$proj1), E(bi.proj$proj1)$weight)
colnames(rede_1) <- c("Source", "Target", "Weight")

# Exporta díades com pesos em formato Excel
rio::export(rede_1, file = "diade_eventos90-00", format = "xlsx")




# método inicialmente tentado
# #dos dados de afiliaçao (part-evento) --> matriz incidência
# matriz_part_evento <- as.matrix(table(matriz_part_evento))
# 
# rede_eventos <- graph.incidence(matriz_part_evento, mode = c("all"))
# plot(rede_eventos, vertex.label.cex = .5, vertex.label.color = "black")
# 
# 
# # transformar em 1-mode network (coparticipaçao em eventos)
# 
# 
# # gerar matriz coparticipação 
# # (parece que tem algum prob com o metodo) do table, mas nao sei
# inc_matrix <- matriz_part_evento %>% select(nome_padrao, conf) %>% 
#   table %>% as.matrix()
# copart_matrix <- tcrossprod(inc_matrix)
# 
# rede_parts <- graph.adjacency(copart_matrix, mode = 'undirected')
# # criar peso por múltipla coparticipação
# E(rede_parts)$weight <- count.multiple(rede_parts)
# rede_parts <- simplify(rede_parts)
# 
# # Atributos dos vértices
# V(rede_parts)$label.cex <- .6
# V(rede_parts)$size <- 6
# 
# V(rede_parts)$size <- 2
# E(rede_parts)$width <- .3
# 
# # Set edge gamma according to edge weight
# egam <- (log(E(rede_parts)$weight)+.3)/max(log(E(rede_parts)$weight)+.3)
# E(rede_parts)$color <- rgb(.5,.5,0,egam)
# 
# # plot
