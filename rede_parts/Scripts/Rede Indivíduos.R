library(tidyverse)
library(igraph)
library(lubridate)

######### PARTE 1: PREPARO DAS BASES ######################

# Gerar matriz bipartida (indivíduo-evento) -----------------

# Importar eventos e organizar
# abrir planilha eventos e organizar dados
eventos <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/Historico/eventos_v4.csv",
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
  ) %>% mutate(data = ymd(data), 
               ano = year(ymd(data)))

eventos$id_evento <- 1:nrow(eventos) #criar id numérico evento


# Importar delegs e indivíduos
delegs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/deleg-2023-07-25.csv", 
                     ";", escape_double = FALSE, 
                     col_types = cols(X1 = col_skip(), cargo_deleg = col_skip(), 
                                      cargo_org = col_skip(), 
                                      desc = col_skip(), fonte = col_skip(), 
                                      nline = col_skip(), org = col_skip(),
                                      org_detalhe = col_skip(), pais = col_skip(),
                                      titulo = col_skip()), 
                     #locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)

individuos <- read_csv("individuos-2022-07-01.csv", 
                       col_types = cols(padrao_antigo = col_skip(),
                                        nrow = col_skip()))

# criar id numérico p/ cada indivíduo e cd individuo-evento
delegs$id_indevento <- 1:nrow(delegs)

individuos %>% select(nome_padrao) %>% distinct() %>% mutate(
  id_individuo = row_number()
) %>% right_join(individuos) -> individuos


# Join das bases
matriz_part_evento <- left_join(delegs, individuos) %>% 
  select(-c(nome, id_org_dupla, nome_padrao))

matriz_part_evento <- left_join(matriz_part_evento, 
                                select(eventos, c(conf, id_evento))
) %>% select(-conf)

lista_eventos_comdeleg <- matriz_part_evento$id_evento


######### PARTE 2: GERANDO A REDE INDIVIDUOS ######################

# Criando a matriz de incidência -----------
# Para gerar projeção, é preciso primeiro criar uma matriz de incidência
# como queremos atributos dinâmicos, uso id p/ individuo em evento
# (ie, mesma pessoa tem id diferente em eventos diferentes)

incidence_matrix <- matriz_part_evento %>% 
  select(id_indevento, id_evento) %>% 
  table() #conta se existem dados naquela combinação individuo-conf
class(incidence_matrix) <- "matrix" # converte de df pra matriz

# Criar projeção unipartite -----

# Para gerar a projeção, 
# a matriz de incidência deve ser multiplicada por sua transposta
# Esse é o Cross-Product Method com multiplicação de matriz manual

matriz_individuos <- incidence_matrix %*% t(incidence_matrix)

# diagonal da matriz traz n de eventos por indivíduo
n_of_events <- diag(matriz_individuos)

# para evitar que a projeção tenha loops nos nós, diagonal é zerada
diag(matriz_individuos) <- 0

# rede: projeção indiv como nós, coparticipação como aresta e peso
personNet <- graph.adjacency(matriz_individuos, mode = "undirected",
                             weighted = TRUE)

######### PARTE 3: INCLUINDO ATRIBUTOS NA REDE ######################

# Método mais simples para incluir os atributos é editar a rede
# como Edge e Node List, não como objeto igraph.

# Incluindo atributos dos vértices -----

# Gerar nodelist

node_list <- as_data_frame(personNet, "vertices")

# Incluir ids como atributos
node_list$id_indevento <- as.integer(node_list$name)

node_list$id_individuo <- matriz_part_evento[#id indivíduo
  node_list$id_indevento==matriz_part_evento$id_indevento,
]$id_individuo

node_list$id_evento <- matriz_part_evento[#id evento
  node_list$id_indevento==matriz_part_evento$id_indevento,
]$id_evento

# Incluir atributos relevantes

  # nome padrao
individuos_unico <- individuos %>% select(-nome) %>% distinct()
node_list <- left_join(node_list, individuos_unico)

  # ano participação
node_list <- left_join(node_list,
                       select(eventos, c(id_evento, ano))) %>% 
  rename(ano_part = ano)


# Se quiser incluir gênero, tem que checar porque está falhando
# o pacote não está entendendo alguns nomes no dataframe,
# embora manualmente funcione (erro de encoding ou similar?)

#   # gênero inferido via pacote genderBR
# node_list <- node_list %>% mutate(
#   gender = genderBR::get_gender(nome_padrao)
# )


# Atributos dinâmicos do vértice ----
  
  # org no momento do evento
# Para incluir org, preciso puxar as bases correspondentes
# no momento, só temos delegs, que nos dá o id_org_dupla
# montar planilha id_indevento | id_org_dupla | id_org_unica | 
                # org_padrao | tipo_org

orgs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/orgs-2022-06-29.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(org_sujo = col_skip(), 
                                                                        org_detalhe_sujo = col_skip(), org_detalhe_limpo = col_skip()), 
                   locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE)

class <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/class-2022-06-29.csv", 
                    delim = ";", escape_double = FALSE, col_types = cols(org_limpo = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE)

chave_orgs <- left_join(select(delegs, c(id_indevento, id_org_dupla)),
                        orgs
) %>% left_join(class) %>% distinct()


# Inclusão na node list

node_list <- left_join(node_list, chave_orgs)
node_list$id_org_dupla <- NULL



# Incluindo atributos de arestas ------
edge_list <- as_data_frame(personNet, "edges")

# Incluir o id do evento
edge_list$id_indevento <- as.integer(edge_list$from)
edge_list <- left_join(edge_list, 
          select(matriz_part_evento, c(id_indevento, id_evento))
          )

# teste se há erros:
# edge_list <- edge_list %>% 
#   rename(id_indeventof = id_indevento, id_eventof = id_evento) %>% 
#   mutate(id_indevento = as.integer(to)) %>% 
#   left_join(select(matriz_part_evento, c(id_indevento, id_evento)))
# 
# edge_list %>% filter(id_evento != id_eventof) #se houver linhas, erro

# Incluir informações dos eventos
edge_list <- left_join(edge_list, eventos) %>%
  select(-c(data, id_indevento))

######### PARTE 4: SALVANDO OS DADOS ######################
write.csv2(node_list, paste0("individuos", "nodelist", 
                             Sys.Date(),".csv")
)

write.csv2(edge_list, paste0("individuos", "edge_list", 
                             Sys.Date(),".csv")
)
######### PARTE 5: REFAZENDO A REDE ######################

# personNet <- graph_from_data_frame(edge_list, directed = FALSE,
#                                    vertices = node_list
                                   # )
#conferindo atributos
    # vertex_attr_names(personNet)
    # edge_attr_names(personNet)

# Temos aqui dados para uma rede dinâmica, mas de forma estática
# Os indivíduos não têm "continuidade", são vértices distintos
# É preciso corrigir.
# O igraph, aparentemente, não é muito bom para redes dinâmicas

# Uma alternativa, se queremos a rede de indivíduos no tempo,
# seria simplesmente combinar os vértices a nível de indivíduo
# (escolhendo uma regra para combinar atributos dinâmicos)

# (uma alternativa aqui seria combinar a um nível ainda maior,
# o de organização. Isso evitaria problema da org dinâmica)

# A outra alternativa é trabalhar os dados com outro pacote ou Gephi.







# Rede dinâmica de indivíduos

# Formatar edge e node list p/ formato TSNA
# É necessário incluir ONSET (início) e TERMINUS (fim)
# p/ cada aresta e p/ cada vértice
# Na análise, posso depois alterar ou "censurar" início/fim se for o caso

# A princípio escolhemos incluir como EDGE ONSET o ANO do evento 
# e como TERMINUS o ANO SEGUINTE AO EVENTO
# Para vértice, o ONSET é o ANO DE PRIMEIRA PARTICIPAÇÃO EM EVENTO
# o TERMINUS foi incluído como o ANO DE ÚLTIMA PARTICIPAÇÃO

# análises e visualizações serão feitas também com terminus censurado