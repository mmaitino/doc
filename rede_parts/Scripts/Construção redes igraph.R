library(tidyverse)
library(igraph)
library(lubridate)

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
delegs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/deleg-2022-06-29.csv", 
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
# Criando a matriz de incidência -----------

# Para gerar projeção, é preciso primeiro criar uma matriz de incidência

incidence_matrix <- matriz_part_evento %>% 
  select(id_individuo, id_evento) %>% 
  table() #conta se existem dados naquela combinação individuo-conf
class(incidence_matrix) <- "matrix" # converte de df pra matriz


# Se quiser rodar rede bipartite, esse arquivo é suficiente.
# bipartite_net <- igraph::graph.incidence(incidence_matrix,
#                                          mode = c("all"))
#   # mudando shape a depender do tipo de nó
#   V(bipartite_net)$shape <- ifelse(
#     V(bipartite_net)$type == FALSE, "circle", "square")
#   # plot simples
#   plot(bipartite_net, vertex.label.cex = .6, 
#        vertex.label.color = "black")


rm(matriz_part_evento) #remover para liberar memória

# Criar projeções unipartite -----

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


# Para gerar a matriz evento-evento, inverte-se a ordem na multiplicação
# projeção eventos como nós, coparticipação como aresta e peso
matriz_eventos <- t(incidence_matrix) %*% incidence_matrix
deleg_size_event <- diag(matriz_eventos)
diag(matriz_eventos) <- 0

# rede: projeção eventos como nós, coparticipação como aresta e peso
eventNet <- graph.adjacency(matriz_eventos, mode = "undirected",
                            weighted = TRUE)
#plot(eventNet, vertex.label.cex = .6, vertex.label.color = "black")

# Incluindo atributos nas redes ------

individuos_unico <- individuos %>% select(-nome) %>% distinct()

set_vertex_attr(personNet, "name", index = V(personNet),
                as.character(individuos_unico$nome_padrao))

# como adicionar o ano do evento como atributo aqui??


eventos_comdeleg <- eventos %>% filter(
  id_evento %in% lista_eventos_comdeleg)

eventNet <- set_vertex_attr(eventNet, "confid", index = V(eventNet),
                as.character(eventos_comdeleg$conf))
eventNet <- set_vertex_attr(eventNet, "conference", index = V(eventNet),
                            as.character(eventos_comdeleg$conference))
eventNet <- set_vertex_attr(eventNet, "tema", index = V(eventNet),
                as.character(eventos_comdeleg$tema))
eventNet <- set_vertex_attr(eventNet, "ano", index = V(eventNet),
                as.character(eventos_comdeleg$ano))


# Nota: esse método ainda não permite incluir atributos dinâmicos
# Seria necessário ainda incluir o ANO como atributo das arestas.
# Idealmente, teríamos, ainda, a org que muda, o que poderia ser
# incluído com o id indevento
# Como funcionaria nesse caso? Rever o script anterior.



# Organizando visualização ----

V(eventNet)$color <- case_when(
  V(eventNet)$tema == "Ozônio" ~ "dodgerblue3",
  V(eventNet)$tema == "Biodiversidade" ~ "darkgreen",
  V(eventNet)$tema == "Biodiversidade - Espécies" ~ "paleturquoise3",
  V(eventNet)$tema == "Lixo tóxico e químicos" ~ "slateblue3",
  V(eventNet)$tema == "Florestas" ~ "darkseagreen3",
  V(eventNet)$tema == "Oceano" ~ "darkblue",
  V(eventNet)$tema == "Grandes conferências ONU" ~ "darkorange2",
  V(eventNet)$tema == "Desertificação" ~ "goldenrod",
  V(eventNet)$tema == "Governança ambiental" ~ "coral4",
  V(eventNet)$tema == "Biodiversidade - UNESCO" ~ "pink4",
  V(eventNet)$tema == "Clima" ~ "firebrick3"
                              )

V(eventNet)$label <- V(eventNet)$confid

# só aparece nome dos 20 top between
top_betw <- betweenness(eventNet) %>% sort(decreasing = T) %>% head(20)
V(eventNet)$label <- if_else(V(eventNet)$name %in% names(top_betw),
                             as.character(V(eventNet)$confid), NULL)

Layout <- layout_with_graphopt(eventNet)

plot(eventNet, vertex.size = betweenness(eventNet)/400,
     vertex.label.cex = .2, vertex.label.color = V(eventNet)$color,
     layout = Layout)

