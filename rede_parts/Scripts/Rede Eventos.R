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

######### PARTE 2: GERANDO A REDE EVENTOS ######################

# Criando a matriz de incidência -----------
# Para gerar projeção, é preciso primeiro criar uma matriz de incidência
incidence_matrix <- matriz_part_evento %>% 
  select(id_individuo, id_evento) %>% 
  table() #conta se existem dados naquela combinação individuo-conf
class(incidence_matrix) <- "matrix" # converte de df pra matriz

# Criar projeção unipartite -----

# Para gerar a projeção, 
# a matriz de incidência deve ser multiplicada por sua transposta
# Esse é o Cross-Product Method com multiplicação de matriz manual

# Para gerar a matriz evento-evento, inverte-se a ordem na multiplicação
# projeção eventos como nós, coparticipação como aresta e peso
matriz_eventos <- t(incidence_matrix) %*% incidence_matrix
deleg_size_event <- diag(matriz_eventos)
diag(matriz_eventos) <- 0

# rede: projeção eventos como nós, coparticipação como aresta e peso
eventNet <- graph.adjacency(matriz_eventos, mode = "undirected",
                            weighted = TRUE)

######### PARTE 3: INCLUINDO ATRIBUTOS NA REDE ######################
# Como são todos atributos de vértice e da planilha eventos
# o mais fácil é um join na nodelist
V(eventNet)$id_evento <- as.integer(V(eventNet)$name) #id_evento
node_list <- as_data_frame(eventNet, "vertices")
edge_list <- as_data_frame(eventNet, "edges")

node_list <- left_join(node_list, eventos)

######### PARTE 4: SALVANDO OS DADOS ######################
write.csv2(node_list, paste0("eventos", "nodelist", 
                             Sys.Date(),".csv")
)

write.csv2(edge_list, paste0("eventos", "edge_list", 
                             Sys.Date(),".csv")
)

######### PARTE 5: REFAZENDO A REDE ######################

# Refazendo a rede
V(eventNet)$name <- as.character(V(eventNet)$conf)

eventNet <- graph_from_data_frame(edge_list, directed = FALSE,
                                  vertices = node_list
)




# # Organizando visualização ----
# 
# V(eventNet)$color <- case_when(
#   V(eventNet)$tema == "Ozônio" ~ "dodgerblue3",
#   V(eventNet)$tema == "Biodiversidade" ~ "darkgreen",
#   V(eventNet)$tema == "Biodiversidade - Espécies" ~ "paleturquoise3",
#   V(eventNet)$tema == "Lixo tóxico e químicos" ~ "slateblue3",
#   V(eventNet)$tema == "Florestas" ~ "darkseagreen3",
#   V(eventNet)$tema == "Oceano" ~ "darkblue",
#   V(eventNet)$tema == "Grandes conferências ONU" ~ "darkorange2",
#   V(eventNet)$tema == "Desertificação" ~ "goldenrod",
#   V(eventNet)$tema == "Governança ambiental" ~ "coral4",
#   V(eventNet)$tema == "Biodiversidade - UNESCO" ~ "pink4",
#   V(eventNet)$tema == "Clima" ~ "firebrick3"
# )
# 
# V(eventNet)$label <- V(eventNet)$conf
# Layout <- layout_with_graphopt(eventNet)
# 
# plot(eventNet, vertex.size = 5,
#      vertex.label.cex = .2, layout = Layout)
