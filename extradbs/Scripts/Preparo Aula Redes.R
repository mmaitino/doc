library(tidyverse)

# Open files from Castro database (.dta) -----
relationdb <- haven::read_dta(here::here("dbs","Castro", "ENB_relationships.dta"))


# "For the ENB (inside) dataset, we code a tie, reflecting cooperative diplomatic interactions inside the negotiations,
# between countries i and j in year t if there was at least one strongly cooperative negotiation
# interaction (i.e. “on behalf of” or “support”, see Table 1) between those two countries during
# that year’s negotiation sessions. (...) Due to the high level of reciprocity in the POLCLIMATE dataset, we treat the dataset
# as de facto undirected. In contrast, the ENB dataset has a lower level of reciprocity, which is
# why we treat it as directed. In addition, both datasets are dichotomized." 
# (Castro and Kammerer 2019, p. 13)

# Transformar o banco em EDGE LIST e NODE LIST ------------
# Isso vai deixar as coisas mais claras pra explicar os dados na aula.
# A base usa ids diferentes para o country1 e country2. Portanto tenho que mudar os ids primeiro.
node_list <- tibble(node_id = NA,
                    country_name = c(relationdb$Country1, relationdb$Country2) %>% unique) %>% 
  arrange(country_name)
node_list$node_id <- 1:nrow(node_list)

node_list <- node_list %>% mutate(
  sigla = countrycode::countrycode(country_name, origin = 'country.name.en', destination = 'iso3c')
) %>% mutate(sigla = if_else(is.na(sigla), country_name, sigla)) %>% 
  mutate(
    subregiao = countrycode(country_name, origin = 'country.name.en', destination = 'un.regionsub.name'),
    flag = countrycode(country_name, origin = 'country.name.en', destination = 'unicode.symbol')
    ) %>% mutate(flag = if_else(is.na(flag), sigla, flag))

### Adicionar características dos países ------
Class_países <- read_excel("~/Doutorado/Cursos oferecidos e falas/Redes R/Class países.xlsx",
                           na = "NA") %>% 
  rename(country_name = país)

node_list <- left_join(node_list, Class_países)

# Acho que seria interessante ter, pelo menos, nível de desenvolvimento e continente.
# Poderia incluir as siglas pra funcionar como label também. Acho que poupa trabalho lá na frente.

edge_list <- relationdb %>% select(c(
  obs_id,
  Country1, Country2, #nodes
  relation, relation2, #tipo de interação (categórica)
  cooperation, #vamos usar só as interações de coop, + facil filtrar
  # características da aresta também entram aqui:
  year, e_date, topic #topic é categórica
  ))

edge_list <- edge_list %>% filter(cooperation == 1) %>%
  select(-c(cooperation, relation, relation2))

# Agora incluo os ids dos países na edge_list.
edge_list <- edge_list %>% rename(country_name = Country1) %>% 
  left_join(node_list %>% select(country_name, node_id) %>% rename(from = node_id)) %>%
  select(-c(country_name)) %>% 
  rename(country_name = Country2) %>% 
    left_join(node_list %>% select(country_name, node_id) %>% rename(to = node_id)) %>% 
  select(-c(country_name)) %>% 
  rename(edge_id = obs_id)


edge_list <- edge_list %>% relocate(from, to) # importante ter a ordem certa em alguns programas

edge_list <- edge_list %>% mutate(
  negotiation_phase = cut(year, c(1994, 1997, 2001, 2006, 2013), 
                          labels = c('pre-kyoto', 'regras de kyoto', 
                                     'implementação de kyoto', 'negociações de novo acordo'))
  
)
# VALERIA A PENA SIMPLIFICAR A EDGE LIST -> RETIRAR OS TIPOS DE EDGE, SOMAR E TRANSFORMAR EM PESO
# write.csv(edge_list, "lista_arestas.csv")
# write.csv(node_list, "lista_vertices.csv")

# Criar uma rede a partir da edge_list e da node_list -------------
library(igraph)

# Como redes exigem muito da computação, vamos usar só um subconjunto dos dados:
# as interações de negociação ocorridas em 2009 e que falam sobre mitigação (topic 2)
mitig_topics <- c(2, 13, 14)


filt_edges <- edge_list %>% filter(
                                   # negotiation_phase == 'pre-kyoto' &
                                   (negotiation_phase == 'regras de kyoto' |
                                   negotiation_phase == 'implementação de kyoto') &
                                   # negotiation_phase == 'negociações de novo acordo' &
                                      topic %in% mitig_topics
) %>% select(from, to)
# somar os edges duplicados para incluir isso como peso
filt_edges <- filt_edges %>% group_by(from, to) %>% summarise(weight = n())

# O pacote igraph oferece muitas formas de construir uma rede. 
# Uma das mais fáceis de compreender é usando uma lista de arestas (em inglês, edge list). 
# (Descrever) (Descrever também a lista de vértices/node list e a utilidade)
g <- graph_from_data_frame(filt_edges, directed = TRUE, vertices = node_list)
# g <- graph_from_data_frame(edge_list, directed = TRUE, vertices = node_list)


plot(g, edge.arrow.size=.2, vertex.size = 3, vertex.label = V(g)$sigla)




# plot(network,
#      vertex.color = rgb(0.8,0.2,0.2,0.9),           # Node color
#      vertex.frame.color = "Forestgreen",            # Node border color
#      vertex.shape=c("circle","square"),             # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
#      vertex.size=c(15:24),                          # Size of the node (default is 15)
#      vertex.size2=NA,                               # The second size of the node (e.g. for a rectangle)
#      vertex.label=LETTERS[1:10],                    # Character vector used to label the nodes
#      vertex.label.color=c("red","blue"),
#      vertex.label.family="Times",                   # Font family of the label (e.g.“Times”, “Helvetica”)
#      vertex.label.font=c(1,2,3,4),                  # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
#      vertex.label.cex=c(0.5,1,1.5),                 # Font size (multiplication factor, device-dependent)
#      vertex.label.dist=0,                           # Distance between the label and the vertex
#      vertex.label.degree=0 ,                        # The position of the label in relation to the vertex (use pi)
# )
# 

# https://r-graph-gallery.com/248-igraph-plotting-parameters.html


# Limpar a rede: manter apenas os países com conexões -------------
V(g)$label <- V(g)$sigla
g2 <- delete_edges(g2, E(g2)[weight < mean(weight)])
g2 <- delete.vertices(g, degree(g)==0)
# versão mais limpa
g2 <- delete.vertices(g2, degree(g2) <= mean(degree(g2)))

plot(g2, edge.arrow.size=.2, vertex.size = 3)
# g3 <- delete.vertices(g, degree(g) <= 1)
# plot(g3, vertex.label = V(g)$sigla, edge.arrow.size=.2,
#      vertex.size = 1, vertex.label.cex = 0.8)

# incluir cor por grau de desenvolvimento
# cinco categorias
dicionario_cores1 <- data.frame(categoria = unique((V(g2)$renda_bancomundial)),
                                cores = c("gray80", "#b174ad", "#068FFF", "#e2525f", "#ff0000")
                                        #  "darkolivegreen", "darkolivegreen1", "tomato4", "tomato2")
                                )

g2 <- set_vertex_attr(g2, "cor_renda",
                value = dicionario_cores1$cores[match(V(g2)$renda_bancomundial, dicionario_cores1$categoria)]
                                                     )

# RColorBrewer::brewer.pal(7, "Set3")

dicionario_cores2 <- data.frame(categoria = unique((V(g2)$continente)),
                                cores = c("gray80", "#8DD3C7", "#FFFFB3", "#BEBADA",
                                          "#FB8072", "#80B1D3", "#FDB462", "#B3DE69")
)

g2 <- set_vertex_attr(g2, "cor_continente",
                value = dicionario_cores2$cores[match(V(g2)$continente, dicionario_cores2$categoria)]
)

# Organizar o layout do gráfico -------------
V(g2)$grau <- degree(g2, mode = 'all', normalized = T)


# Valeria explicar brevemente qual a lógica de cada alg
layout_kk <- layout_with_kk(g2, weights = 1/E(g2)$weight) #Kamada Kawai
# kk usa peso como 'larger values will result longer edges'. no nosso caso, peso é força, nao custo.
layout_fr <- layout_with_fr(g2) #Frucherman-Reingold
layout_auto <- layout_nicely(g2) 


plot(g2, layout = layout_kk, 
     edge.arrow.size=0.1, 
     vertex.size = 5*V(g2)$grau, vertex.color = V(g2)$cor_renda,
     vertex.label.cex = 0.8, vertex.label.color = 'black')

plot(g2, layout = layout_fr, edge.arrow.size=.2,
     vertex.size = 10*V(g2)$grau, vertex.color = V(g2)$cor_renda,
     vertex.label.cex = 0.8, vertex.label.color = 'black')


plot(g2, layout = layout_auto, edge.arrow.size=0.1,
     vertex.size = 10*V(g2)$grau, vertex.color = V(g2)$cor_renda,
     vertex.label.cex = 0.8, vertex.label.color = 'black')
