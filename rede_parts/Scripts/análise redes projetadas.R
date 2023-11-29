library(tidyverse)
library(igraph)
library(readxl)
# Abrindo arquivo de redes projetadas p/ eventos ----

nodelist_7089 <- read_excel("Redes/nodelistevento70-89.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "numeric", "skip", "skip"))
edgelist_7089 <- read_excel("Redes/edgelistevento70-89.xlsx") %>% 
  rename(weight = Weight)

nodelist_9099 <- read_excel("Redes/nodelistevento90-99.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "numeric", "skip", "skip"))
edgelist_9099 <- read_excel("Redes/edgelistevento90-99.xlsx") %>% 
  rename(weight = Weight)

# edgelistpart_9099 <- read_excel("Redes/edgelistparticipante90-99.xlsx")

# Transformando em objetos igraph
graph <-graph_from_data_frame(edgelist_9099, directed = F, 
                              vertices = nodelist_9099)

# graph_indiv <- graph <-graph_from_data_frame(edgelistpart_9099, directed = F)


# Calculando medidas de topografia ----






# Identificando nós mais centrais ----



# clustering ----
cluster_net <- cluster_fast_greedy(graph)
sizes(cluster_net)
plot(cluster_net, graph, layout = layout_with_fr)


library(ape)
dendPlot(cluster_net, mode = 'phylo')


# Visualização ----
# Fruchterman and Reingold layout [spring embedder]
plot(graph, layout = layout_with_fr)
#Kamada and Kawai [energy placement]
plot(graph, layout = layout_with_kk)

#DrL method, appropriate for large networks
layout_drl <- layout_with_drl(graph_indiv)
plot(graph_indiv, layout = layout_drl, vertex.size = 10, vertex.label = NA)


