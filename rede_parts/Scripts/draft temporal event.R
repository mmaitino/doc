library(network)
library(tidyverse)

# Reading files ---------
edgelist <- read_delim("eventosedge_list2022-08-24.csv", 
                       delim = ";", escape_double = FALSE, 
                       col_types = cols(...1 = col_skip()), 
                       locale = locale(encoding = "ISO-8859-1"), 
                       trim_ws = TRUE)

nodelist <- read_delim("eventosnodelist2022-08-24.csv", 
                       delim = ";", escape_double = FALSE, 
                       col_types = cols(...1 = col_skip()), 
                       locale = locale(encoding = "ISO-8859-1"), 
                       trim_ws = TRUE)

# o statnet/network não reconhece factors como atributos, 
# variáveis precisam ser character (não integer)
      nodelist <- nodelist %>% mutate(
        name = as.character(name), 
        id_evento = as.character(id_evento),
        data = as.character(data), 
        ano = as.character(ano)
      )
      
# corrigindo labels (conf)
      nodelist <- nodelist %>% 
        mutate(conf = str_replace(conf, "Prep Committee","PrepCom")) %>% 
        mutate(conf = str_replace(conf, "Ad Hoc Working Group of Legal and Technical Experts for the Preparation of a Protocol on Chlorofluorocarbons to the Vienna Convention for the Protection of the Ozone Layer, Session ",
                                  "AHWG-Oz, ")) %>% 
        mutate(conf = str_replace(conf, "(st|nd|rd|th) Extraordinary Session",
                                  "ES")) %>% 
        mutate(conf = str_replace(conf, ", Ad Hoc Working Group of Experts on Biological Diversity",
                                  " AHWG-BD"))
      
      
# criando atributo cor a partir de tema
      nodelist <- mutate(nodelist,
                         tema = chartr("ôóíçãê", "ooicae", tema)
      ) %>% 
        mutate(tema = if_else(tema == "Biodiversidade - Espécies", 
                              "Especies", tema)) %>% 
        mutate(tema = if_else(tema == "Grandes conferências ONU",  
                              "Desenvolvimento Sustentavel", tema))

palette <- rainbow(11) #cria paleta de cores
listtemas <- unique(nodelist$tema)
nodelist$color <- "placeholder"
for(i in 1:11){
nodelist[nodelist$tema == listtemas[i],]$color <- palette[i]
}
        
               


# Correcting event IDs ------
# Problema: o número de vértices no pacote networkDynamic
# é derivado do id máx de vértice identificado na edge list
# Como os ids foram criados para todos eventos (não os com deleg)
# isso vira problema grave (e.g 596 vértices ao invés de 289).
      #[depois das limpezas de ano, virou 268!]

# Antes de criar a rede dinâmica, portanto, vamos recriar os ids
# É preciso também filtrar apenas o período relevante
# (o que já deveria ter sido feito no script anterior, 
# ao salvar edge e node list) - CORRIGIR

# Mantendo apenas os eventos dos anos escolhidos
nodelist <- nodelist %>% filter(1970 <= as.integer(ano) &
                                  as.integer(ano) <= 2018)
edgelist <- edgelist %>% filter(#manter apenas qdo ambos %in% período
  edgelist$from %in% nodelist$id_evento &
    edgelist$to %in% nodelist$id_evento
)

# Criando lista IDs com delegs
ids_eventswithdeleg <- append(unique(nodelist$id_evento),
                              append(unique(edgelist$from),
                                     unique(edgelist$to))) %>% 
  unique()

ids_dictionary <- data.frame(event_gen_id = ids_eventswithdeleg,
                             vertex_id = 1:268)
# ids_dictionary$event_gen_id <- as.character(ids_dictionary$event_gen_id)



nodelist <- nodelist %>% rename(event_gen_id = id_evento) %>% 
  left_join(ids_dictionary)

#corrigindo edgelist
#corrigindo coluna from
edgelist$event_gen_id <- as.character(edgelist$from)
edgelist <- left_join(edgelist, ids_dictionary)
edgelist$from_event_gen_id <- edgelist$event_gen_id
edgelist$from <- edgelist$vertex_id
#corrigindo coluna to
edgelist$event_gen_id <- as.character(edgelist$to)
edgelist$vertex_id <- NULL
edgelist <- left_join(edgelist, ids_dictionary)
edgelist$to_event_gen_id <- edgelist$event_gen_id
edgelist$to <- edgelist$vertex_id
edgelist$vertex_id <- NULL


# Interactive dynamic graphs ------
library(ndtv)
library(networkDynamic)


# Adapt nodelist to networkDynamic format
dynamic_nodelist <- nodelist %>% 
  mutate(vertex_id = as.numeric(vertex_id)) %>% 
  mutate(onset = as.numeric(ano)) %>% 
  select(-c(name, ano)) %>% 
  mutate(terminus = onset + 1,
         onset.censored = F,
         terminus.censored = T
         ) %>% mutate(duration = terminus - onset)

dnl <- select(dynamic_nodelist, 
              c(onset, terminus, vertex_id,
                conf, conference, tema, data, color))
dnl$terminus <- 2022
  
  
# Adapt edgelist to networkDynamic format
# onset of edge is the year of latest event
# as this info was stored in nodelist, must join

dynamic_edgelist <- nodelist %>% 
  #get from_vertex year at nodelist
  mutate(from = as.numeric(vertex_id)) %>% 
  select(ano, from) %>% 
  right_join(edgelist) %>% 
  rename(anof = ano) %>% 
  select(-event_gen_id) %>% 
  #get to_vertex year at nodelist
  mutate(vertex_id = as.numeric(to)) %>% 
  left_join(select(nodelist, c(ano, vertex_id))) %>% 
  rename(anot = ano) %>% 
  #keep most recent year
  select(-vertex_id) %>% 
  mutate(anof = as.numeric(anof), anot = as.numeric(anot)) %>% 
  mutate(onset = if_else(anof>anot, anof, anot),
         terminus = 2022) %>% 
  select(-c(anof, anot)) %>% 
  #finish format
  rename(from_vertex_id = from, to_vertex_id = to) %>% 
  mutate(onset.censored = FALSE,
         terminus.censored = FALSE,
         duration = terminus - onset,
         edge.id = row_number(),
         to_vertex_id = as.numeric(to_vertex_id)
         )

del <- select(dynamic_edgelist, c(onset, terminus,
                                  from_vertex_id, to_vertex_id,
                                  weight))





# Create network ------




# Tentativa via estática --------
#### IMPORTANTE: o pacote sna, no qual networkDynamic é baseado
# tem incompatibilidades com tidyverse
# Tidyverse transforma DF em TIBBLE, o que quebra o netDyn
# Portanto, para rodar os spells, precisamos ler como dataframe!
# A ordem das colunas tb é essencial p/ funcionamento:
# Deve ser ONSET, TERMINUS, FROM/TAIL_ID, TO/HEAD_ID, depois atributos

# # rede estática
# sel <- select(del, c(from_vertex_id, to_vertex_id, weight))
# # a rede estática não está completa: statnet não lê os isolates
# # (vértices sem aresta) qdo cria via edge list
# isolates <- dnl[dnl$vertex_id %in% c(del$from_vertex_id,
#                                      del$to_vertex_id) == F,]
# sel <- bind_rows(sel,#incluí isolates como loops
#                  data.frame(from_vertex_id = isolates$vertex_id,
#                             to_vertex_id = isolates$vertex_id,
#                             weight = 0))
# 
# 
# 
# net <- network(as.data.frame(sel),
#                vertex.attr = as.data.frame(select(dnl,
#                                                   c(vertex_id,
#                                                     conf,
#                                                     conference,
#                                                     tema,
#                                                     data))),
#                loops = T,
#                directed = F,
#                ignore.eval = F)
# #incluí loops para conseguir incluir isolates, agora desativo
# set.network.attribute(net, "loops", F)
# 
# 
# # Rede dinâmica
# # não deu certo. está puxando mais edges e nao está ativando correto
# dyn2 <- networkDynamic(net,
#                        edge.spells = as.data.frame(del),
#                        vertex.spells = as.data.frame(dnl),
#                        vertex.pid = "vertex_id")
# list.vertex.attributes(dyn2)




# Tentativa direto a dinâmica -----------
dyn_net <- networkDynamic(edge.spells = as.data.frame(del),
                          vertex.spells = as.data.frame(dnl),
                          create.TEAs = TRUE
                          )

# se fizer sem a estática como base, fica direcionada ainda.
set.network.attribute(dyn_net, attrname = "directed", FALSE)

# Com TEAs, ele inclui os atributos (incluindo weight.active)
# Mas não diferencia o que era estático dos dinâmicos!
# (por isso a necessidade de incluir a estática como base)
network.vertex.names(dyn_net)<-dnl$conf

# # definir persistent id na rede (nao sei se deu certo!)
set.network.attribute(dyn_net,'vertex.pid',
                      dyn_net %v% 'vertex.names')
initialize.pids(dyn_net)


# Tenho um problema com os atributos estáticos e não consigo adicionar
# pq a ordem qdo dyn_net %v% vertex.names não corresponde à correta
# a ordem de ids (no vídeo, ao menos) é a de aparição dos vértices
# no vertex_id, é alfabética
# set.vertex.attribute(dyn_net, "conf",
#                      value = arrange(dnl, onset) %>% pull(conf),
#                      return.tea=FALSE
#                      )
# O problema é que os atributos variam conforme o slice!



# Preparing network for analysis and visualization -----

# Manter apenas o período desejado
# dyn_net <- network.extract(dyn_net, onset=1970, terminus=2018,
#                           rule="any")
# # any= só vertices ativos no período


# Imagem filmstrip
# filmstrip(dyn_net, displaylabels=F,
#           mfrow=c(1, 2),#mfrow=n row/col img
#           slice.par=list(start=1970, end=2018,
#                          interval=10, aggregate.dur=9, 
#                          rule='any')
# )


# Criando animação
# compute antes é opcional (senao calcula sozinho no render)
compute.animation(dyn_net, 
                  animation.mode = "kamadakawai",#layout
                  slice.par=list(start=1970, end=2018,
                                 interval=1, 
                                 aggregate.dur=1, rule='any'))

# Ainda não aprendi a puxar os atributos estáticos corretamente
render.d3movie(dyn_net,
               usearrows = F, 
               displaylabels = F,
               bg="#ffffff", vertex.border="#333333",
               # Dimensões do vértice
               vertex.cex =  function(slice){
                 #1 + (betweenness(slice)/500)
                 #size proporcional a between ou degree
                 #nao deu muito certo.Gigantes pós 90 
                 1 + degree(slice)/20
                 },

               #uma boa era inventar deleg_size como attr evento
               #e colocar isso como node size
               vertex.col = function(slice){#color (definida antes)
               (slice %v% "color")}, 
               # Dimensões da aresta
               edge.lwd = function(slice){slice %e% 'weight'/3 + 1}, #width
               edge.col = '#55555599',
               
               # Tooltip
               
               vertex.tooltip = function(slice) {
                 paste(
                   "<b>Name:</b>", (slice %v% "conf"),
                   "<br>",
                   "<b>Convention:</b>", (slice %v% "conference"),
                   "<br>",
                   "<b>Tema:</b>", (slice %v% "tema"),
                   "<br>",
                   "<b>Data:</b>", (slice %v% "data")
                 )
               },
               
               edge.tooltip = function(slice) {
                 paste(
                   "<b>Shared participants:</b>", 
                   (slice %e% "weight")
                 )
               }
)






# # Plotting interactive network ----

# Creating network

# net <- network(as.data.frame(edgelist), 
#                vertex.attr = as.data.frame(nodelist), 
#                matrix.type = 'edgelist', directed = F,
#                ignore.eval = F)

# library(visNetwork)
# 
# palette
# 
# # Tweak nodes
# vis.nodes <- nodelist
# 
# vis.nodes$shape  <- "dot"  
# vis.nodes$shadow <- TRUE # Nodes will drop shadow
# vis.nodes$title  <- vis.nodes$conf # Text on click
# # vis.nodes$label  <- vis.nodes$type.label # Node label
# # vis.nodes$size   # Node size
# vis.nodes$borderWidth <- 2 # Node border width
# # vis.nodes$color.background <- 
# vis.nodes$color.border <- "black"
# vis.nodes$color.highlight.background <- "orange"
# vis.nodes$color.highlight.border <- "darkred"
# 
# 
# # Tweak edges
# vis.links <- edgelist
# 
# vis.links$width <- links$weight*10 # line width
# vis.links$color <- "gray"    # line color  
# vis.links$smooth <- FALSE    # should the edges be curved?
# vis.links$shadow <- FALSE    # edge shadow
# 
# # Plot the network
# intnet <- visNetwork(vis.nodes, vis.links, width="100%", height="400px",
#            main = "Multilateral Environmental Conferences Network",
#            submain = "as experienced by Brazil (1970-2018)",
#            )
# 
# intnet <- visLayout(intnet)
# 
# intnet <- visOptions(intnet, highlightNearest = TRUE, 
#                      selectedBy = "tema")