library(tidyverse)
library(igraph)
# Importação das bases deleg

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


# P/ simplificar num primeiro momento, limite a não CBD/UNFCCC
matriz_part_evento <- matriz_part_evento %>% 
  filter(str_detect(conf, "UNFCCC|CBD")==F)


# Organização em formato de redes -------------

#dos dados de afiliaçao (part-evento) --> matriz incidência
matriz_part_evento <- as.matrix(table(matriz_part_evento))

rede_eventos <- graph.incidence(matriz_part_evento, mode = c("all"))
plot(rede_eventos, vertex.label.cex = .5, vertex.label.color = "black")


# transformar em 1-mode network (coparticipaçao em eventos)


# gerar matriz coparticipação 
# (parece que tem algum prob com o metodo) do table, mas nao sei
inc_matrix <- matriz_part_evento %>% select(nome_padrao, conf) %>% 
  table %>% as.matrix()
copart_matrix <- tcrossprod(inc_matrix)

rede_parts <- graph.adjacency(copart_matrix, mode = 'undirected')
# criar peso por múltipla coparticipação
E(rede_parts)$weight <- count.multiple(rede_parts)
rede_parts <- simplify(rede_parts)

# Atributos dos vértices
V(rede_parts)$label.cex <- .6
V(rede_parts)$size <- 6

V(rede_parts)$size <- 2
E(rede_parts)$width <- .3

# Set edge gamma according to edge weight
egam <- (log(E(rede_parts)$weight)+.3)/max(log(E(rede_parts)$weight)+.3)
E(rede_parts)$color <- rgb(.5,.5,0,egam)

# plot
