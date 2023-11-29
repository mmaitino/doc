# Script made to test whether/how different ways to create a network of orgs
# from the same data might differ. As we aggregate individuals into organizations
# for the analysis, results might change if we consider organizations as starting
# point (aggregate before network) or collapse vertices of individuals (aggregate 
# after network). Difference might appear in weights, but only if redundant links
# between organizations at the same time are disregarded.


nodelist <- data.frame(
  indyr = c("E1","F1","G1","E2","G2","H2","E3","H3"),
  indiv = c("E","F","G","E","G","H","E","H"),
  time = c(1,1,1,2,2,2,3,3),
  org = c("mre","mma","mma","mre","usp","mct","mre","mma")
)

edgelist <- data.frame(
  from = c("E1","E1","F1","E2","E2","G2","E3"),
  to = c("F1","G1","G1","G2","H2","H2","H3"),
  time = c(1,1,1,2,2,2,3)
)

# compare resulting networks:
# no collapsing: individual year as nodes -----
g <- graph_from_data_frame(edgelist, directed = F,
                           vertices = nodelist)
plot(g)

# collapsing with individuals as nodes (lose org and time attribute) ----

ind_g <- contract(g,
  mapping = as.numeric(
    plyr::mapvalues(V(g)$indiv, from = unique(nodelist$indiv), 
                    to = 1:length(unique(nodelist$indiv)))
  ),
  vertex.attr.comb=list(#methods for combining attributes
    weight="sum", "first")) #sum weights, keep first of others
# contract generates a multilayer network, which we then simplify
ind_g <- simplify(ind_g) # this also sums weight of edges


# collapsing with orgs as nodes (lose indiv and time attribute) ----
orgind_g <- contract(g,
                     mapping = as.numeric(
                       plyr::mapvalues(V(g)$org, from = unique(nodelist$org), 
                                       to = 1:length(unique(nodelist$org)))
                     ),
                     vertex.attr.comb=list(#methods for combining attributes
                       weight="sum", "first"))
V(orgind_g)$label <- V(orgind_g)$org
plot(orgind_g)

# creating directly from orgs as nodes -----
orgedgelist <- edgelist %>% rename(indyr = from) %>% 
  left_join(select(nodelist,-c(indiv,time))) %>% 
  select(-indyr) %>% rename(from = org, indyr = to) %>% 
  left_join(select(nodelist,-c(indiv,time))) %>% 
  select(-indyr) %>% rename(to = org)

orgg <- graph_from_data_frame(orgedgelist, directed = F)
plot(orgg)  

# results are the same (incl weight), as they consider redundant rows
# as multiple edges. They would only differ if it were simplified before
# (in the example below, I sum org participations that happen at same time)
simporgedgelist <- edgelist %>% rename(indyr = from) %>% 
  left_join(select(nodelist,-c(indiv,time))) %>% 
  select(-indyr) %>% rename(from = org, indyr = to) %>% 
  left_join(select(nodelist,-c(indiv))) %>% 
  select(-indyr) %>% rename(to = org) %>% 
  distinct() %>% select(-time)

simporgg <- graph_from_data_frame(simporgedgelist, directed = F)
plot(simporgg)
