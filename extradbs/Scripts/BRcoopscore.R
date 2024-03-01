library(tidyverse)

# Open files from Castro database (.dta) -----
relationdb <- haven::read_dta(here::here("dbs","Castro", "ENB_relationships.dta"))
# country_actdb <- haven::read_dta(here::here("dbs","Castro", "statements_count.dta"))

# GENERATING A COOPERATION INDEX -----------

# Filter statements by Brazil (sender, C1) or about Brazil (target, C2) ----
BR_statements <- relationdb %>% filter(Country1 == "Brazil" | Country2 == "Brazil")

# To simplify agreement/disagreement:
# Codebook: on behalf 1; support 2; with 3; agre 4; delay 5; opp 6; crit 7 (decrease coop)
# variable 'cooperation' simplifies (1 = coop; 0 = confl)
# variable 'relation' is ordinal from coop to conf (1 to 7), w/ labels

# Two ways to calculate a score for Brazil relations (per year, group etc):
# Score as SUM(COOP - CONF)
# Score w/ weight SUM(relation). Assumes change uniform between units
# I prefer simple score, but should do this as robustness check.


# Calculating scores for agreement with G77 countries -----------
# How much does BRA agree to G77 countries? (0 = not g77, 1= g77 member, 2= g77 itself)
BR_statements %>% filter(Country1 == "Brazil") %>% 
  mutate(cooperation = if_else(cooperation == 0, -1, 1)) %>% 
  group_by(G77_target, year) %>% 
  summarise(coop_score = sum(cooperation), weighted_score = sum(relation))

# How much do G77 countries agree w/ Brazil?
BR_statements %>% filter(Country2 == "Brazil") %>% 
  mutate(cooperation = if_else(cooperation == 0, -1, 1)) %>% 
  group_by(G77_sender, year) %>% 
  summarise(coop_score = sum(cooperation), weighted_score = sum(relation))

# Disregarding direction:
db <- BR_statements %>% mutate(G77 = case_when(G77_sender == 2 | G77_target == 2 ~ 2,
                                               country2 == 29 & G77_sender == 1 | 
                                            country1 == 28 & G77_target == 1 ~ 1,
                                        #o do 0 tem que retirar país tb?
                                         G77_sender == 0 | G77_target == 0 ~ 0
                                         )) %>% 
                           mutate(cooperation = if_else(cooperation == 0, -1, 1)) %>% 
                           group_by(G77, year) %>% 
                           summarise(coop_score = sum(cooperation), 
                                     weighted_score = sum(relation)) %>% 
  pivot_wider(names_from = G77, values_from = c(coop_score, weighted_score)) %>% 
  replace(is.na(.), 0) %>% #NA coded as 0
  group_by(year) %>% summarise(coop_score = coop_score_2 + coop_score_1 - coop_score_0,
                               weighted_score = weighted_score_2 +
                                 weighted_score_1 - weighted_score_0)

# Problem: shouldn't there be a control for n of statements? or whether many others agree?
# Could use coop score/n statements (send + target)

relationdb %>% filter(Country1 == "Brazil") %>% group_by(year) %>% 
  summarise(n_send = n()) -> BRsend
relationdb %>% filter(Country2 == "Brazil") %>% group_by(year) %>% 
  summarise(n_targ = n()) -> BRtarg
BR_total <- left_join(BRsend, BRtarg) %>% mutate(BRtotal = n_send + n_targ)

db <- left_join(db, BR_total) %>% mutate(coop_avg = coop_score/BRtotal,
                                         weighted_score_avg = weighted_score/BRtotal)

# Interessante ver que em 2004 o score com peso mostra uma queda brutal, que não vemos
# no sem peso. Será que BR segue coop, mas c/ menos intensidade? Será que há peso gde
# de ciclos como 'pr rotativo do g77'?

# Expanding solution: function w/ grouping as argument --------------

calculate_coop <- function(statementsdb, grouping, country = "Brazil"){
  
  sender = paste0(grouping, "_sender")
  target = paste0(grouping, "_target")
  countryid_send = statementsdb[statementsdb$Country1 == country, "country1"] %>% 
    unique %>% haven::zap_labels() %>% pull(country1)
  countryid_targ = statementsdb[statementsdb$Country2 == country, "country2"] %>%
    unique %>% haven::zap_label() %>% pull(country2)
  
  
  #Count statements sent or targeting the country
  country_sentstatements <- statementsdb %>% filter(country1 == countryid_send) %>%
    group_by(year) %>%  summarise(n_send = n())
  country_tgtstatements <- statementsdb %>% filter(country2 == countryid_targ) %>%
    group_by(year) %>%  summarise(n_targ = n())
  country_totalstatements <- left_join(country_sentstatements, country_tgtstatements) %>%
    mutate(state_total = n_send + n_targ)

  
  statementsdb %>% 
    #filter statements
    filter(Country1 == country | Country2 == country) %>% 
    # fuse sender and target
    # (include both directions of relations to grouping)
    mutate( {{grouping}} := 
              # chaves e := é pra variável ser mutável cf arg
              case_when(
                # 2 = grouping spokesman
                !!rlang::parse_quo(sender, #parse_quo faz string -> expressao
                                   env = rlang::caller_env()) == 2 |
                  !!rlang::parse_quo(target, 
                                     env = rlang::caller_env()) == 2 ~ 2,
                # 1 = country in grouping
                # since analyzed country can be in grouping, disconsider 
                # when send/targ is the country analysed
                !!rlang::parse_quo(sender, env = rlang::caller_env()) == 1 &
                  country1 != countryid_send | !!rlang::parse_quo(target, 
                                                                  env = rlang::caller_env()) == 1 &
                  country2 != countryid_targ ~ 1,
                
                # 0 = not in grouping
                !!rlang::parse_quo(sender, env = rlang::caller_env()) == 0 &
                  country1 != countryid_send | !!rlang::parse_quo(target, 
                                                                  env = rlang::caller_env()) == 0 &
                  country2 != countryid_targ ~ 0
              )
    ) %>% 
    #summarise scores
    group_by(!!rlang::parse_quo(grouping, env = rlang::caller_env()), year) %>%
    summarise(coop_score = sum(cooperation),
              weighted_score = sum(relation)) %>%
    pivot_wider(names_from = !!rlang::parse_quo(grouping, env = rlang::caller_env()),
                 values_from = c(coop_score, weighted_score)) %>% 
    replace(is.na(.), 0) %>% #NA coded as 0
    mutate(coop_score = coop_score_2 + coop_score_1 - coop_score_0,
           weighted_score = weighted_score_2 + weighted_score_1 - weighted_score_0) %>% 
    # relativize_scores
    left_join(country_totalstatements) %>%
    mutate(coop_avg = coop_score/state_total,
           weighted_score_avg = weighted_score/state_total) %>% 
    mutate(grouping = {{grouping}})
    
}


# Testar se resultados estão corretos! ---------

# Rodar para diferentes grupos e comparar agreement -------
# BRA se afastou do G77? Se aproximou? Idem para Umbrella, EU, etc
# problema: temos um possível viés no score médio.
# controle é n total de statements (nao de statements que mencionam alguem do grupo),
# logo grupo maior tende a ser mais mencionado (há mais países p/ mencionar o BR
# ou serem por ele mencionados). Talvez isso explique distância de BASIC?

BReu <- calculate_coop(relationdb, "EU", "Brazil")
BRg77 <- calculate_coop(relationdb, "G77", "Brazil")
BRbasic <- calculate_coop(relationdb, "BASIC")
BRumb <- calculate_coop(relationdb, "Umbrella")

bind_rows(BReu, BRg77) %>% ggplot(aes(x=year, color = grouping)) + 
  geom_line(aes(y = coop_avg)) + geom_hline(yintercept = 0, color = "red") +
  geom_vline(xintercept = c(1999, 2003, 2007, 2011), linetype = 'dashed') +
  scale_x_discrete(limits = c(1995, 1997, 1999, 2001, 
                              2003, 2005, 2007, 2009, 2011, 2013)) +
  ggtitle("Brazil average cooperation score at UNFCCC")

useu <- calculate_coop(relationdb, "EU", "United States")
usg77 <- calculate_coop(relationdb, "G77", "United States")
usumb <- calculate_coop(relationdb, "Umbrella", "United States")
bind_rows(useu, usg77, usumb) %>% ggplot(aes(x=year, color = grouping)) + 
  geom_line(aes(y = coop_avg)) + geom_hline(yintercept = 0, color = "red") +
  geom_vline(xintercept = c(1997, 2001, 2005, 2009), linetype = 'dashed') +
  scale_x_discrete(limits = c(1995, 1997, 1999, 2001, 
                              2003, 2005, 2007, 2009, 2011, 2013)) +
  ggtitle("US average cooperation score at UNFCCC")


# REPLICATION OF CASTRO NETWORK ---------
# Relationdb -> directed network of coop (or, inversely, of confl)
library(igraph)

relationdb %>% select(Country1, Country2, cooperation, topic, year, e_date) %>% 
  graph_from_data_frame(directed = T) -> coop_graph

E(coop_graph)$weight <- E(coop_graph)$cooperation #weight=1 if coop, 0 if conflict
# calculate centrality measures:
coop_graph <- set_vertex_attr(coop_graph,
                              name = "indegree",
                              value = degree(coop_graph, mode = "in")
)

# top most active (indegree = n of statements):
V(coop_graph)[V(coop_graph)$indegree %in% head(V(coop_graph)$indegree, 20)]
# Brazil indegree
V(coop_graph)[V(coop_graph)$name == "Brazil"]$indegree

#Pra fazer direito, teria que transformar em uma rede dinâmica, 
# aí teríamos indegree p/cada ano