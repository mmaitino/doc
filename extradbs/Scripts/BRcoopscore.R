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

calculate_coop <- function(country = "Brazil", grouping, statementsdb){

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
    left_join(country_totalstatements) %>% # include data on n of statements
    mutate(country = {{country}},
           grouping = {{grouping}}
           ) # %>% 
    # mutate(coop_score = sum(coop_score_2, coop_score_1, - coop_score_0, na.rm = T),
    #        # weighted_score = sum(weighted_score_2, weighted_score_1, - weighted_score_0,
    #        #                   na.rm = T)
    #        ) %>% 
    # # relativize_scores
    # left_join(country_totalstatements) %>%
    # mutate(coop_avg = coop_score/state_total,
    #        # weighted_score_avg = weighted_score/state_total
    #        ) %>% 
    # mutate(grouping = {{grouping}})
    
}

finish_coop_scores <- function(coopdb){
  column_checker <- tibble(coop_score_0=numeric(),
                           coop_score_1=numeric(), 
                           coop_score_2=numeric(),
                           weighted_score_2=numeric(), 
                           weighted_score_1=numeric(),
                           weighted_score_0=numeric()
  )
  
  coopdb %>% left_join(column_checker) %>% 
  mutate(coop_score = coop_score_2 + coop_score_1 - coop_score_0,
         weighted_score = weighted_score_2 + weighted_score_1 - weighted_score_0
         ) %>%
  # relativize_scores
  mutate(coop_avg = coop_score/state_total,
         weighted_score_avg = weighted_score/state_total
         )
}


# Testar se resultados estão corretos! ---------

# Rodar para diferentes grupos e comparar agreement -------
# BRA se afastou do G77? Se aproximou? Idem para Umbrella, EU, etc
# problema: temos um possível viés no score médio.
# controle é n total de statements (nao de statements que mencionam alguem do grupo),
# logo grupo maior tende a ser mais mencionado (há mais países p/ mencionar o BR
# ou serem por ele mencionados). Talvez isso explique distância de BASIC?

BReu <- calculate_coop("Brazil", "EU", relationdb)
BRg77 <- calculate_coop("Brazil", "G77", relationdb)
BRbasic <- calculate_coop("Brazil", "BASIC", relationdb)
BRumb <- calculate_coop("Brazil", "Umbrella", relationdb)

bind_rows(BReu, BRg77) %>% ggplot(aes(x=year, color = grouping)) + 
  geom_line(aes(y = coop_avg)) + geom_hline(yintercept = 0, color = "red") +
  geom_vline(xintercept = c(1999, 2003, 2007, 2011), linetype = 'dashed') +
  scale_x_discrete(limits = c(1995, 1997, 1999, 2001, 
                              2003, 2005, 2007, 2009, 2011, 2013)) +
  ggtitle("Brazil average cooperation score at UNFCCC")

useu <- calculate_coop("United States", "EU", relationdb)
usg77 <- calculate_coop("G77", "United States", "G77", relationdb)
usumb <- calculate_coop("United States", "Umbrella", relationdb)
bind_rows(useu, usg77, usumb) %>% ggplot(aes(x=year, color = grouping)) + 
  geom_line(aes(y = coop_avg)) + geom_hline(yintercept = 0, color = "red") +
  geom_vline(xintercept = c(1997, 2001, 2005, 2009), linetype = 'dashed') +
  scale_x_discrete(limits = c(1995, 1997, 1999, 2001, 
                              2003, 2005, 2007, 2009, 2011, 2013)) +
  ggtitle("US average cooperation score at UNFCCC")



#coop for all
country_list <- c(BR_statements$Country1, BR_statements$Country2) %>% unique
grouping_list <- c("G77", "EU", "AOSIS", "African Group", 
                   "Environmental Integrity Group", "LDCs","ALBA",
                   "SICA","Coalition of Rainforest Nations", "Arab Group" ,
                   "Like Minded Developing Countries","BASIC", "AILAC",
                   "Congo Basin Countries", "Umbrella Group" )
country_list <- setdiff(country_list, grouping_list)

# pensei em achar algum modo de comparar se BR concorda mais do que a média com g77

df_countries77 <- map_df(country_list, calculate_coop, "G77", relationdb) %>% 
  finish_coop_scores()
df_countriesEU <- map_df(country_list, calculate_coop, "EU", relationdb) %>% 
  finish_coop_scores()

df_countries <- bind_rows(df_countries77,df_countriesEU)

country_average77 <- df_countries77 %>% group_by(year, grouping) %>% 
  summarise(coop_countryavg = mean(coop_avg, na.rm = T),
            coop_countrymedian = median(coop_avg, na.rm = T),
            weightcoop_countryavg = mean(weighted_score_avg, na.rm = T),
            weightcoop_countrymedian = median(weighted_score_avg, na.rm = T),
            )

country_averageEU <- df_countriesEU %>% group_by(year, grouping) %>% 
  summarise(coop_countryavg = mean(coop_avg, na.rm = T),
            coop_countrymedian = median(coop_avg, na.rm = T),
            weightcoop_countryavg = mean(weighted_score_avg, na.rm = T),
            weightcoop_countrymedian = median(weighted_score_avg, na.rm = T),
  )

country_average <- bind_rows(country_average77, country_averageEU)

df_countries %>%
  left_join(country_average) %>% select(year, country, grouping,
                                        coop_avg, coop_countryavg,
                                        coop_countrymedian) %>%
  pivot_longer(c(coop_avg, coop_countryavg, coop_countrymedian), 
               names_to = "type", values_to = "score") %>%
  filter(country == "Brazil") %>% mutate(
    type = case_when(type == "coop_avg" ~ country,
                     type == "coop_countryavg" ~ "Country mean",
                     type == "coop_countrymedian" ~ "Country median")) %>% 
  
  ggplot(aes(x=year, color = type)) +
  geom_line(aes(y = score)) + geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~grouping, ncol =1) +
  ggtitle("Cooperation degree at the UNFCCC with G77 and European Union")



# relative coop

df_countries %>% filter(country == "Brazil") %>% 
  left_join(country_average) %>% select(year, country, grouping,
                                        coop_avg, coop_countryavg,
                                        coop_countrymedian) %>% 
  mutate(coopdif = coop_avg - coop_countryavg) %>% 
  mutate(difpos = if_else(coopdif >=0, 1, -1)) %>%
  mutate(cooprel = abs(coop_avg) / abs(coop_countryavg) * difpos) %>% 
  ggplot(aes(x=year, y = cooprel)) + geom_line() + geom_hline(yintercept = 0, 
                                                              color = "red") +
  ggtitle("Brazil relative cooperation with grouping (BR coop / avg country coop)",
    subtitle = "above 0 = more cooperation than average country") +
  facet_wrap(~ grouping, nrow = 1) +
  ylim(-4, 4)


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