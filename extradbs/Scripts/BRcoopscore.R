library(tidyverse)

# Open files from Castro database (.dta) -----
# path1 <- here::here("C:/Users/marti/Documents/Doutorado/controle_doc/extradbs", "dbs", "Castro", "ENB_relationships.dta")
# relationdb <- haven::read_dta(path1)

relationdb <- haven::read_dta(here::here("dbs","Castro", "ENB_relationships.dta"))

# country_actdb <- haven::read_dta(here::here("dbs","Castro", "statements_count.dta"))

# File from my replication ------
# path2 <- here::here("C:/Users/marti/Documents/Doutorado/controle_doc/ENB", "interacoesscraped96-13.csv")
# interacoesscraped96_13 <- read_delim("interacoesscraped96-13.csv", 
#                                      delim = ";", escape_double = FALSE, 
#                                      col_types = cols(...1 = col_skip()), 
#                                      trim_ws = TRUE)
# interacoesscraped96_13 <- interacoesscraped96_13 %>% rename(Country1 = sender, Country2 = target)

# GENERATING A COOPERATION INDEX -----------

# Filter statements by Brazil (sender, C1) or about Brazil (target, C2) ----
BR_statements <- relationdb %>% filter(Country1 == "Brazil" | Country2 == "Brazil")

# BR_statements_rep <- interacoesscraped96_13 %>% filter(Country1 == "BRAZIL" | Country2 == "BRAZIL")

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


#tem uma planilha com membros por grupo (pro período coberto)
country_groups <- haven::read_dta("C:/Users/marti/Documents/Doutorado/controle_doc/extradbs/dbs/Castro/country_groups.dta")
G77_members <- country_groups %>% filter(G77 != 0) %>% 
  mutate(countryyear = paste(country, year, sep = "-")) %>% pull(countryyear)

EU_members <- country_groups %>% filter(EU != 0) %>% 
  mutate(countryyear = paste(country, year, sep = "-")) %>% pull(countryyear)
# #teria que arrumar os nomes dos países, não estou usando os mesmos nomes! aí o match quebra.
# BR_statements_rep <- BR_statements_rep %>%
#   mutate(Country1 = if_else(Country1 %in% c("G-77/CHINA", "G77/CHINA", "G77"), "G77", Country1),
#          Country2 = if_else(Country2 %in% c("G-77/CHINA", "G77/CHINA", "G77"), "G77", Country2))

# BR_statements_rep %>% filter(Country1 == "BRAZIL") %>%
#   mutate(cooperation = if_else(interaction %in% c("behalf","spokewith","agreement","support"),
#                                                    1, 0),
#          year = year(negotiation_date)
#          ) %>%
#   mutate(cooperation = if_else(cooperation == 0, -1, 1)) %>%
#   mutate(cyear = paste(Country2,year, sep = "-")) %>%
#   mutate(G77_target = if_else(cyear %in% toupper(G77_members), TRUE, FALSE)) %>%
#   group_by(G77_target, year) %>%
#   summarise(coop_score = sum(cooperation))

  


# How much do G77 countries agree w/ Brazil?
BR_statements %>% filter(Country2 == "Brazil") %>% 
  mutate(cooperation = if_else(cooperation == 0, -1, 1)) %>% 
  group_by(G77_sender, year) %>% 
  summarise(coop_score = sum(cooperation), weighted_score = sum(relation))


# BR_statements_rep %>% filter(Country2 == "BRAZIL") %>% 
#   mutate(cooperation = if_else(interaction %in% c("behalf","spokewith","agreement","support"),
#                                1, 0),
#          year = year(negotiation_date)
#   ) %>% 
#   mutate(cooperation = if_else(cooperation == 0, -1, 1)) %>% 
#   mutate(cyear = paste(Country1, year, sep = "-")) %>% 
#   mutate(G77_sender = if_else(cyear %in% toupper(G77_members), TRUE, FALSE)) %>% 
#   group_by(G77_sender, year) %>% 
#   summarise(coop_score = sum(cooperation))



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


# db_rep <- BR_statements_rep %>%
#   mutate(cooperation = if_else(interaction %in% c("behalf","spokewith","agreement","support"),
#                                                      1, 0),
#            year = year(negotiation_date)
#            ) %>%
#   mutate(cooperation = if_else(cooperation == 0, -1, 1)) %>%
#   mutate(c1year = paste(Country1, year, sep = "-"),
#          c2year = paste(Country2, year, sep = "-")) %>%
#   mutate(G77_sender = if_else(c1year %in% toupper(G77_members), TRUE, FALSE),
#          G77_target = if_else(c2year %in% toupper(G77_members), TRUE, FALSE)) %>%
#   mutate(G77 = case_when(G77_sender == TRUE & Country2 == "BRAZIL" ~ 1,
#                          G77_target == TRUE & Country1 == "BRAZIL" ~ 1,
#                          G77_sender == FALSE & Country2 == "BRAZIL" ~ 0,
#                          G77_target == FALSE & Country1 == "BRAZIL" ~ 0)
#   ) %>%
#   mutate(EU_sender = if_else(c1year %in% toupper(EU_members), TRUE, FALSE),
#          EU_target = if_else(c2year %in% toupper(EU_members), TRUE, FALSE)) %>%
#   rowwise %>% mutate(EU = if_else(any(EU_sender, EU_target), 1, 0)) %>% ungroup %>%
#   mutate(cooperation = if_else(cooperation == 0, -1, 1)) %>%
#   group_by(G77, year) %>%
#   # group_by(EU, year) %>% 
#   summarise(coop_score = sum(cooperation)) %>%
#   pivot_wider(names_from = G77, #EU,
#               values_from = coop_score, names_prefix = "coop_score_") %>%
#   replace(is.na(.), 0) %>% #NA coded as 0
#   group_by(year) %>% summarise(coop_score = coop_score_1 - coop_score_0)


         







# Problem: shouldn't there be a control for n of statements? or whether many others agree?
# Could use coop score/n statements (send + target)

relationdb %>% filter(Country1 == "Brazil") %>% group_by(year) %>% 
  summarise(n_send = n()) -> BRsend
relationdb %>% filter(Country2 == "Brazil") %>% group_by(year) %>% 
  summarise(n_targ = n()) -> BRtarg
BR_total <- left_join(BRsend, BRtarg) %>% mutate(BRtotal = n_send + n_targ)

db <- left_join(db, BR_total) %>% mutate(coop_avg = coop_score/BRtotal,
                                         weighted_score_avg = weighted_score/BRtotal)


# interacoesscraped96_13 %>% filter(Country1 == "BRAZIL") %>% group_by(year(negotiation_date)) %>%
#   summarise(n_send = n()) -> BRsend_rep
# interacoesscraped96_13 %>% filter(Country2 == "BRAZIL") %>% group_by(year(negotiation_date)) %>%
#   summarise(n_targ = n()) -> BRtarg_rep
# BR_total_rep <- left_join(BRsend_rep, BRtarg_rep) %>% mutate(BRtotal = n_send + n_targ) %>%
#   rename(year = `year(negotiation_date)`)
# 
# db_rep <- left_join(db_rep, BR_total_rep) %>% mutate(coop_avg = coop_score/BRtotal)

# Interessante ver que em 2004 o score com peso mostra uma queda brutal, que não vemos
# no sem peso. Será que BR segue coop, mas c/ menos intensidade? Será que há peso gde
# de ciclos como 'pr rotativo do g77'?

# Expanding solution: function w/ grouping as argument --------------

calculate_coop <- function(grouping, country = "Brazil", 
                           statementsdb = relationdb, aggregateby = "year"){

  sender = paste0(grouping, "_sender")
  target = paste0(grouping, "_target")
  countryid_send = statementsdb[statementsdb$Country1 == country, "country1"] %>% 
    unique %>% haven::zap_labels() %>% pull(country1)
  countryid_targ = statementsdb[statementsdb$Country2 == country, "country2"] %>%
    unique %>% haven::zap_label() %>% pull(country2)
  
  
  #Count statements sent or targeting the country
  country_sentstatements <- statementsdb %>% filter(country1 == countryid_send) %>%
    group_by(!!rlang::parse_quo(aggregateby, #parse_quo faz string -> expressao
                                env = rlang::caller_env())) %>%  summarise(n_send = n())
    # group_by(aggregateby) %>%  summarise(n_send = n())
  country_tgtstatements <- statementsdb %>% filter(country2 == countryid_targ) %>%
    group_by(!!rlang::parse_quo(aggregateby, #parse_quo faz string -> expressao
                                env = rlang::caller_env())) %>%  summarise(n_targ = n())
    # group_by(aggregateby) %>%  summarise(n_targ = n())
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
    group_by(!!rlang::parse_quo(grouping, env = rlang::caller_env()), 
             !!rlang::parse_quo(aggregateby, env = rlang::caller_env())
             #aggregateby
             ) %>%
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

BReu <- calculate_coop("EU", "Brazil", relationdb) %>% finish_coop_scores()
BRg77 <- calculate_coop("G77", "Brazil", relationdb) %>% finish_coop_scores()
BRbasic <- calculate_coop( "BASIC", "Brazil", relationdb)
BRumb <- calculate_coop("Umbrella", "Brazil", relationdb)

bind_rows(BReu, BRg77) %>% ggplot(aes(x=year, color = grouping)) + 
  geom_line(aes(y = coop_avg)) + geom_hline(yintercept = 0, color = "red") +
  geom_vline(xintercept = c(1999, 2003, 2007, 2011), linetype = 'dashed') +
  scale_x_discrete(limits = c(1995, 1997, 1999, 2001, 
                              2003, 2005, 2007, 2009, 2011, 2013)) +
  ggtitle("Brazil average cooperation score at UNFCCC")

useu <- calculate_coop("EU", "United States", relationdb) %>% finish_coop_scores()
usg77 <- calculate_coop("G77", "United States", relationdb) %>% finish_coop_scores()
usumb <- calculate_coop("Umbrella", "United States", relationdb) %>% finish_coop_scores()
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

## checar pq parou de funcionar. uma possível causa é que
## mudei a ordem da função (calculate coop agora tem seq GROUPING, PAIS, c/ BR como padrao)
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




# compare with rep ------
# BRg77 %>% select(year, grouping, coop_avg) %>% mutate(data = "castro") %>%
#   bind_rows(db_rep %>% select(year, coop_avg) %>% mutate(grouping = "G77", data = "rep")) %>%
#   ggplot(aes(x=year, color = grouping)) +
#   facet_wrap(~data, ncol = 1) +
#   geom_line(aes(y = coop_avg)) + geom_hline(yintercept = 0, color = "red") +
#   geom_vline(xintercept = c(1999, 2003, 2007, 2011), linetype = 'dashed') +
#   scale_x_discrete(limits = c(1995, 1997, 1999, 2001,
#                               2003, 2005, 2007, 2009, 2011, 2013)) +
#   ggtitle("Brazil average cooperation score at UNFCCC")

# BReu %>% select(year, grouping, coop_avg) %>% mutate(data = "castro") %>%
#   bind_rows(db_rep2 %>% select(year, coop_avg) %>% mutate(grouping = "EU", data = "rep")) %>%
#   ggplot(aes(x=year, color = grouping)) +
#   facet_wrap(~data, ncol = 1) +
#   geom_line(aes(y = coop_avg)) + geom_hline(yintercept = 0, color = "red") +
#   geom_vline(xintercept = c(1999, 2003, 2007, 2011), linetype = 'dashed') +
#   scale_x_discrete(limits = c(1995, 1997, 1999, 2001,
#                               2003, 2005, 2007, 2009, 2011, 2013)) +
#   ggtitle("Brazil average cooperation score at UNFCCC")


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


# teste pca --------------
coalitions <- c("AILAC", "ALBA", "AOSIS", "AfricanG", "ArabG", "BASIC", "COMIFAC", "CaribbeanC",
                "CentralAm", "CentralG11", "CfRN", "CongoB", "EITs", "EU", "EIG", "G77",
                "G9", #"CACAM",
                "LDCs", "LMDC", "Mountain", "OPEC", "SICA", "SAfricaDC",
                "Umbrella", "Visegrad"#, "AsianG"
                )

BReu <- calculate_coop("EU","Brazil", relationdb) %>% finish_coop_scores()
BRg77 <- calculate_coop("G77","Brazil", relationdb) %>% finish_coop_scores()
BRbasic <- calculate_coop("BASIC","Brazil", relationdb)
BRumb <- calculate_coop("Umbrella","Brazil", relationdb)

df <- map_df(coalitions, calculate_coop)
df <- finish_coop_scores(df)
df <- select(df, -c(coop_score_0, coop_score_1, weighted_score_0, weighted_score_1,
                weighted_score_2, n_send, n_targ, state_total))

# other possibilities of df
# dividindo dados por tema
maptopic_porano <- function(year_var){
  map_df(coalitions, calculate_coop, "Brazil",
         relationdb %>% filter(year == year_var), "topic") -> topic_year
  # topic_year <- finish_coop_scores(topic_year)
  # topic_year <- topic_year %>% pivot_wider(names_from = "grouping", values_from = "coop_avg")
  topic_year$year <- year_var
  topic_year
}

list <- map(1995:2013, maptopic_porano)
df_topics_ano <- bind_rows(list)
df_topics_ano <- finish_coop_scores(df_topics_ano)
df_topics_ano %>%
  select(-c(weighted_score_0, weighted_score_1, weighted_score_2, weighted_score, weighted_score_avg, 
            coop_score_0, coop_score_1, coop_score_2, coop_score)) %>% 
  filter(!grouping %in% c("COMIFAC", "CaribbeanC", "CentralAm", "CentralG11", "EITs", "G9",
            "Mountain", "OPEC", "SAfricaDC", "Visegrad")) %>% 
  mutate(topic_name = case_when(
    topic == 1 ~ "Adaptation",
    topic == 2 ~ "Mitigation",
    topic == 3 ~ "Finance",
    topic == 4 ~ "Capacity building",
    topic == 5 ~ "Technology",
    topic == 6 ~ "International transport",
    topic == 7 ~ "LULUCF (forests)",
    topic == 8 ~ "Flexibility mechanisms",
    topic == 9 ~ "REDD (reducing emissions from deforestation)",
    topic == 10 ~ "Organisation",
    topic == 11 ~ "Institutional arrangement",
    topic == 12 ~ "Content of new agreement",
    topic == 13 ~ "Mitigation Annex1",
    topic == 14 ~ "Mitigation non-Annex1",
    topic == 15 ~ "Reporting",
    topic == 16 ~ "Principles",
    topic == 17 ~ "Response measures",
    topic == 18 ~ "Research / climate science",
    topic == 18 ~ "Agriculture"
  )) %>% 
  filter(topic %in% c(#1,2,3,7,9,12,
                      13,14)) %>% 
  pivot_wider(names_from = c(grouping, topic_name), values_from = "coop_avg") -> df_topicspivot

df_topicspivot %>% select(-c(year, country, topic)) %>% scale() %>% as_tibble() %>% 
  princomp() -> pca_topics

# standardize
df_num <- df %>% 
  select(-c(weighted_score, weighted_score_avg, coop_score, coop_score_2)) %>% #calcular c/ coop_avg
  # filter(year == 1995) %>% # separar o ano (se nao quiser rodar PCA p/ todos dados juntos)
  pivot_wider(names_from = "grouping", values_from = "coop_avg") %>% 
  select(-c(year, country)) %>% # remover as colunas não numéricas
  scale() %>% as_tibble() %>% 
  # select(-c(COMIFAC, CaribbeanC, CentralAm, CentralG11, EITs, G9,
  #           Mountain, OPEC, SAfricaDC, Visegrad))
  select(AOSIS, EU, G77, BASIC)


#correlações de cada variavel
df_num %>% 
  cor(use="pairwise") %>% round(1) %>% ggcorrplot::ggcorrplot(type = 'lower',
                                                                      lab = T)

library(stats)
pca <- princomp(df_num)
summary(pca, loadings = T, cutoff = 0.3)

df_pca <- pca$scores %>% as_tibble()
df_pca$year <- 1995:2013
df_pca %>% mutate(governo = case_when(year >= 1995 & year < 1999 ~ "FHC1",
                                      year >= 1999 & year < 2003 ~ "FHC2",
                                      year >= 2003 & year < 2007 ~ "Lula1",
                                      year >= 2007 & year < 2011 ~ "Lula2",
                                      year >= 2011 & year < 2014 ~ "Dilma1")
                  ) %>% 
ggplot(aes(x=Comp.1, y =Comp.2, color = governo, label = as.character(year))) + 
  geom_point() + geom_text() +
  ggtitle("PCA components") +
  xlab("Dim1 (51% variance) - EU/AOSIS-G77") + #xlim(-3.3,3.3) +
  ylab("Dim2 (21% variance) - BASIC") #+ ylim(-3,3)

# contribuição de cada dimensão
library(factoextra)
eig_val <- get_eigenvalue(pca)
fviz_eig(pca, addlabels = T, ylim = c(0, 50))

# contribuição de cada variável para as dimensoes
fviz_pca_biplot(pca, repel = F, col.var = "black", col.ind = "gray")
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)

df2 <- df %>% 
  select(-c(weighted_score, weighted_score_avg, coop_score, coop_score_2)) %>%
  pivot_wider(names_from = "grouping", values_from = "coop_avg") %>% 
  bind_cols(as_tibble(pca$scores))

df2 %>% ggplot(aes(x=year, y =Comp.1)) + geom_line()
df2 %>% ggplot(aes(x=year, y =Comp.2)) + geom_line()
df2 %>% ggplot(aes(x=year, y =Comp.3)) + geom_line()


df2 %>%
  mutate(components_multiplied = (.52*Comp.1 + .21*Comp.2 + .11*Comp.3)/.84) %>% #all groupings
  # mutate(components_multiplied = (.51*Comp.1 + .33*Comp.2 + .09*Comp.3)/.93) %>% #only EU, AOSIS, BASIC, G77
  arrange(year) %>%
  mutate(components_multiplied = GGally::rescale01(components_multiplied)) %>%
  ggplot(aes(x=year, y = components_multiplied - lag(components_multiplied))) +
  # ggplot(aes(x=year, y = GGally::rescale01(Comp.1))) + 
  geom_line() + geom_point() +
  #geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_vline(xintercept = c(1999, 2003, 2007, 2011), 
             color = "darkgray", linetype = "dashed") +
  scale_x_continuous(breaks = c(1995, 1999, 2003, 2007, 2011, 2014))  +
  ylab("BR Position index") + #ylim(-1, 1) +
  theme(legend.position = 'bottom', axis.title.x = element_blank()) +
  ggtitle("Yearly variation in Brazilian cooperation with groupings at UNFCCC",
          # subtitle = "PCA, first three components (83,9% of variance)") #all groupings
  subtitle = "PCA, first three components (93,4% of variance)") #only EU, AOSIS, BASIC, G77
  # ggtitle("Brazil position - PCA 1st component",
  # subtitle = "positive = close to G77, negative = close to EU/AOSIS\n PCA of cooperation with EU, AOSIS, BASIC and G77")
