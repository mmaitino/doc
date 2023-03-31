# Script to generate network from participant databases

# First, we will organize databases.
# Then, we generate a incidence matrix (incidence of individuals in events)
# We then project the bipartite into two networks (events and organizations)
# The script then separately organizes the networks (include attributes, etc)
# and generates the figures used in the paper.


# Libraries used -----
library(tidyverse)
library(igraph)
library(lubridate)

# STEP 1 - ORGANIZING AND IMPORTING DATA ------

# Import and organize data on events ------
#.... Open event spreasheet and organize data --------
eventos <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/Historico/eventos_v4.csv",
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
eventos <- eventos %>%
  rename(#rename columns
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
  filter(is.na(data)==F) %>% #remove events with no information on dates
  select(conf, conference, tema, data) #analysis only needs these columns

#.... Standardizing event dates -----
# As events may lack complete date information must standardize
# No particular importance to date other than year, but full needed for datetime
eventos <- eventos %>% mutate(
  data = if_else(str_count(data)==4, # if missing month
                 paste0(data, "-01"), # standard as January
                 data)) %>%
  mutate(
    data = if_else(is.na(data)== F, 
                   paste0(data, "-01"), # standard all dates as 1st of the month
                   data)
  ) %>% mutate(data = ymd(data), 
               ano = year(ymd(data)))

#.... Create numerical id for events ----
eventos$id_evento <- 1:nrow(eventos)

#Import and organize data on participant lists and individuals ----

# Participant lists
delegs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/deleg-2022-06-29.csv", 
                     ";", escape_double = FALSE, 
                     col_types = cols(#X1 = col_skip(), 
                                      cargo_deleg = col_skip(), 
                                      cargo_org = col_skip(), 
                                      desc = col_skip(), fonte = col_skip(), 
                                      nline = col_skip(), org = col_skip(),
                                      org_detalhe = col_skip(), pais = col_skip(),
                                      titulo = col_skip()), 
                     #locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)

# Individuals
individuos <- read_csv("individuos-2022-07-01.csv", 
                       col_types = cols(padrao_antigo = col_skip(),
                                        nrow = col_skip()))

#.... Create numerical ids ----
# To keep track of individuals and of their participation in events, create ids
# Individual-Event Id (one for each participation of the individual in event)
delegs$id_indevento <- 1:nrow(delegs)

# Individual Id (permanent individual id shared across events)
individuos %>% select(nome_padrao) %>% distinct() %>% mutate(
  id_individuo = row_number() #create id for each individual
) %>% right_join(#add id to original dataframe
  individuos, multiple = "all") -> individuos


# Joining the databases -----
# Data on individuals (standardize participant names and include ids)
matriz_part_evento <- left_join(delegs, individuos) %>% 
  select(-c(nome, id_org_dupla, nome_padrao))
# Data on events (include data on the events)
matriz_part_evento <- left_join(matriz_part_evento, 
                                select(eventos, c(conf, id_evento))
) %>% select(-conf)

  # Resulting dataframe only includes numerical ids

# List of events that have participants (as some lack Brazilian delegations)
lista_eventos_comdeleg <- matriz_part_evento$id_evento


# STEP 2 - THE EVENTS PROJECTED NETWORKS ------

# 2A - GENERATING THE BIPARTITE NETWORK -----------
# Generating an incidence matrix -----
# Before creating a network projection, we must create an incidence matrix

incidence_matrix <- matriz_part_evento %>% 
  select(id_individuo, id_evento) %>% # use data of individuals and events
  table() # count if there is data in each combination of individual and event
class(incidence_matrix) <- "matrix" # converts the dataframe into matrix object

# If we wanted to create a bipartite network for analysis, this would be enough


# 2B - GENERATING THE EVENTS NETWORK -----------
# Creating unipartite projections ----

# To generate a network projection, one must multiply the incidence matrix
# by its transpose. 
# This is called the Cross-Product Method, applied here directly
# through the multiplication of the matrices (instead of by packages, eg)


#.... Projecting an Event-Event Network -----
# To create the event-event matrix, multiplication order should be inverted

# Event-Event Matrix through cross-Product Method
matriz_eventos <- t(incidence_matrix) %*% incidence_matrix
deleg_size_event <- diag(matriz_eventos) # diagonal gives us the size of events
# Diagonal should be zero before projection to avoid node loops
diag(matriz_eventos) <- 0 

# Network: Projecting events as nodes, coparticipation as edges and weight
eventNet <- graph.adjacency(matriz_eventos, mode = "undirected",
                            weighted = TRUE)

rm(matriz_eventos) # remove object to clear space

# Including attributes in the network ------------
# We will now include some attributes for nodes, so that we can better
# analyze and describe the network

# to avoid problems, we will use as ref list of events with BR delegation
eventos_comdeleg <- eventos %>% filter(id_evento %in% lista_eventos_comdeleg)

# Include event name as 'confid'
eventNet <- set_vertex_attr(eventNet, "confid", index = V(eventNet),
                            as.character(eventos_comdeleg$conf))
# Include general conference/treaty name as 'conference'
eventNet <- set_vertex_attr(eventNet, "conference", index = V(eventNet),
                            as.character(eventos_comdeleg$conference))
# Include issue-area of event as 'tema'
eventNet <- set_vertex_attr(eventNet, "tema", index = V(eventNet),
                            as.character(eventos_comdeleg$tema))
# Include year of event as 'ano'
eventNet <- set_vertex_attr(eventNet, "ano", index = V(eventNet),
                            as.character(eventos_comdeleg$ano))

# We can now see all vertex attributes:
# vertex.attributes(eventNet)
# summary(eventNet)

#.... Including attributes for use in visualization
# Include nodesize as proportional to node centrality
# a) which centrality? should I use betweeness as well?



# Include ConfID as labels
V(eventNet)$label <- V(eventNet)$confid

# adjust labels to better fit figures
long_labels <- tibble(label = V(eventNet)$label, 
                      nchar = nchar(V(eventNet)$label))

# long_labels %>% filter(nchar > 10) %>% View # id long labels
long_labels$newlab <- long_labels$label
long_labels <- long_labels %>%
  mutate(
    newlab = str_replace(newlab,
                         "Extraordinary Session", "ES"
    )
  ) %>% 
  mutate(
    newlab = str_replace(newlab,
                         "Ad Hoc Working Group of Legal and Technical Experts for the Preparation of a Protocol on Chlorofluorocarbons to the Vienna Convention for the Protection of the Ozone Layer",
                         "VC AHWG")
  ) %>% 
  mutate(
    newlab = str_replace(newlab,
                         "Ad Hoc Working Group of Experts on Biological Diversity",
                         "AHWG")
  ) %>% mutate(
    newlab = str_replace(newlab,
                         "Minamata", "MC"
    )
  ) %>% mutate(
    newlab = str_replace(newlab,
                         "Prep Committee", "PrepCom"
    )
  )%>% mutate(
    newlab = str_replace(newlab,
                         "UNESCO IOC", "IOC"
    )
  )%>% mutate(
    newlab = str_replace(newlab,
                         ", ", ",\n"
    )
  ) %>% mutate(
    newlab = str_remove(newlab, "st |nd |rd |th |Session ")
  )
  

V(eventNet)$label <- long_labels$newlab

# Standardize color scheme by Issue-Area
#### note: colors in graph legend were not made automatically,
#### so if change here need to manually change there!
## There is definitely a better way to do this (using the df_legend from below)

V(eventNet)$color <- case_when(
  V(eventNet)$tema == "Ozônio" ~ "dodgerblue3",
  V(eventNet)$tema == "Biodiversidade" ~ "darkgreen",
  V(eventNet)$tema == "Biodiversidade - Espécies" ~ "paleturquoise3",
  V(eventNet)$tema == "Lixo tóxico e químicos" ~ "slateblue3",
  V(eventNet)$tema == "Florestas" ~ "darkseagreen3",
  V(eventNet)$tema == "Oceano" ~ "royalblue4",
  V(eventNet)$tema == "Grandes conferências ONU" ~ "darkorange2",
  V(eventNet)$tema == "Desertificação" ~ "goldenrod",
  V(eventNet)$tema == "Governança ambiental" ~ "coral4",
  V(eventNet)$tema == "Biodiversidade - UNESCO" ~ "pink4",
  V(eventNet)$tema == "Clima" ~ "firebrick3"
)


# Filtering the network by years -------------
filternet_byyear <- function(network, begin, end) {
  #function includes specified years
  filterednetwork <- delete_vertices(network, V(network)$ano < begin | 
                                       V(network)$ano > end)
  filterednetwork
}

full_eventNet <- filternet_byyear(eventNet, 1970, 2018)
#filternet_byyear(eventNet, 1970, 1979) %>% plot


# Only show label if node is among the top-20 largest betweenness
# (If picture is too large, include this as well)
# (Remember to run it AFTER filtering network)
# só aparece nome dos 20 top between
# top_betw <- betweenness(eventNet) %>% sort(decreasing = T) %>% head(20)
# V(eventNet)$label <- if_else(V(eventNet)$name %in% names(top_betw),
#                              as.character(V(eventNet)$confid), NULL)

# Generating a simplified event network ----

# Collapse all nodes by conference to simplify full_eventNet

# To do this, we need a mapping of existing conferences
conf_list <- unique(V(full_eventNet)$conference)
# We can then collapse the nodes with contract
simple_eventNet <- contract(
  full_eventNet,
  mapping = as.numeric(
    plyr::mapvalues(V(full_eventNet)$conference, from = conf_list, 
                    to = 1:length(conf_list))
  ),
  vertex.attr.comb=list(#methods for combining attributes
    weight="sum", "first")) #sum weights, keep first of others
# contract generates a multilayer network, which we then simplify
simple_eventNet <- simplify(simple_eventNet) # this also sums weight of edges

#.... Calculating some relevant statistics for the network ----
simpleNet_degree <- degree(simple_eventNet, mode = 'all') %>% sort(decreasing = T)
top_simpleNetdeg <- head(simpleNet_degree, 10) %>% names()
data.frame(
  deg = simpleNet_degree,
  id = names(simpleNet_degree)) %>% filter(id %in% top_simpleNetdeg)

V(simple_eventNet)[V(simple_eventNet)$name %in% top_simpleNetdeg]$conference
simpleNet_avg_degree <- mean(simpleNet_degree)
simpleNet_med_degree <- median(simpleNet_degree)



#.... Organizing the simplified event net ---------
# To properly visualize the network, we need to map the conference names
# to shorter labels

short_conf <- tibble(
  conference = c(
    "Montreal Protocol on Substances that Deplete the Ozone Layer",
    "Convention on Biological Diversity",
    "International Treaty on Plant Genetic Resources for Food and Agriculture",
    "Basel Convention on the Control of Transboundary Movements of Hazardous Wastes and their Disposal",
    "Convention on International Trade in Endangered Species of Wild Fauna and Flora",
    "Convention on Migratory Species",
    "FAO Committee on Forestry",
    "International Plant Protection Convention",
    "Conference on International Co-operation on Preparedness and Response to Pollution Incidents by Hazardous and Noxious Substances",
    "UN Convention to Combat Desertification",
    "International Conference on Chemicals Management",
    "International Tropical Timber Organization",
    "London Convention on the Prevention of Marine Pollution by Dumping of Wastes and Other Matter 1972",
    "International Conference on the Revision of the HNS Convention",
    "International Convention for the Prevention of Pollution from Ships",
    "Minamata Convention on Mercury",
    "Stockholm Convention on Persistent Organic Pollutants",
    "Rotterdam Convention on the Prior Informed Consent Procedure for Certain Hazardous Chemicals and Pesticides in International Trade",
    "International Convention for the Safe and Environmentally Sound Recycling of Ships",
    "UNCED", "UNCHE", "UNCSD", "UN Environment Assembly",                                                                                                           
    "UNEP Governing Council", "UNESCO IOC", "UNESCO World Heritage Convention",                                                                                                  
    "UNFCCC", "United Nations Forum on Forests", 
    "Vienna Convention for the Protection of the Ozone Layer",                                                                           
    "WSSD"
  ),
  short = c("MP", "CBD", "ITPGRFA", "CHW","CITES","CMS","COFO",
            "CPM","HNS-OPRC","ICCD","ICCM", "ITTO", "LC", "IMO\nLEG",
            "MARPOL","MC","POPS","RC","SRC",
            "UNCED", "UNCHE", "UNCSD", "UNEA",
            "UNEP\nGC", "UNESCO\nIOC", "UNESCO\nWC",                                                                                                  
            "UNFCCC", "UNFF", "VC", "WSSD")
)

# Include shortconf as an attribute
simple_eventNet <- set_vertex_attr(simple_eventNet, 
                                   "shortconf", index = V(simple_eventNet),
                            as.character(short_conf$short))

# Include shortconf as the label
V(simple_eventNet)$label <- V(simple_eventNet)$shortconf


# 2C - VISUALIZING THE NETWORKS OF EVENTS ------------

# Fig2 - Simplified events network (1970-2018) ------------

#.... Calculating node centrality -----
# For the event graphs, we use simple degree centrality (n of connections). 
# This reflects our goal of illustrating main agendas, not which connect
# different parts of the community (as would betweenness)

simpEv_deg <- degree(simple_eventNet, mode = 'all')

# weighted degree
# simpEv_wdeg <- strength(simple_eventNet, mode = "all") 
# weight for UNFCCC is so large it just won't make any sense in the graph

# betweeness centrality
# simpEv_betw <- betweenness(simple_eventNet)

#.... Generating the figure ------

# As there is much variation with the attributes (weight, degree, etc),
# we do figures only partly proportional to attributes.
# thus, we divide the variables and add absolute values

set.seed(123) #set seed to guarantee reproducibility of figure layout  

coords <- layout_(simple_eventNet, with_gem())
plot(simple_eventNet, layout = coords,
     vertex.size = 5 + simpEv_deg/2.5,
     # vertex.size = 5 + simpEv_betw/5,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width=E(simple_eventNet)$weight/30
     )

# Including a legend
  #### note: colors were not taken automatically, so if change above
  #### need to manually change here
df_legend <- tibble(
  tema = c("Ozônio", "Biodiversidade", "Biodiversidade - Espécies",
           "Lixo tóxico e químicos", "Florestas", "Oceano",
           "Grandes conferências ONU", "Desertificação", "Governança ambiental", 
           "Biodiversidade - UNESCO", "Clima"),
  labels = c("Ozone","Biodiversity", "Species",
             "Chemical and Toxic Waste","Forests","Oceans",
             "UN Sustainable Dev","Desertification","Env Governance",
             "UNESCO Heritage","Climate"),
  colors = c("dodgerblue3","darkgreen","paleturquoise3",
             "slateblue3", "darkseagreen3", "royalblue4",
             "darkorange2", "goldenrod", "coral4",
             "pink4","firebrick3")
)

legend("bottomright",legend=df_legend$labels, 
       col= df_legend$colors,
       bty = "n", pt.cex = 1.5,
       cex = .7, pch=16,
       inset = -0.15, xpd = TRUE
)

# Including title
title("Figure 2. Multilateral Environmental Events Network (1970-2018)",
      sub = "Collapsed by conference/treaty")

# Still needs to be improved: legend is weird, would be nice to increase dist
# between nodes, font/color in some labels are not working.
#.... Saving output -------
dev.copy(pdf, paste0("Figures/","Fig2", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()






rm(simple_eventNet)
# Fig3 - 70s Events network (1970-1979) -----------
# The decade separated figures could be generated through a function
# but since figures usually need some tweaking, will do manually

events70net <- filternet_byyear(full_eventNet, 1970, 1979)

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
coords <- layout_(events70net, with_fr())
  #using Fruchterman-Reingold with standard parameters

# Calculate degree centrality
Ev70_deg <- degree(events70net, mode = "all")

plot(events70net,
     layout = coords,
     vertex.size = 10 + Ev70_deg,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width= 5 + E(events70net)$weight
)

# Including legend
# legend should only have issues present at decade
temas_presentes <- V(events70net)$tema %>% unique
temp_legend <- filter(df_legend, tema %in% temas_presentes)


legend("right",legend=temp_legend$labels, 
       col= temp_legend$colors,
       bty = "n", pt.cex = 1,
       cex = .7, pch=16,
       inset = -0.15, xpd = TRUE
)

# Including title
title("Figure 3. Multilateral Environmental Events Network (1970-1979)")

#.... Saving output -------
dev.copy(pdf, paste0("Figures/","Fig3", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()



# Fig4 - 80s Events network (1980-1989) -----------
rm(events70net)
# The decade separated figures could be generated through a function
# but since figures usually need some tweaking, will do manually

events80net <- filternet_byyear(full_eventNet, 1980, 1989)

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
coords <- layout_(events80net, with_fr())

# Calculate degree centrality
Ev80_deg <- degree(events80net, mode = "all")

plot(events80net,
     layout = coords,
     vertex.size = 10 + Ev80_deg,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width= 5 + E(events80net)$weight
)


# Including legend
temas_presentes <- V(events80net)$tema %>% unique
temp_legend <- filter(df_legend, tema %in% temas_presentes)

legend("right",legend=temp_legend$labels, 
       col= temp_legend$colors,
       bty = "n", pt.cex = 1,
       cex = .7, pch=16,
       inset = -0.15, xpd = TRUE
)

# Including title
title("Figure 4. Multilateral Environmental Events Network (1980-1989)")

#.... Saving output -------
dev.copy(pdf, paste0("Figures/","Fig4", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()


# Fig5 - 90s Events network (1990-1999) -----------
dev.off()
rm(events80net)
# The decade separated figures could be generated through a function
# but since figures usually need some tweaking, will do manually

events90net <- filternet_byyear(full_eventNet, 1990, 1999)

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
coords <- layout_(events90net, with_fr())
  # this layout is bad, as too cluttered.
  # igraph deprecated some of the commands to change parameters (eg repulsion)
  # Using multienrichjam uses qgraph implementation to tweak them.
  # remotes::install_github("jmw86069/multienrichjam",
  #                         dependencies=TRUE);

spaced_coords <- multienrichjam::layout_with_qfr(events90net,
                                                 repulse = 4,
                                                 repulse.rad = 67^4-(1.9*(10^7))
) #repulse radius helps adjust relative spacing: the larger, the tighter
# default = n(vertices)^repulse


# Calculate degree centrality
Ev90_deg <- degree(events90net, mode = "all")

# As we now have too many events, must reduce labels
# Only show label if node is among the top largest degrees
top_deg <- Ev90_deg %>% sort(decreasing = T) %>% head(3)
V(events90net)$origlabel <- V(events90net)$label
V(events90net)$label <- if_else(V(events90net)$name %in% names(top_deg),
                             as.character(V(events90net)$origlabel), "")




# Making the plot

par(mar=c(2,1,1,1)+0.1) #changes margins for plot

plot(events90net,
     layout = spaced_coords,
     vertex.size = 3.5 + Ev90_deg/5,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .6,
     edge.width= 1 + E(events90net)$weight/10
)


# Including legend
temas_presentes <- V(events90net)$tema %>% unique
temp_legend <- filter(df_legend, tema %in% temas_presentes)

legend("right",legend=temp_legend$labels, 
       col= temp_legend$colors,
       bty = "n", pt.cex = 1,
       cex = .7, pch=16,
       inset = -0.1, xpd = TRUE
)

# Including title
title("Figure 5. Multilateral Environmental Events Network (1990-1999)")


#.... Saving output -------
dev.copy(pdf, paste0("Figures/","Fig5", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()

# Fig6 - 00s Events network (2000-2009) -----------
dev.off()
rm(events90net)
# The decade separated figures could be generated through a function
# but since figures usually need some tweaking, will do manually

events00net <- filternet_byyear(full_eventNet, 2000, 2009)

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
# coords <- layout_(events00net, with_fr())
# this layout is bad, as too cluttered.
# igraph deprecated some of the commands to change parameters (eg repulsion)
# Using multienrichjam uses qgraph implementation to tweak them.
# remotes::install_github("jmw86069/multienrichjam",
#                         dependencies=TRUE);

spaced_coords <- multienrichjam::layout_with_qfr(events00net,
                                                 repulse = 3.5,
                                                 repulse.rad = (vcount(events00net)^3.2)-(2*10^5)
                                                 )

#repulse radius helps adjust relative spacing: the larger, the tighter
# default = n(vertices)^repulse


# Calculate degree centrality
Ev00_deg <- degree(events00net, mode = "all")

# As we now have too many events, must reduce labels
# Only show label if node is among the top largest degree
top_deg <- Ev00_deg %>% sort(decreasing = T) %>% head(3)
V(events00net)$origlabel <- V(events00net)$label
V(events00net)$label <- if_else(V(events00net)$name %in% names(top_deg),
                                as.character(V(events00net)$origlabel), "")




# Making the plot
plot(events00net,
     layout = spaced_coords,
     vertex.size = 4 + Ev00_deg/5,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .6,
     edge.width= 1 + E(events00net)$weight/10
)


# Including legend
temas_presentes <- V(events00net)$tema %>% unique
temp_legend <- filter(df_legend, tema %in% temas_presentes)

legend("right",legend=temp_legend$labels, 
       col= temp_legend$colors,
       bty = "n", pt.cex = 1,
       cex = .7, pch=16,
       inset = -0.15, xpd = TRUE
)

# Including title
title("Figure 6. Multilateral Environmental Events Network (2000-2009)")

#.... Saving output -------
dev.copy(pdf, paste0("Figures/","Fig6", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()

# Extra - 10s Events network (2010-2018) -----------
rm(events00net)
# The decade separated figures could be generated through a function
# but since figures usually need some tweaking, will do manually

events10net <- filternet_byyear(full_eventNet, 2010, 2018)

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
# coords <- layout_(events10net, with_fr())
# this layout is bad, as too cluttered.
# igraph deprecated some of the commands to change parameters (eg repulsion)
# Using multienrichjam uses qgraph implementation to tweak them.
# remotes::install_github("jmw86069/multienrichjam",
#                         dependencies=TRUE);

spaced_coords <- multienrichjam::layout_with_qfr(events10net,
                                                 repulse = 3.5,
                                                 repulse.rad = (vcount(events10net)^3.2)-(3*10^5)
)

#repulse radius helps adjust relative spacing: the larger, the tighter
# default = n(vertices)^repulse


# Calculate degree centrality
Ev10_deg <- degree(events10net, mode = "all")

# As we now have too many events, must reduce labels
# Only show label if node is among the top largest degree
top_deg <- Ev10_deg %>% sort(decreasing = T) %>% head(3)
V(events10net)$origlabel <- V(events10net)$label
V(events10net)$label <- if_else(V(events10net)$name %in% names(top_deg),
                                as.character(V(events10net)$origlabel), "")




# Making the plot
plot(events10net,
     layout = spaced_coords,
     vertex.size = 5 + Ev10_deg/10,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .6,
     edge.width= 1 + E(events10net)$weight/10
)


# Including legend
temas_presentes <- V(events10net)$tema %>% unique
temp_legend <- filter(df_legend, tema %in% temas_presentes)

legend("right",legend=temp_legend$labels, 
       col= temp_legend$colors,
       bty = "n", pt.cex = 1,
       cex = .7, pch=16,
       inset = -0.15, xpd = TRUE
)

# Including title
title("Figure. Multilateral Environmental Events Network (2010-2018)")


#.... Saving output -------
dev.copy(pdf, paste0("Figures/","Figextra", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()

# STEP 3 - THE INDIVIDUALS PROJECTED NETWORK (STATIC) ------
# As we still have the incidence matrix from before, we can use it to
# create the other projection.

# Creating unipartite projections ----
#.... Projecting an Individual-Individual Network -----
# Individual-Individual Matrix through cross-Product Method
matriz_individuos <- incidence_matrix %*% t(incidence_matrix)

# We can calculate matrix diagonal, giving us n of events per individual
n_of_events <- diag(matriz_individuos)

# This allows us to see which individuals participated in more than X confs
# which(n_of_events > 10) -> most_part
# individuos %>% filter(
# id_individuo %in% most_part) %>% pull(nome_padrao) %>% unique

# Before creating the network projection, diagonal should be zeroed 
# If we don't do that, there will be loops for the nodes in the projection
diag(matriz_individuos) <- 0

# Network: indiv projection as nodes, coparticipation as edges and weight
# (more coparticipation = more weight)
personNet <- graph.adjacency(matriz_individuos, mode = "undirected",
                             weighted = TRUE)

rm(matriz_individuos) #clean up space by removing object

# This is a very large graph. This strategy, however, is a problematic one
# as it drops information on year of connections.
# data here is individuals (which is constant throughout years)
# the edges are the ones that vary year by year
# With this matrix, I can only include the name attribute (as org is indiv-year)


# Including attributes in the network -----------
# Individuos dataframe has duplicates, as it was created to standardize names.
# To use names as attributes, we need to remove this duplication
individuos_simple <- select(individuos, c(id_individuo, nome_padrao)) %>% distinct()

personNet <- set_vertex_attr(personNet, "name", index = V(personNet),
                            as.character(individuos_simple$nome_padrao))

personNet <- set_vertex_attr(personNet, "n_events", index = V(personNet),
                             n_of_events)

# person_degrees <- degree(personNet, mode = "all")
# summary(person_degrees)
# sort(person_degrees, decreasing = T) %>% head(30) %>% View


# Filtering the network by number of events attended -----------

# This network is too large, demanding too much of processing 
# We can reduce its size by 'discarding' individuals that don't participate
# in many events, which may be deemed less relevant.
# As our interest is in a policy community, this makes sense: 
# we want individuals who are regular participants in multilateral events.

# Data is very concentrated in individuals who only participate in or two events:
freq <- as.data.frame(table(n_of_events))
ggplot(freq, aes(x = n_of_events, y = Freq)) + 
  geom_bar(stat = 'identity')


# We thus filter the network, including only those who participate in 2+ events
filt_personNet <- induced_subgraph(personNet,
                                   V(personNet)[n_events >= 2])

# create label as two last words in name
V(filt_personNet)$label <- str_extract(V(filt_personNet)$name, '\\w+\\s\\w+$')

#.... Generating visualization
# If we want, we can now produce an illustration of the filtered personNet


set.seed(123) #set seed to guarantee reproducibility of figure layout  
coords <- layout_(filt_personNet, with_fr())

# As we now have too many events, must reduce labels
# Only show label if node is among the top-20 largest degree
filt_person_between <- betweenness(filt_personNet)

top_betw <- filt_person_between %>% sort(decreasing = T) %>% head(10)

V(filt_personNet)$origlabel <- V(filt_personNet)$label

V(filt_personNet)$label <- if_else(V(filt_personNet)$name %in% names(top_betw),
                                as.character(V(filt_personNet)$origlabel), "")


# Making the plot
plot(filt_personNet,
     layout = coords,
     vertex.size = 4 + filt_person_between/800,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .6,
     edge.width= 1 + E(filt_personNet)$weight/4
)


# STEP 4 - INCLUDING MORE ATTRIBUTES IN INDIVIDUAL NETWORK ----------
# As seen above, the strategy of directly plotting individuals is not good
# for visualizing structural patterns. This happens because we are using
# 'stable' individual ids (fixed in time) and our info on individuals
# (mainly, ORGs and ORG types) is actually dynamic, changing over time.
# While there might be a way to get year and other event characteristics
# as edge attributes, it is probably easier (and safer) to remake the net
# from scratch.

# There are two ways to go about this:
# 1) Creating a dynamic graph from data. IndYear would be nodes with
# Year, Person Id, Org Id as attributes
# (year and other event characteristics could also come as edge attribute)
# This is the most thorough solution and requires other packages, such as 
# networkDynamic or ndtv. It is more complex but much more flexible.

# 2) Collapse IndYear data as Organizations, while keeping year data as info
# on the edges. This is simpler and loses some features (dynamic changes in
# individual membership to org) but is easier to understand and interpret, as
# well as to implement. Also need to remake matrix from scratch but otherwise
# can be done with same tools as above.

# In the future, solution 1 should still be pursued.
# If we keep individuals' dynamic information, we will be able to better
# describe the network: we can show net evolution, individuals' org membership
# (which may change over time), and simplify the network as an organization
# network itself (this can also be done directly, with Org-Event ties instead
# of Indiv-Event ties [as described in 2], with different results).

# Generating a new incidence matrix -------

incidence_matrix <- matriz_part_evento %>% 
select(id_indevento, id_evento) %>% # use data of individuals and events
  table() # count if there is data in each combination of individual and event
class(incidence_matrix) <- "matrix" # converts the dataframe into matrix object

# Creating a unipartite projection ------
#.... Projecting an IndYear-IndYear Network -----
# IndYear-IndYear Matrix through cross-Product Method
matriz_ind_ano <- incidence_matrix %*% t(incidence_matrix)

# We can calculate matrix diagonal, giving us n of events per individual-year
personyr_n_of_events <- diag(matriz_ind_ano)

# Before creating the network projection, diagonal should be zeroed 
# If we don't do that, there will be loops for the nodes in the projection
diag(matriz_ind_ano) <- 0

# Network: indiv projection as nodes, coparticipation as edges and weight
# (more coparticipation = more weight)
personyrNet <- graph.adjacency(matriz_ind_ano, mode = "undirected",
                             weighted = TRUE)

# Including attributes in the network -----------
# For easier manipulation, will include as dataframe, not as igraph object
node_list <- as_data_frame(personyrNet, "vertices")

# Include other ids (individual and event)
node_list$id_indevento <- as.integer(node_list$name)
node_list$id_individuo <- matriz_part_evento[#id indivíduo
  node_list$id_indevento==matriz_part_evento$id_indevento,
]$id_individuo

node_list$id_evento <- matriz_part_evento[#id evento
  node_list$id_indevento==matriz_part_evento$id_indevento,
]$id_evento

# include individual name (nome padrao)
individuos_unico <- individuos %>% select(-nome) %>% distinct()
node_list <- left_join(node_list, individuos_unico)

# include year of participation in event
node_list <- left_join(node_list,
                       select(eventos, c(id_evento, ano))) %>% 
  rename(ano_part = ano)

# include number of participations in events per individual
node_list$nparticipations <- personyr_n_of_events[
   node_list$id_indevento==names(personyr_n_of_events)]

# include individuals genders
# This is not working, as some names are not being read correctly
# see, e.g., Luiz Gylvan Meira Filho. Probably needs some kind of cleanup
# node_list <- node_list %>% mutate(
#   gender = genderBR::get_gender(nome_padrao)
# )

#.... Including info on organizations -----
# to do this, we must first import organization data
orgs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/orgs-2022-06-29.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(org_sujo = col_skip(), 
                                                                        org_detalhe_sujo = col_skip(), org_detalhe_limpo = col_skip()), 
                   locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE)

# for some reason, we have duplicated rows in orgs (eg id_org_dupla 89, MRE, is duplicated)
# should confirm if there was any manual error that caused this in previous db updates
# (as far as I looked, doesnt seem to have erased any previous org but double check
# if there is any reason for this before correcting in the file.)
# so correction is only done within script:
orgs <- distinct(orgs)



class <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/class-2022-06-29.csv", 
                    delim = ";", escape_double = FALSE, col_types = cols(org_limpo = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE)

# the same thing happens in class - might be good to review the update scripts
# as there are some NAs, probable this was a result of the update files in excel
# only being appended to original and no cleaning afterwards.
class <- class %>% filter(is.na(id_org_unica) == F) %>% distinct()


# joining the databases
chave_orgs <- left_join(select(delegs, c(id_indevento, id_org_dupla)),
                        orgs
) %>% left_join(class) %>% distinct()


# Include organization data in node list
node_list <- left_join(node_list, chave_orgs)
node_list$id_org_dupla <- NULL

##.... Including year and issue as edge attributes ------
edge_list <- as_data_frame(personyrNet, "edges")

# Including event id
edge_list$id_indevento <- as.integer(edge_list$from)
edge_list <- left_join(edge_list, 
                       select(matriz_part_evento, c(id_indevento, id_evento))
)

# Including event info
edge_list <- left_join(edge_list, eventos) %>%
  select(-c(data, id_indevento))

#.... Remaking the network ----
personyrNet <- graph_from_data_frame(edge_list, directed = FALSE,
                                   vertices = node_list
)
# we can check attributes
# vertex_attr_names(personyrNet)
# edge_attr_names(personyrNet)

# STEP 5: THE ORGANIZATIONS COLLAPSED NETWORK ------------
# 5A - TRANSFORMING INDIVID+YEAR NETWORK INTO AN ORGANIZATION NETWORK -----
# While this represents an inevitable loss of information, it should
# also help in interpreting and discerning structures

#.... Filtering years of interest for the network
# At this point of the research, we are not interested in a full picture
# of organizations (ie, covering the whole period).
# We will first filter the period of interest, then collapse to orgs
# this should avoid some problems (e.g. allowing us to properly filter
# by org info such as n of events attended on that period, not complete database)

yearfilter_net <- function(net, min, max){
  net <- induced_subgraph(net,
                          V(net)[ano_part >= min & ano_part <= max])
  net
}

collapse_ind2org <- function(indyearnet){
  org_list <- unique(V(indyearnet)$org_limpo)
  
  org_yrnet <- contract(indyearnet,
                        mapping = as.numeric(
                          plyr::mapvalues(V(indyearnet)$org_limpo, 
                                          from = org_list, 
                                          to = 1:length(org_list))
                        ),
                        vertex.attr.comb=list(#methods for combining attributes
                          weight="sum", #sum weights,
                          nparticipations = "sum",  #sum nparticipations
                          "first")) # keep first of others
  
  org_yrnet <- simplify(org_yrnet) # sum weights to simplify network
  
  # Remove vertex of org No Info (id-org-unica = 565), as they are not
  # really the same organization and this distorts data.
  org_yrnet <- delete_vertices(org_yrnet, V(org_yrnet)$id_org_unica == 565)
  
  # As we now collapsed as organizations, various attributes stop making sense
  # Let's remove them to avoid misiterpretation and clean up space
  attrs_to_remove <- c("id_indevento", "id_individuo",
                       "id_evento", "nome_padrao", "ano_part")
  
  for (i in seq_along(attrs_to_remove)){
    print(attrs_to_remove[i])
    org_yrnet <- remove.vertex.attribute(org_yrnet, attrs_to_remove[i])
  }
  org_yrnet
}

###.... Preparing a pallette based on type ----
# As there are too many categories of organization in total (24),
# we will use the simplified categorization (10). This allows us
# to better compare across figures.
types <- class %>% select(tipo_org_reduzido) %>% distinct() %>% 
  filter(is.na(tipo_org_reduzido)==F)

pallette <- RColorBrewer::brewer.pal(9, "Set1") %>% rev()
types$color <- pallette
types$label <- c("Other","Business","Federal Gov (non MFA)",
                 "Subnational","Civil Society","Academia",
                 "Parliament","Not identified","MFA")



preparing_nodeappearance <- function(orgdecadenet, nlabelstoshow){
  #.... Calculating node centrality
  
  # We will use betweenness centrality here, as we are particularly interested
  # in the influence/capacity of orgs to mediate/control the flow of information
  
  org_betw <- betweenness(orgdecadenet)
  
  # # Only show label if node is among the top largest degree
  # top_deg <- org_betw %>% sort(decreasing = T) %>% head(nlabelstoshow)
  # V(orgdecadenet)$origlabel <- V(orgdecadenet)$org_limpo
  # 
  # V(orgdecadenet)$label <- if_else(V(orgdecadenet)$name %in% names(top_deg),
  #                                  as.character(V(orgdecadenet)$origlabel), "")
  
  # include betweenness as vertex attribute
  orgdecadenet <- set.vertex.attribute(orgdecadenet, "betw", 
                                       index = V(orgdecadenet), 
                                       betweenness(orgdecadenet,
                                                   normalized = T))
  
  ### Note: 
  # As we will be dealing with centrality measures, we should be aware of a divergence
  # between igraph and other packages (and, I believe, Gephi). Igraph treats edge weight
  # as a measure of cost/friction in relations, not as a strength (measure of capacity/
  # bandwith of the edge). This is NOT what we want.
  # So when we are working with measures that use weight in calculation,
  # we will need to use an inverted measure of weight (1/weight)
  
  E(orgdecadenet)$origweight <- E(orgdecadenet)$weight
  # this makes weight inverted
  E(orgdecadenet)$weight <- 1/E(orgdecadenet)$origweight
  
  # We will maintain this inversion just to calculate betweenness.
  # Then, we will return it to normal, as our layout (Fruchterman Reingold) uses
  # weight in the other way.
  
  org_invbetw <- betweenness(orgdecadenet)
  
  # Only show label if node is among the top largest degree
  top_deg <- org_invbetw %>% sort(decreasing = T) %>% head(nlabelstoshow)
  V(orgdecadenet)$origlabel <- V(orgdecadenet)$org_limpo
  
  V(orgdecadenet)$label <- if_else(V(orgdecadenet)$name %in% names(top_deg),
                                   as.character(V(orgdecadenet)$origlabel), "")
  
  # include betweenness as vertex attribute
  orgdecadenet <- set.vertex.attribute(orgdecadenet, "invbetw", 
                                       index = V(orgdecadenet), 
                                       betweenness(orgdecadenet,
                                                   normalized = T))
  # Return edge weight to the original
  E(orgdecadenet)$invweight <- E(orgdecadenet)$weight
  E(orgdecadenet)$weight <- E(orgdecadenet)$origweight
  
  #.... Coloring nodes by simplified organization type 
  
  index <- data.frame(tipo_org_reduzido = V(orgdecadenet)$tipo_org_reduzido)
  index <- left_join(index, types)
  V(orgdecadenet)$color <- index$color
  V(orgdecadenet)$type <- index$label
  orgdecadenet
}

# function to calculate centrality at the networks for selected orgs
central_stats <- function(orgdecadenet, measure, lglnormal = F, decade,
                          invertweight = F){
  
  df <- data.frame(#ids for mre, mma, mapa, mct 
    name = V(orgdecadenet)[V(orgdecadenet)$id_org_unica %in% c(435, 442, 421, 422)]$name,
    org = V(orgdecadenet)[V(orgdecadenet)$id_org_unica %in% c(435, 442, 421, 422)]$org_limpo
  )
  
  if(invertweight){E(orgdecadenet)$weight <- 1/E(orgdecadenet)$weight}
  
  if(measure == "betweenness"){
    stats <- betweenness(orgdecadenet, directed = F,
                         normalized = lglnormal) #calculate for all
  }else if(measure == "closeness"){
    stats <- closeness(orgdecadenet, normalized = lglnormal)
  }else if(measure == "eigenvector"){
    lglnormal <- NA
    stats <- eigen_centrality(orgdecadenet)$vector
  }else if(measure == "pagerank"){
    lglnormal <- NA
    stats <- page_rank(orgdecadenet)$vector
  }
  
  # only keep values for selected orgs
  stats <- data.frame(name = names(stats), value = stats)
  stats <- add_row(stats, name = "avg", value = mean(stats$value)) %>% 
    filter(name %in% df$name | name == "avg") %>% left_join(df)
  
  # include labels of calculation
  stats$measure <- measure
  stats$decade <- decade
  stats$normalized <- lglnormal
  stats$weightinverted <- invertweight
  stats
}


# 5B - VISUALIZING THE NETWORKS OF ORGANIZATIONS ------

# Fig7 - 70s Org network (1970-1979) -----------
#.... Filtering and collapsing networks by decade ---------
orgdecadenet <- yearfilter_net(personyrNet, 1970, 1979) %>% 
  collapse_ind2org() %>% preparing_nodeappearance(30)

# Due to the use of the database for a different purpose, the Ministry of the Navy
# was originally separated from the Navy as an organization. This is to allow for
# future ministerial changes (Ministry becomes the Ministry of Defense in the 00s
# which is not to be confused with the Navy itself)
# For our purposes, however, this separation does not make any sense. 
# So we need to fuse these vertices
V(orgdecadenet)$combine <- if_else(
  V(orgdecadenet)$org_limpo == "Ministério da Marinha",
  "Marinha do Brasil", #rename Ministério da Marinha to Marinha do Brasil
  V(orgdecadenet)$org_limpo
)
# now combine the two vertices for this network
comb_list <- unique(V(orgdecadenet)$combine)
  
orgdecadenet <- contract(orgdecadenet,
                      mapping = as.numeric(
                        plyr::mapvalues(V(orgdecadenet)$combine, 
                                        from = comb_list, 
                                        to = 1:length(comb_list))
                      ),
                      vertex.attr.comb=list(#methods for combining attributes
                        weight="sum", #sum weights,
                        nparticipations = "sum",  #sum nparticipations
                        "first")) # keep first of others

orgdecadenet <- simplify(orgdecadenet)



#.... Saving statistics for analysis later -----

full_stats <- central_stats(orgdecadenet, "betweenness", lglnormal = F, "70s") %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", T, "70s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", F, "70s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", T, "70s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", F, "70s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", T, "70s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", F, "70s", invertweight = T)  ) %>%
  bind_rows( central_stats(orgdecadenet, "closeness", T, "70s", invertweight = T)  ) %>%
  bind_rows( central_stats(orgdecadenet, "eigenvector", decade = "70s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "eigenvector", decade = "70s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "pagerank", decade = "70s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "pagerank", decade = "70s", invertweight = T)  )



#.... Generating the figure ------

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
# coords <- layout_(orgdecadenet, with_fr())
# coords <- layout_(orgdecadenet, with_kk())
# KK functions as a spring, more weight giving more distance. 
# FR does the opposite: attract when more weight, repels others


spaced_coords <- multienrichjam::layout_with_qfr(orgdecadenet,
                                                 repulse = 3.5,
                                                 repulse.rad = (vcount(orgdecadenet)^3.2)-(2*10^5)
)

# Change labels: name for top5 between orgs
short_lab <- data.frame( long = V(orgdecadenet)$label) %>% filter(long != "")
# short_lab$short <- c("MRE","IPqM","BNH","IBGE","UFRGS")
# n = 10
# short_lab$short <- c("MRE","IPqM","BNH","IBGE", "MDIC", "MAPA","MS","SP","PR","UFRGS")
# n = 30
short_lab$short <- c("IBDF", "MRE", "SUDAM", "EMBRAPA", "UNDP-FAO-IBDF","MInt","IPqM",
                     "BNH", "IBGE", "MDIC", "MAPA", "MS", "SP", "PR", "SEPLAN", 
                     "Senate", "Deputies", "CNI", "SANESP", "Llloyd", "DOCENAVE",
                     "Navy", "Petro", "RDEP", "Syndarma", "SUNAMAM", "UFRGS")

all_labs <- left_join(data.frame( long = V(orgdecadenet)$label),
                      short_lab)


V(orgdecadenet)$longlab <- V(orgdecadenet)$label
V(orgdecadenet)$label <- all_labs$short

# Plotting the figure
par(mar=c(2,1,1,1)+0.1) #set margins for the plot

plot(orgdecadenet,
     # layout = coords, 
     layout = spaced_coords,
     vertex.size = 5 + V(orgdecadenet)$invbetw*10,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width= 1 + E(orgdecadenet)$weight/15
)

# Including legend
tipos_presentes <- V(orgdecadenet)$type %>% unique
temp_legend <- filter(types, label %in% tipos_presentes)

legend("bottomright",legend=temp_legend$label, 
       col= temp_legend$color,
       bty = "n", pt.cex = 1.2,
       cex = .7, pch=16,
       inset = -0.05, xpd = TRUE
)

# Including title
title("Figure 7. Organizations Network (1970-1979)")

#.... Saving output -----

dev.copy(pdf, paste0("Figures/","Fig7", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
         )
dev.off()


rm(orgdecadenet)
# Fig8 - 80s Org network (1980-1989) -----------
#.... Filtering and collapsing networks by decade ---------
orgdecadenet <- yearfilter_net(personyrNet, 1980, 1989) %>% 
  collapse_ind2org() %>% preparing_nodeappearance(10)

#.... Saving statistics for analysis later -----

full_stats <- full_stats %>% 
  bind_rows(central_stats(orgdecadenet, "betweenness", lglnormal = F, "80s")) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", T, "80s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", F, "80s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", T, "80s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", F, "80s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", T, "80s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", F, "80s", invertweight = T)  ) %>%
  bind_rows( central_stats(orgdecadenet, "closeness", T, "80s", invertweight = T)  ) %>%
  bind_rows( central_stats(orgdecadenet, "eigenvector", decade = "80s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "eigenvector", decade = "80s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "pagerank", decade = "80s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "pagerank", decade = "80s", invertweight = T)  )


#.... Generating the figure ------

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
# coords <- layout_(orgdecadenet, with_fr())
# coords <- layout_(orgdecadenet, with_kk())
# KK functions as a spring, more weight giving more distance. 
# FR does the opposite: attract when more weight, repels others


spaced_coords <- multienrichjam::layout_with_qfr(orgdecadenet,
                                                 repulse = 3.5,
                                                 repulse.rad = (vcount(orgdecadenet)^3.2)-(2*10^5)
)

# Change labels: name for top between orgs
short_lab <- data.frame( long = V(orgdecadenet)$label) %>% filter(long != "")
short_lab$short <- c("MRE","EMBRAPA","IBDF", "FBCN","IBAMA",
                     "UNDP-FAO-IBDF","SEMAM", "Navy", "USP")
all_labs <- left_join(data.frame( long = V(orgdecadenet)$label),
                      short_lab)


V(orgdecadenet)$longlab <- V(orgdecadenet)$label
V(orgdecadenet)$label <- all_labs$short

# Plotting the figure
par(mar=c(2,1,1,1)+0.1) #set margins for the plot

plot(orgdecadenet,
     #layout = coords, 
      layout = spaced_coords,
     vertex.size = 5 + V(orgdecadenet)$invbetw*10,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width= 1 + E(orgdecadenet)$weight/15
)

# Including legend
tipos_presentes <- V(orgdecadenet)$type %>% unique
temp_legend <- filter(types, label %in% tipos_presentes)

legend("right",legend=temp_legend$label, 
       col= temp_legend$color,
       bty = "n", pt.cex = 1.2,
       cex = .7, pch=16,
       inset = -0.05, xpd = TRUE
)

# Including title
title("Figure 8. Organizations Network (1980-1989)")

#.... Saving output -----

dev.copy(pdf, paste0("Figures/","Fig8", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()


rm(orgdecadenet)

# Fig9 - 90s Org network (1990-1999) -----------
#.... Filtering and collapsing networks by decade ---------
orgdecadenet <- yearfilter_net(personyrNet, 1990, 1999) %>% 
  collapse_ind2org() %>% preparing_nodeappearance(10)

#.... Saving statistics for analysis later -----

full_stats <- full_stats %>% 
  bind_rows(central_stats(orgdecadenet, "betweenness", lglnormal = F, "90s")) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", T, "90s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", F, "90s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", T, "90s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", F, "90s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", T, "90s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", F, "90s", invertweight = T)  ) %>%
  bind_rows( central_stats(orgdecadenet, "closeness", T, "90s", invertweight = T)  ) %>%
  bind_rows( central_stats(orgdecadenet, "eigenvector", decade = "90s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "eigenvector", decade = "90s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "pagerank", decade = "90s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "pagerank", decade = "90s", invertweight = T)  )




#.... Generating the figure ------

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
# coords <- layout_(orgdecadenet, with_fr())
# coords <- layout_(orgdecadenet, with_kk())
# KK functions as a spring, more weight giving more distance. 
# FR does the opposite: attract when more weight, repels others
#for this 70s graph, Kamada-Kawai shows the structure better, as distancing the 
#'main bloc' from MRE allows us to see its structure more clearly

spaced_coords <- multienrichjam::layout_with_qfr(orgdecadenet,
                                                 repulse = 3.5,
                                                 repulse.rad = (vcount(orgdecadenet)^3)-(2*10^5)
)

# Change labels: name for top between orgs
short_lab <- data.frame( long = V(orgdecadenet)$label) %>% filter(long != "")
short_lab$short <- c("MRE","MMA","MPOG", "MAPA","SP",
                     "AL","INPE", "RJ", "Navy","Senate")
all_labs <- left_join(data.frame( long = V(orgdecadenet)$label),
                      short_lab)


V(orgdecadenet)$longlab <- V(orgdecadenet)$label
V(orgdecadenet)$label <- all_labs$short

# Plotting the figure
par(mar=c(2,1,1,1)+0.1) #set margins for the plot

plot(orgdecadenet,
     # layout = coords, 
     layout = spaced_coords,
     vertex.size = 3 + V(orgdecadenet)$invbetw*10,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width= 1 + E(orgdecadenet)$weight/100
)

# Including legend
tipos_presentes <- V(orgdecadenet)$type %>% unique
temp_legend <- filter(types, label %in% tipos_presentes)

legend("right",legend=temp_legend$label, 
       col= temp_legend$color,
       bty = "n", pt.cex = 1.2,
       cex = .7, pch=16,
       inset = -0.05, xpd = TRUE
)

# Including title
title("Figure 9. Organizations Network (1990-1999)")

#.... Saving output -----

dev.copy(pdf, paste0("Figures/","Fig9a", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()



#.... Alternative figure: only orgs with 3+ participations -----
# To clear up the graph and better look at the community structure, 
# it might be more interesting to limit the organizations in the figure.
# Normally, this would be done by filtering vertex degree (n of connections).
# This would not be what we want, as someone who participates in only one COP or at UNCED
# might have a higher degree than someone at 10 smaller events.
# What we want, instead, is the number of event participations per org as the filter.

filtdecadenet <- induced_subgraph(orgdecadenet, 
                 V(orgdecadenet)[nparticipations  >= 3])

set.seed(123)
l <- layout_(filtdecadenet, with_fr())
#kk highlights MRE centrality by pushing it away but makes structure otherwise cluttered
# l <- layout_(filtdecadenet, with_kk()) 

#recalculate betweenness for subcommunity
# filtdecadenet <- set.vertex.attribute(filtdecadenet, "newbetw", 
#                                      index = V(filtdecadenet), 
#                                      betweenness(filtdecadenet, normalized = T))

#not using this in this paper but comparison of newbetw vs general betw is great
#can help identify different functions: eg, some, like MRE, connect inside-outside and
#others connect inside the community. Should remember to use INVERTED WEIGHT as well.

# plot
par(mar=c(2,1,1,1)+0.1) #set margins for the plot

plot(filtdecadenet,
     layout = l, 
     # layout = spaced_coords,
     vertex.size = 3 + V(filtdecadenet)$invbetw*10,
     # vertex.size = 3 + V(filtdecadenet)$newbetw/5,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width= 1 + E(filtdecadenet)$weight/100
)

title("Figure 11. Filtered Organizations Network (1990-1999, 3+)",
      sub = "Orgs with 3 or more event participations")

# Save
dev.copy(pdf, paste0("Figures/","Fig9b", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()

# Clean
rm(orgdecadenet)


# Fig10 - 00s Org network (2000-2009) -----------
#.... Filtering and collapsing networks by decade ---------
orgdecadenet <- yearfilter_net(personyrNet, 2000, 2009) %>% 
  collapse_ind2org() %>% preparing_nodeappearance(10)

#.... Saving statistics for analysis later -----

full_stats <- full_stats %>% 
  bind_rows(central_stats(orgdecadenet, "betweenness", lglnormal = F, "00s")) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", T, "00s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", F, "00s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "betweenness", T, "00s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", F, "00s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", T, "00s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "closeness", F, "00s", invertweight = T)  ) %>%
  bind_rows( central_stats(orgdecadenet, "closeness", T, "00s", invertweight = T)  ) %>%
  bind_rows( central_stats(orgdecadenet, "eigenvector", decade = "00s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "eigenvector", decade = "00s", invertweight = T)  ) %>% 
  bind_rows( central_stats(orgdecadenet, "pagerank", decade = "00s")  ) %>% 
  bind_rows( central_stats(orgdecadenet, "pagerank", decade = "00s", invertweight = T)  )
  

#.... Generating the figure ------

# Creating layout
set.seed(123) #set seed to guarantee reproducibility of figure layout  
# coords <- layout_(orgdecadenet, with_fr())
# coords <- layout_(orgdecadenet, with_kk())


# KK functions as a spring, more weight giving more distance. 
# FR does the opposite: attract when more weight, repels others

spaced_coords <- multienrichjam::layout_with_qfr(orgdecadenet,
                                                 repulse = 3.5,
                                                 repulse.rad = (vcount(orgdecadenet)^2.4)-(4*10^5)
)

# Change labels: name for top between orgs
short_lab <- data.frame( long = V(orgdecadenet)$label) %>% filter(long != "")
short_lab
short_lab$short <- c("MS","Vitae","MDA", "CC-PR","EMBRAPA",
                     "Rio","", "", "","")
all_labs <- left_join(data.frame( long = V(orgdecadenet)$label),
                      short_lab)


V(orgdecadenet)$longlab <- V(orgdecadenet)$label
V(orgdecadenet)$label <- all_labs$short
# Manually include relevant labels
V(orgdecadenet)[id_org_unica == 435]$label <- "MRE"
V(orgdecadenet)[id_org_unica == 442]$label <- "MMA"
V(orgdecadenet)[id_org_unica == 422]$label <- "MCT"
V(orgdecadenet)[id_org_unica == 421]$label <- "MAPA"



# Plotting the figure
par(mar=c(2,1,1,1)+0.1) #set margins for the plot

plot(orgdecadenet,
     # layout = coords, 
     layout = spaced_coords,
     vertex.size = 3 + V(orgdecadenet)$invbetw*10,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width= 1 + E(orgdecadenet)$weight/2700
)

# Including legend
tipos_presentes <- V(orgdecadenet)$type %>% unique
temp_legend <- filter(types, label %in% tipos_presentes)

legend("right",legend=temp_legend$label, 
       col= temp_legend$color,
       bty = "n", pt.cex = 1.2,
       cex = .7, pch=16,
       inset = -0.05, xpd = TRUE
)

# Including title
title("Figure 10. Organizations Network (2000-2009)")

#.... Saving output -----
dev.copy(pdf, paste0("Figures/","Fig10a", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()

#.... Alternative figure: only orgs with 10+ participations -----
# To clear up the graph and better look at the community structure, 
# it might be more interesting to limit the organizations in the figure.
# Normally, this would be done by filtering vertex degree (n of connections).
# This would not be what we want, as someone who participates in only one COP or at UNCED
# might have a higher degree than someone at 10 smaller events.
# What we want, instead, is the number of event participations per org as the filter.

filtdecadenet <- induced_subgraph(orgdecadenet, 
                                  V(orgdecadenet)[nparticipations  >= 10])

set.seed(123)
# l <- layout_(filtdecadenet, with_kk()) 
# l <- layout_(filtdecadenet, with_fr())
l <- multienrichjam::layout_with_qfr(filtdecadenet,
                                                 repulse = 3.5,
                                                 repulse.rad = (vcount(orgdecadenet)^2.3)-(4*10^5)
)


# l <- layout_nicely(filtdecadenet)

#recalculate betweenness for subcommunity
# filtdecadenet <- set.vertex.attribute(filtdecadenet, "newbetw", 
#                                       index = V(filtdecadenet), 
#                                       betweenness(filtdecadenet, normalized = T))

#not using this in this paper but comparison of newbetw vs general betw is great
#can help identify different functions: eg, some, like MRE, connect inside-outside and
#others connect inside the community. Remember to INVERT WEIGHTS.

# plot
par(mar=c(2,1,1,1)+0.1) #set margins for the plot

plot(filtdecadenet,
     layout = l, 
     # layout = spaced_coords,
     vertex.size = 3 + V(filtdecadenet)$invbetw*10,
     # vertex.size = 3 + V(filtdecadenet)$newbetw/5,
     vertex.frame.color = "white",
     vertex.label.color = 'black',
     vertex.label.cex = .7,
     edge.width= 1 + E(filtdecadenet)$weight/2700
)

title("Figure 12. Filtered Organizations Network (2000-2009, 10+)",
      sub = "Orgs with 10 or more event participations")

# Save
dev.copy(pdf, paste0("Figures/","Fig10b", "-",Sys.Date(),".pdf"),
         width  = par("din")[1]*4,
         height = par("din")[2]*2.5,
         #family = "Helvetica-Narrow"
)
dev.off()
# Clean
rm(orgdecadenet)

write.csv2(full_stats, "centrality_measures.csv")


