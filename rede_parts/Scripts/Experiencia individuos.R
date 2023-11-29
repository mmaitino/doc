library(tidyverse)
library(igraph)
library(lubridate)

######### PREPARO DAS BASES ######################

# Importar os dados ----------
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
delegs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/deleg-2023-07-25.csv", 
                     ";", escape_double = FALSE, 
                     col_types = cols(cargo_deleg = col_skip(), 
                                      #cargo_org = col_skip(), 
                                      desc = col_skip(), fonte = col_skip(), 
                                      nline = col_skip(), org = col_skip(),
                                      org_detalhe = col_skip(), pais = col_skip(),
                                      titulo = col_skip()), 
                     #locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)

individuos <- read_csv("individuos-2023-07-27.csv", 
                       col_types = cols(padrao_antigo = col_skip(),
                                        nrow = col_skip()))

# criar id numérico p/ cada indivíduo e cd individuo-evento
delegs$id_indevento <- 1:nrow(delegs)

individuos %>% select(nome_padrao) %>% distinct() %>% mutate(
  id_individuo = row_number()
) %>% right_join(individuos) -> individuos


# Filtrar as bases - uso apenas dos dados de clima -----------

eventos <- eventos %>% filter(str_detect(conf, "UNFCCC"))
delegs <- delegs %>% filter(str_detect(conf, "UNFCCC"))


# Join das bases ------------
matriz_part_evento <- left_join(delegs, individuos) %>% 
  select(-c(nome, id_org_dupla, nome_padrao))

matriz_part_evento <- left_join(matriz_part_evento, 
                                select(eventos, c(conf, data, id_evento))
) %>% select(-conf)


# ordenar por data do evento (ordem é importante pro loop)
matriz_part_evento <- arrange(matriz_part_evento, data)

######### CRIANDO A TABELA DE EXPERIÊNCIA ######################

# Gerando as funções get experience ----
#absoluta
get_absexp <- function(event_id, absat0 = 0, tst = matriz_part_evento){
  exp_df %>% mutate(
    absat1 = if_else(ind %in% filter(tst,
                                     id_evento== (event_id - 1))$id_individuo,
                     absat0 + 1,
                     absat0)
    ) %>% pull(absat1) -> absat1
  absat1
}

#contínua (zera qdo falta uma)
get_contexp <- function(event_id, contat0 = 0, tst = matriz_part_evento){
  exp_df %>% mutate(
    contat1 = if_else(ind %in% filter(tst,
                                     id_evento== (event_id - 1))$id_individuo,
                     contat0 + 1,
                     0)
  ) %>% pull(contat1) -> contat1
  contat1
}

# Calculando os dados de experiência -----------

# Lista todos os ids de indivíduo presentes nas UNFCCC
lista_individuos <- pull(matriz_part_evento, id_individuo) %>% unique
# Lista todos os ids de evento referentes a UNFCCC
listEventid <- unique(pull(matriz_part_evento, id_evento))

# Cria o dataframe de experiência vazio
exp_df <- data.frame(matrix(nrow = length(lista_individuos),
                            ncol = 1+length(listEventid))) 
colnames(exp_df) <- append("ind", paste0("absat",listEventid)) #nomeia as colunas
exp_df$ind <- lista_individuos

# Rodando o loop:

for(i in 1:length(listEventid)){
  print(listEventid[i])
if(i == 1){#quando é o primeiro evento, precisa colocar o zero artificial pra funcionar
  exp_df[,i+1] <- get_absexp(listEventid[i], 0)} else {#nos demais usar a coluna anterior
    exp_df[,i+1] <- get_absexp(listEventid[i], exp_df[,i])
  }
}

######### EXPLORANDO DADOS DE EXPERIÊNCIA ######################

# Temos, agora, experiência absoluta por indivíduo a cada evento da COP
# São todos os indivíduos (2182) e todos os eventos (39, pois inclui INCs)
# Falta, então: 
# a) filtrar os indivíduos que me interessam - quero só gov? só os mais exp?
# b) relacionar os ids com nomes (tanto eventos como indivíduos)
# c) agrupar indivíduos por organização e tirar média (esse vai ser um pouquinho
# mais chato pra montar, pq fiz com id de individuo, nao ind-evento. entao tem que
# fazer esse match pra cada evento e depois reduzir)

# Gerando tabelas: quem são os brasileiros mais experientes? ------
# Há diferentes formas de responder. Incluo os INCs? Foram muitos em pouco tempo,
# o que pode distorcer. Divido por década? Divido por organização?

#..... Os 30 mais experientes ao final do período ----------
top_exp <- exp_df %>% select(ind, absat549) %>% arrange(desc(absat549)) %>% head(30)

# incluir nomes
include_names <- function(df){df %>% rename(id_individuo = ind) %>% 
    left_join(select(individuos, c(id_individuo, nome_padrao)) %>% distinct)
}

top_exp <- top_exp %>% include_names %>% rename(exp = absat549)
top_exp

#..... Os 30 mais experientes sem INCs ----------
# Esse método dá mais peso aos anos 90, pois foram às INCs (11 reuniões entre 91 e 95)
# e eu não incluo reuniões além das COPs para o resto do período (1 ao ano, exceto 2001)
# vamos ignorar as INCs no cálculo entao 
# (outra alternativa seria incluir apenas como 1 exp por ano, 
# mas aí acho que teria que mudar no cálculo geral do exp_df (colapsar eventos por ano))

# Para ignorar completamente, basta retirar a experiência adquirida antes da COP1.
# equivalente a todos iniciando a experiência em cops zerados
top_exp_cop <- exp_df %>% mutate(exp_cop = (absat549 - absat524)) %>%
  select(ind, exp_cop) %>% arrange(desc(exp_cop)) %>% head(30) %>% 
  include_names()
top_exp_cop

#..... Os 20 mais experientes por década ----------
#calculo exp entre COP06 (528, 1a de 2000) e INC1 (551, 1o dos 90)
#lembrando que o valor em t é sempre calculado com t-1 (exp adquirida ANTES do evento)
#por isso inclusão de 2000
top_exp90 <- exp_df %>% mutate(exp_cop = (absat528 - absat551)) %>%
  select(ind, exp_cop) %>% arrange(desc(exp_cop)) %>% head(20) %>% 
  include_names()
top_exp90

#calculo exp entre COP16 (540, 1a de 2010) e COP06 (528, 1a de 2000)
top_exp00 <- exp_df %>% mutate(exp_cop = (absat540 - absat528)) %>%
  select(ind, exp_cop) %>% arrange(desc(exp_cop)) %>% head(20) %>% 
  include_names()
top_exp00

#calculo exp entre COP25 (549, de 2019) e COP16 (540, 1a de 2010)
top_exp10 <- exp_df %>% mutate(exp_cop = (absat549 - absat540)) %>%
  select(ind, exp_cop) %>% arrange(desc(exp_cop)) %>% head(20) %>% 
  include_names()
top_exp10
  # ao interpretar de forma comparativa, lembrar que os 10 não incluem exp adquirida
  # em 2019! isto é, tem uma conf a menos de experiencia potencial

######### EXPLORANDO EXPERIENCIA DE FORMA AGREGADA ######################
# A ideia aqui é ver como a experiência média dos representantes de uma organização
# muda ao longo do tempo. Ideia é fazer com MRE, MMA, MCT, etc
# Não-govs também é interessante, mas média talvez distorceria demais
# (n de organizações que entram e saem sem continuidade é grande, puxaria pro zero)

# mudando formato da base ---------
# dados como organização variam no tempo, o que pede o formato ind-event como row
exp_dflong <- pivot_longer(exp_df, cols = starts_with("absat"), names_to = "id_evento",
                       names_prefix = "absat", values_to = "exp") %>% 
  mutate(id_evento = as.integer(id_evento))

#pegando os ids necessarios pra integrar bases ------
df_details <- left_join(delegs, individuos) %>% left_join(eventos) %>% glimpse %>% 
  select(id_indevento, id_individuo, id_evento, id_org_dupla, cargo_org)
#incluindo os em exp_dflong
exp_dflong <- exp_dflong %>% rename(id_individuo = ind) %>% left_join(df_details)
# temos mtos rows com NA, que correspondem aos eventos nos quais indivíduo não participou

# incluindo dados de organização --------
# temos aqui o id_org_dupla. precisamos de id_org_unica.
# primeiro passo é importar ORGs
orgs <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/orgs-2023-07-25.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(org_sujo = col_skip(), 
                                                                        org_detalhe_sujo = col_skip(), org_detalhe_limpo = col_skip()), 
                   locale = locale(encoding = "UTF-8"), 
                   trim_ws = TRUE)

class <- read_delim("~/Doutorado/controle_doc/BD_deleg-evento/class-2022-06-29.csv", 
                    delim = ";", escape_double = FALSE, col_types = cols(org_limpo = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE)

chave_orgs <- left_join(select(delegs, c(id_indevento, id_org_dupla)),
                        orgs
) %>% left_join(class) %>% distinct()

# agora temos que incluir esses dados na planilha exp_dflong
exp_dflong <- left_join(exp_dflong, select(chave_orgs, -tipo_org))


# Calculando a experiência média de determinadas organizações no tempo ----------
# Primeiro passo é filtrar as organizações desejadas.
# Em princípio, vou usar só governo federal

# Nota: aqui temos as orgs mais quebradas do que o ideal (e.g. Gylvan é AEB, não MCT)
# Pra melhorar os dados, teria que juntar essas orgs primeiro.

average_exp_byorg <- 
  exp_dflong %>% filter(str_detect(tipo_org_reduzido, "Governo federal")) %>% 
  # select(org_limpo, id_individuo, id_evento, exp) %>% 
  group_by(id_evento, org_limpo) %>% 
  summarise(average_exp = mean(exp))
  # Não esquecer de checar depois se o agrupamento está saindo correto!!


# Tabulando o tipo de cargo enviado por cada organização às COPs ----------

# Com essa mesma estrutura de dados, podemos também tabular o número de indivíduos
# em cada posto do MRE (ou outro ministério) por evento
# Atenção: só conta os cargos presentes na lista.
# CHECAR: como está lidando com os Sem informação (NA)?
# MRE é o id_org_unica == 435

# evidentemente, antes de calcular a tabela aqui é necessário harmonizar os nomes
# de cargos, o que não foi feito. isso faz com que a tabela não sirva pra muito.
cargosMRE_byevent <-
  exp_dflong %>% filter(id_org_unica==435) %>% 
  group_by(id_evento, cargo_org) %>% 
  summarise(Freq = n())

# uma vez harmonizado, é só dar um pivot wider pra retomar o formato cargo | cop1 | cop2 etc
# nao esquecer de incluir os zeros quando for abrir a tabela dessa maneira
# é possível também editar pra incluir percentuais