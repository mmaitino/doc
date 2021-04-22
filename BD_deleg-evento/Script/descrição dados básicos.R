library(tidyverse)
library(here)

# Leitura das bases ---------
deleg <- read_delim("deleg-2021-04-21.csv", 
                    ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), 
                                                                 X1_1 = col_skip()), locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE)

orgs <- read_delim("orgs-2021-04-21.csv", 
                   ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                   locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE) %>% distinct()

class <- read_delim("class-2021-04-21.csv", 
                    ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE) %>% distinct()

eventos <- read_delim("eventos_v2.csv", 
                      ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                      trim_ws = TRUE)

# Criar deleg_completo (deleg+orgs+class) --------
orgs_classificado <- left_join(orgs, class) %>% select(-c(org_sujo, org_detalhe_sujo))
# N de rows aumenta, porque tem orgs que ficaram apenas na class e saíram da lista orgs (foram erros na padronização e corrigidos posteriormente)
# Teste (resultado deve ser tibble vazio): left_join(orgs, class, by = "id_org_unica") %>% filter(org_limpo.x != org_limpo.y)

deleg_completo <- deleg %>% select(-c(org, org_detalhe)) %>% left_join(orgs_classificado, by = "id_org_dupla")

# Limpeza da deleg_completo
deleg_completo <- deleg_completo %>% mutate(across(where(is.character), str_trim))


# Limpeza da base dos eventos --------
library(lubridate)
### Limpar e renomear colunas
eventos <- eventos %>% select(-c(`Código ONU`, Comentário, `Formato lista`, 
                                 `Aberto?`, `Questões a atentar`, 
                                 Corrigendum, Local)) %>% #retira colunas irrelevantes
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
  )

eventos <- eventos %>% mutate(
  data = if_else(str_count(data)==4, #se falta o mês (só ano)
                 paste0(data, "-01"), #padroniza como janeiro
                 data)) %>% 
  mutate(
    data = if_else(is.na(data)== F,
                   paste0(data, "-01"), #padroniza data no dia 1 do mês
                   data)
  ) %>% mutate(data = ymd(data), ano = year(ymd(data))) 

eventos <- eventos %>% mutate(across(where(is.character), str_trim))

# Descrição da base dos eventos -----------
library(ggplot2)
###.... Linha: evolução eventos no tempo, dividido total e coletados ----
freqeventos_ano <- eventos %>% group_by(ano) %>% summarise(total = n())
freqeventos_ano_coletados <- eventos %>% filter(coleta == "Sim") %>% 
  group_by(ano) %>% summarise(coletados = n())
freqeventos_ano <- left_join(freqeventos_ano, freqeventos_ano_coletados) %>% 
  pivot_longer(cols = c(total, coletados), names_to = "Legenda")

ggplot(freqeventos_ano, aes(x=ano, y = value, group = Legenda)) +
  scale_color_manual(values = c("palegreen4", "palegreen3")) +
  scale_fill_manual(values = c("palegreen4", "palegreen3")) +
  geom_area(position='identity', aes(fill=Legenda), alpha=0.3) +
  geom_line(aes(color=Legenda)) +
  labs(title = "Número de eventos por ano de realização") +
  ylim(0,18) +
  scale_x_continuous(NULL, n.breaks = 10) +
  scale_y_continuous(NULL, n.breaks = 6) +
  theme(plot.title = element_text(size=22), 
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        legend.position="bottom")
  
###.... Colunas empilhadas: número de eventos coletados/total, divididos por temas
freq_tema <- eventos %>% group_by(tema, coleta) %>% summarise(total = n()) %>%
  filter(is.na(coleta)== F) %>% #ignorando os que não tentei coletar ainda 
  mutate(coleta = if_else(coleta == "Senha",
                          "Travado por senha",
                          coleta))

    # p/ mudar ordem da legenda e do gráfico, acho que tem que mudar ordem dos factors antes

ggplot(freq_tema, aes(x = tema, y = total, fill = coleta, label = total)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("indianred1", "dodgerblue3", "snow4")) +
  labs(title = "Resultados da coleta de eventos",
       subtitle = "Dados separados por tema do evento") +
  coord_flip() +
  theme(plot.title = element_text(size=22), 
        legend.text = element_text(size=10),
        legend.title = element_text(size =12),
        legend.position="bottom",
        axis.text.x = element_text(size = 7)) +
  scale_x_discrete(NULL) + scale_y_continuous("Contagem de eventos")


# Descrição das delegações ---------
deleg_evento <- left_join(deleg_completo,
                          select(eventos, c(conf, tema, ano, tipo_evento, infMEA_list)))
    #Nota: do jeito que estou organizando os arquivos, as conferências processadas
    #para as quais o BRA não enviou participantes desaparecem! 
# >>>>>>>>>>>> Valeria pensar como solucionar o problema no futuro <<<<<<<<<<<<<<<<<<

resumo_deleg_size <- deleg_evento %>% group_by(conf, ano) %>% summarize(count = n())

