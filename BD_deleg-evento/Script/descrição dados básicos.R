library(tidyverse)
library(here)
library(lubridate)
library(ggplot2)
# Leitura das bases ---------
deleg <- read_delim("deleg-2021-07-22.csv", 
                    ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), 
                                                                 X1_1 = col_skip()), locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE)

orgs <- read_delim("orgs-2021-07-22.csv", 
                   ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                   locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE) %>% distinct()

class <- read_delim("class-2021-07-22.csv", 
                    ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE) %>% distinct()

eventos <- read_delim("eventos_v3.csv", 
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
# retirar ano >2020, < 1970
eventos <- eventos %>% filter(ano >= 1970 & ano <= 2020)

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
  scale_x_continuous(NULL, n.breaks = 10) +
  scale_y_continuous(NULL, limits = c(0, 22)) +
  theme(plot.title = element_text(size=22), 
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        legend.position="bottom")

ggsave(paste0("Coleta","-",Sys.Date(),".png"),
       width = 9, height = 4)

# Filtro das principais conferências
eventos %>% filter(`Principais convenções + gdes conf` == "Sim") -> freq_principais
freq_princ_ano <- freq_principais %>% group_by(ano) %>% summarise(total = n()) 
freq_princ_col <- freq_principais %>% filter(coleta == "Sim") %>% 
  group_by(ano) %>% summarise(coletados = n())
freq_princ_ano <- left_join(freq_princ_ano, freq_princ_col) %>% 
  pivot_longer(cols = c(total, coletados), names_to = "Legenda")

ggplot(freq_princ_ano, aes(x=ano, y = value, group = Legenda)) +
  scale_color_manual(values = c("palegreen4", "palegreen3")) +
  scale_fill_manual(values = c("palegreen4", "palegreen3")) +
  geom_area(position='identity', aes(fill=Legenda), alpha=0.3) +
  geom_line(aes(color=Legenda)) +
  labs(title = "Número de eventos por ano de realização",
       subtitle = "Principais convenções multilaterais e grandes conferências ONU") +
  scale_x_continuous(NULL, n.breaks = 10) +
  scale_y_continuous(NULL, limits = c(0, 22)) +
  theme(plot.title = element_text(size=22), 
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        legend.position="bottom")

ggsave(paste0("Coleta principais","-",Sys.Date(),".png"),
       width = 9, height = 4)
  
###.... Colunas empilhadas: número de eventos coletados/total, divididos por temas ----
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

ggsave(paste0("Coleta tema","-",Sys.Date(),".png"),
       width = 15, height = 7)

###.... Colunas empilhadas: número de eventos coletados/total, divididos por tipo ----
freq_tipo <- eventos %>% group_by(tipo_evento, coleta) %>% summarise(total = n()) %>%
  filter(is.na(coleta)== F) %>% #ignorando os que não tentei coletar ainda 
  mutate(coleta = if_else(coleta == "Senha",
                          "Travado por senha",
                          coleta)) %>% 
  mutate(tipo_evento = if_else(is.na(tipo_evento), "A ser classificado",
                                tipo_evento))

ggplot(freq_tipo, aes(x = tipo_evento, y = total, 
                      fill = coleta, label = total)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("indianred1", "dodgerblue3", "snow4")) +
  labs(title = "Resultados da coleta de eventos",
       subtitle = "Dados separados por tipo de evento") +
  coord_flip() +
  theme(plot.title = element_text(size=22), 
        legend.text = element_text(size=10),
        legend.title = element_text(size =12),
        legend.position="bottom",
        axis.text.x = element_text(size = 7)) +
  scale_x_discrete(NULL) + scale_y_continuous("Contagem de eventos")

ggsave(paste0("Coleta tipo","-",Sys.Date(),".png"),
       width = 12, height = 4.5)

# Descrição tamanho das delegações ---------
deleg_evento <- left_join(deleg_completo,
                          select(eventos, c(conf, tema, ano, tipo_evento, infMEA_list)))
    #Nota: do jeito que estou organizando os arquivos, as conferências processadas
    #para as quais o BRA não enviou participantes desaparecem! 
# >>>>>>>>>>>> Valeria pensar como solucionar o problema no futuro <<<<<<<<<<<<<<<<<<


###....Dispersão deleg_size ----
resumo_deleg_size <- deleg_evento %>% group_by(conf, ano) %>% summarize(count = n()) %>% left_join(select(eventos, c(conf, location, tema))) %>% 
  mutate(Local = if_else(str_detect(location, "Brazil"), "Brasil", 
                         "Fora do Brasil/Sem info"),
         Tema = case_when (
           tema == "Clima" ~ "Clima",
           #str_detect(tema, "Biodiversidade") ~ "Biodiversidade",
           tema == "Grandes conferências ONU" ~ "Mega conf. ONU",
           tema != "Clima" & tema != "Grandes conferências ONU" ~ "Outros",
           )) %>% mutate(Local = if_else(is.na(Local), "Fora do Brasil/Sem info",
                                         Local)
                         )

# com Rio+20
disp_delegsize <- ggplot(resumo_deleg_size, aes(x = ano, y = count, color = Tema, group = Local)) +
  geom_point(aes(shape = Local)) +
  geom_label(aes(x = 2012, y = 1500, label = "Rio+20"), nudge_x = 2) +
  geom_label(aes(x = 2009, y = 572, label = "COP15"), nudge_x = 2) +
  geom_label(aes(x = 1992, y = 157, label = "ECO-92"), nudge_x = 2) +
  geom_label(aes(x = 2002, y = 294, label = "Rio+10"), nudge_x = 2) +
  scale_size_manual(values = c(3,2)) +
  scale_shape_manual(values = c(17,16)) +
  scale_color_manual(values = c(#"dodgerblue",
                                "tomato1",
                                "olivedrab4", "lightsteelblue4")) + 
  labs(title = "Tamanho da delegação por evento") +
  theme(plot.title = element_text(size=22), 
        legend.text = element_text(size=10),
        legend.title = element_text(size =12),
        legend.position="bottom") +
  scale_y_continuous("Número de participantes registrados", n.breaks = 8) +
  scale_x_continuous(name = NULL, n.breaks = 12)


ggsave(paste0("Disp size"," Rio+20","-",Sys.Date(),".png"),
       disp_delegsize,
       width = 11, height = 5)


# sem Rio +20
disp_delegsize +
  scale_y_continuous("Número de participantes registrados", n.breaks = 8,
                     limits = c(0,600), #cop15
                     # limits = c(0,70) #zoom confs menores
                     )

ggsave(paste0("Disp size"," sem Rio+20","-",Sys.Date(),".png"),
       width = 11, height = 5)

# tipo_evento
deleg_size_bytipo <- deleg_evento %>% group_by(conf, ano) %>% summarize(count = n()) %>%
  left_join(select(eventos, c(conf, tipo_evento))) #%>% 
  # mutate(tipo_evento = if_else(tipo_evento == "Main regular party meetings (COP, MOP, etc)",
  #                              "COP/MOP", "Pré-tratado ou plenipotenciária"))

ggplot(deleg_size_bytipo, aes(x=ano, y = count, color = tipo_evento)) +
  geom_point() +
  geom_label(aes(x = 2012, y = 1500, label = "Rio+20"), nudge_x = 2) +
  geom_label(aes(x = 2009, y = 572, label = "COP15"), nudge_x = 2) +
  geom_label(aes(x = 1992, y = 157, label = "ECO-92"), nudge_x = 2) +
  geom_label(aes(x = 2002, y = 294, label = "Rio+10"), nudge_x = 2) +
  scale_size_manual(values = c(3,2)) +
  scale_shape_manual(values = c(17,16)) +
  scale_color_manual(values = c("tomato1","olivedrab4", "royalblue3")) +
  # scale_color_manual(values = c("gray38", "tomato1")) +
  labs(title = "Tamanho da delegação por evento")+ 
  theme(plot.title = element_text(size=22), 
        legend.text = element_text(size=10),
        legend.title = element_text(size =12),
        legend.position="bottom") +
  scale_y_continuous("Número de participantes registrados", n.breaks = 8) +
  scale_x_continuous(name = NULL, n.breaks = 12) -> disp_delegsize_tipo

ggsave(paste0("Disp size","tipo Rio+20","-",Sys.Date(),".png"),
       disp_delegsize_tipo,
       width = 11, height = 5)


disp_delegsize_tipo +
  scale_y_continuous("Número de participantes registrados", n.breaks = 8,
                     limits = c(0,600) #cop15
                     # limits = c(0,70) #zoom geral
                     )

ggsave(paste0("Disp size","tipo sem Rio+20","-",Sys.Date(),".png"),
       width = 11, height = 5)

###....Média deleg_size ----
resumo_deleg_size %>% group_by(ano) %>% 
  summarise(mean_deleg_size = mean(count),
            median_deleg_size = median(count),
            sd_deleg_size = sd(count),
            n_events = n()
  ) -> stats_deleg_size

ggplot(stats_deleg_size, aes(x=ano, y=mean_deleg_size)) +
  geom_line() + geom_point() +
   geom_label(aes(x = 2012, y = 195, label = "2012 - Rio+20")) +
   geom_label(aes(x = 1992, y = 55, label = "1992 - Rio")) +
  labs(title = "Tamanho médio das delegações brasileiras a conferências ambientais",
       subtitle = "Dados das listas oficiais de participantes emitidas pelos eventos") +
  scale_y_continuous(n.breaks = 8) +
  scale_x_continuous(name = NULL, n.breaks = 12)


ggsave(paste0("Evolução média deleg_size","-",Sys.Date(),".png"),
       width = 11, height = 5)
# Descrição perfil participantes ----

###....Dispersão % MRE ----
MRE_conf <- deleg_evento %>% group_by(conf, ano, tipo_org_reduzido) %>% 
  summarise(count = n()) %>% mutate(percentual = count/sum(count), `Tamanho da delegação` = sum(count)) %>% 
  filter(ano >= 1970 & ano <= 2020) %>% # retirando delegs fora do escopo temporal
  filter(tipo_org_reduzido == "Governo federal MRE") %>% 
  left_join(select(eventos, c(conf, location, tema, tipo_evento))) %>% 
  mutate(Local = if_else(str_detect(location, "Brazil"), "Brasil", "Fora do Brasil"),
         Tema = case_when (
           tema == "Clima" ~ "Clima",
           tema == "Grandes conferências ONU" ~ "Mega conf. ONU",
           #tema == "Florestas" ~ "Florestas",
           str_detect(tema, "Biodiversidade") ~ "Biodiversidade",
           tema != "Clima" & tema != "Grandes conferências ONU" & #tema != "Florestas" ~ "Outros"
             tema != "Biodiversidade" ~ "Outros"
         )) #%>% 
  #mutate(Tema = if_else(Tema == "Florestas", "Outros", Tema))

# Inclui aqui tamanho total da delegação como size do scatterplot
ggplot(MRE_conf, aes(x=ano, y=percentual, color = Tema, group = Local)) +
  geom_point(aes(#shape = Local, 
                 size = `Tamanho da delegação`)) +
  scale_y_continuous(name = "% de representantes do MRE", labels = scales::percent) +
  scale_shape_manual(values = c(17,16)) +
  scale_color_manual(values = c("olivedrab4","tomato1","magenta3","royalblue3")) +
  # scale_color_manual(values = c("tomato1","olivedrab4", "royalblue3")) +
  labs(title = "Perfil das delegações por evento",
       subtitle = "Percentual da delegação composto por pessoas vinculadas ao Itamaraty") +
  scale_x_continuous(name = NULL, n.breaks = 12) +
  scale_size(range = c(1, 5), name="Tamanho da delegação") +
  theme(plot.title = element_text(size=22), 
        legend.text = element_text(size=10),
        legend.title = element_text(size =12),
        legend.position="right") 
  

ggsave(paste0("Dispersão Perc MRE deleg_size","-",Sys.Date(),".png"),
       width = 11, height = 5)

###....Dispersão % MRE por tipo evento ----
MRE_conf %>% mutate(Tema = if_else(Tema == "Clima", "Clima", "Outros"),
                    tipo_evento = if_else(
                      tipo_evento == "Main regular party meetings (COP, MOP, etc)",
                      "COP/MOP", "Negociação ou plenipotenciária")
  
) %>% filter(ano >= 1985) %>% 
ggplot(aes(x=ano, y=percentual, color = tipo_evento)) +
         geom_point(aes(size = `Tamanho da delegação`)) +
         scale_y_continuous(name = "% de representantes do MRE", labels = scales::percent) +
         scale_color_manual(values = c("gray38", "tomato1")) +
         labs(title = "Perfil das delegações por evento",
              subtitle = "Percentual da delegação composto por pessoas vinculadas ao Itamaraty") +
         scale_x_continuous(name = NULL, n.breaks = 12) +
               theme(plot.title = element_text(size=22), 
               legend.text = element_text(size=10),
               legend.title = element_text(size =12),
               legend.position="right") +
           scale_size(range = c(1, 5), name="Tamanho da delegação")


ggsave(paste0("Dispersão Perc MRE deleg_size tipo","-",Sys.Date(),".png"),
       width = 11, height = 5)       


###....Evolução % MRE, MCT, MMA no tempo ----
orgs_princ <- deleg_evento %>% 
  mutate(org = case_when(
    org_limpo == "Ministério das Relações Exteriores" ~ "Ministério das Relações Exteriores",
    tipo_org == "Laboratórios, centros e institutos de pesquisa vinculados ao MCT" ~
      "Ministério da Ciência e Tecnologia",
    id_org_unica %in% c(23,422) ~ "Ministério da Ciência e Tecnologia",
    id_org_unica %in% c(442, 324, 333, 555) ~ "Ministério do Meio Ambiente",
    # id_org_unica %in% c(326, 576, 441) ~ "Ministério do Meio Ambiente",
    id_org_unica %in% c(214, 421) ~ "Ministério da Agricultura, Pecuária e Abastecimento",
  ))
# inclusão de IBAMA, ICMBio, AEB, IBDF (326), SFB (576), M Interior (441)
# laboratórios nacionais e afins

freq_minist_tempo <- orgs_princ %>% 
  group_by(ano, org) %>% 
  summarise(total = n()) %>% mutate(percentual = total / sum(total)) %>% 
  ungroup() %>% 
  filter(org %in% c("Ministério das Relações Exteriores",
                    "Ministério do Meio Ambiente", 
                    "Ministério da Ciência e Tecnologia",
                    "Ministério da Agricultura, Pecuária e Abastecimento"
                    
  )) %>% 
  # incluir contagem de observações nulas (freq = 0)
  complete(ano, org, fill = list(total = 0, percentual = 0)) 

# Gráfico
freq_minist_tempo %>% 
  ggplot(aes(x=ano, y = percentual)) +
  geom_line() + geom_point() +
  lemon::facet_rep_wrap(~org, ncol = 1,
                        repeat.tick.labels = "bottom") +
  labs(title = "Evolução da participação em conferências ambientais", 
       subtitle = "Percentual de participantes registrados no ano vinculados a MRE, MMA, MCT e MAPA.
MMA inclui IBAMA e ICMBio. MCT inclui AEB e laboratórios vinculados. MAPA inclui EMBRAPA.") +
  scale_y_continuous(name = "% dos participantes no ano", labels = scales::percent) +
  scale_x_continuous(name = NULL, n.breaks = 12) +
  theme(plot.title = element_text(size=22))


ggsave(paste0("Parts por org-ano", "-",Sys.Date(),".png"),
       width = 7.5, height = 5.3)

######### MCT e MMA "puros"
# freq_minist_tempo <- deleg_evento %>% 
#   group_by(ano, org_limpo) %>% 
#   summarise(total = n()) %>% mutate(percentual = total / sum(total)) %>% 
#   ungroup() %>% 
#   filter(org_limpo %in% c("Ministério das Relações Exteriores",
#                           "Ministério do Meio Ambiente", 
#                           "Ministério da Ciência e Tecnologia"
#   )) %>% 
#   # incluir contagem de observações nulas (freq = 0)
#   complete(ano, org_limpo, fill = list(total = 0, percentual = 0)) 
# 
# freq_minist_tempo %>% 
#   ggplot(aes(x=ano, y = percentual)) +
#   geom_line() + geom_point() +
#   lemon::facet_rep_wrap(~org_limpo, ncol = 1,
#                         repeat.tick.labels = "bottom") +
#   labs(title = "Evolução da participação em conferências ambientais", 
#        subtitle = "Percentual de participantes registrados no ano vinculados a MRE, MMA, MCT.
# Não são considerados representantes de IBAMA, ICMBio, IBDF, AEB, laboratórios vinculados ao MCT.") +
#   scale_y_continuous(name = "% dos participantes no ano", labels = scales::percent) +
#   scale_x_continuous(name = NULL, n.breaks = 12) +
#   theme(plot.title = element_text(size=22))

#....Evolução dos diferentes tipos de organização ----
freq_orgs_tempo <- deleg_evento %>% 
  # mutate(tipo_org_reduzido = if_else(
  #   tipo_org_reduzido %in% c("Sociedade civil, sindicatos, movimentos sociais", "Setor empresarial"),
  #   "Sociedade civil e empresas", tipo_org_reduzido)) %>%
  group_by(ano, tipo_org_reduzido) %>% 
  summarise(total = n()) %>% mutate(percentual = total / sum(total)) %>% 
  ungroup() %>% # incluir contagem de observações nulas (freq = 0)
  complete(ano, tipo_org_reduzido, fill = list(total = 0, percentual = 0))

freq_orgs_tempo$tipo_org_reduzido[freq_orgs_tempo$tipo_org_reduzido == "Governos subnacionais (Executivo, Legislativo, Empresas Públicas ou Autarquias)"] <- "Governos subnacionais"



freq_orgs_tempo$tipo_factor <- factor(freq_orgs_tempo$tipo_org_reduzido, 
                                      levels = c("Governo federal MRE", "Governo federal não-MRE", 
                                                 "Governos subnacionais", "Legislativo federal",
                                                 "Sociedade civil, sindicatos, movimentos sociais", "Setor empresarial",
                                                 # "Sociedade civil e empresas"
                                                 "Órgãos de ensino e pesquisa", "Outro",
                                                 "Não identificado"
                                                 ))

freq_orgs_tempo %>% 
  ggplot(aes(x=ano, y = percentual)) +
  geom_line() + geom_point() +
  lemon::facet_rep_wrap(~tipo_factor, ncol = 1,
                        repeat.tick.labels = "bottom") +
  labs(title = "Composição das delegações brasileiras a conferências ambientais", 
       subtitle = "por tipo de organização, em percentual dos participantes no ano") +
  scale_y_continuous(name = "% dos participantes no ano", labels = scales::percent) +
  scale_x_continuous(name = NULL, n.breaks = 12) +
  theme(plot.title = element_text(size=18))

ggsave(paste0("Delegações por tipo org", "-",Sys.Date(),".png"),
       width = 6.7, height = 10.2)
