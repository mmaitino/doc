library(tidyverse)
library(here)
library(lubridate)

# Leitura das bases ---------
deleg <- read_delim("deleg-2021-07-22.csv", 
                    ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), 
                                                                 X1_1 = col_skip()), locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE)

orgs <- read_delim("orgs-2021-08-26.csv", 
                   ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                   locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE) %>% distinct()

class <- read_delim("class-2021-08-26.csv", 
                    ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE) %>% distinct()

eventos <- read_delim(here("Historico", "eventos_v3.csv"), 
                      ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                      trim_ws = TRUE)

# Nota: ainda falta incorporar a essa rotina os códigos de indivíduo
# e, muito mais importantes, os dados de eventos com deleg_size = 0 (BR não envia)


# Criar deleg_completo (deleg+orgs+class) --------
orgs_classificado <- left_join(orgs, class) %>% select(-c(org_sujo, org_detalhe_sujo))
# N de rows aumenta, porque tem orgs que ficaram apenas na class e saíram da lista orgs (foram erros na padronização e corrigidos posteriormente)
# Teste (resultado deve ser tibble vazio): left_join(orgs, class, by = "id_org_unica") %>% filter(org_limpo.x != org_limpo.y)

deleg_completo <- deleg %>% select(-c(org, org_detalhe)) %>% left_join(orgs_classificado, by = "id_org_dupla")

# Limpeza da deleg_completo
deleg_completo <- deleg_completo %>% mutate(across(where(is.character), str_trim))

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

eventos$data_orig <- eventos$data
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


# Construção dos dados resumidos a nível Evento
deleg_evento <- left_join(deleg_completo,
                          select(eventos, c(conf, tema, ano, tipo_evento, infMEA_list)))

resumo_deleg_evento <- deleg_evento %>% group_by(conf, ano, tipo_org_reduzido) %>% 
  summarise(count = n()) %>% 
  mutate(deleg_size = sum(count))

# reorganização do formato da base (long -> wide)
# ao invés de conf-tipo como unidade, só conf unidade e quebra em mais colunas
resumo_deleg_evento <- resumo_deleg_evento %>% 
  mutate(tipo_org_reduzido = case_when(
    tipo_org_reduzido == "Governo federal MRE" ~ "MRE",
    tipo_org_reduzido =="Governo federal não-MRE" ~ "GovFed",
    tipo_org_reduzido =="Não identificado" ~ "SemId",
    tipo_org_reduzido == "Governos subnacionais (Executivo, Legislativo, Empresas Públicas ou Autarquias)" ~ "Subnac",
    tipo_org_reduzido == "Legislativo federal" ~ "Leg",
    tipo_org_reduzido == "Órgãos de ensino e pesquisa" ~ "Academia",
    tipo_org_reduzido == "Sociedade civil, sindicatos, movimentos sociais" ~ "SocCivil",
    tipo_org_reduzido == "Setor empresarial" ~ "Empresa",
    tipo_org_reduzido == "Outro" ~ "Outro"
  )
  ) %>% pivot_wider(names_from = tipo_org_reduzido, values_from = count) %>% 
  mutate_all(~replace_na(., 0)) # trocar os NA de todas as colunas por 0

# reincluir data original, local e tema nos dados
deleg_eventos <- left_join(resumo_deleg_evento, 
                           select(eventos, c(conf, conference, tema, 
                                             data_orig, location)))

# salvar como versão atual da base consolidada
save_updated <- function(nome_base){
  write.csv2(get(nome_base, globalenv()),
             paste0(nome_base,"-",Sys.Date(),".csv"), row.names = F
  )
}

save_updated("deleg_eventos")
