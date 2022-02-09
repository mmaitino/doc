library(tidyverse)

# Função datefy
datefy <- function(string){
  string <- tolower(string)
  
  datefy_month <- function(string){
    # months <- 1:12
    # names(months) <- c("January", "February", "March",
    #             "April", "May", "June",
    #             "July", "August", "September", 
    #             "October", "November", "December")
    case_when(
      str_detect(string, "january") ~ 1,
      str_detect(string, "february") ~ 2,
      str_detect(string, "march") ~ 3,
      str_detect(string, "april") ~ 4,
      str_detect(string, "may") ~ 5,
      str_detect(string, "june") ~ 6,
      str_detect(string, "july") ~ 7,
      str_detect(string, "august") ~ 8,
      str_detect(string, "september") ~ 9,
      str_detect(string, "october") ~ 10,
      str_detect(string, "november") ~ 11,
      str_detect(string, "december") ~ 12
    )
  }
  
  datefy_day <- function(string){
    # regex <- paste("\\d+?[\\sof]?", "November")
    as.numeric(str_extract(string, "\\d+? "))
  }
  
  datefy_year <- function(string){
    # regex <- paste("\\d+?[\\sof]?", "November")
    as.numeric(str_extract(string, "19|20\\d+"))
  }
  
  datefied <- lubridate::ymd(
    paste(datefy_year(string),
          datefy_month(string),
          datefy_day(string),
          sep = "-")
  )
  datefied
}

# Abrindo os arquivos -----
reports <- read_delim("UNFCCC_reports.csv",
                      delim = ";", 
                      escape_double = FALSE, 
                      col_types = cols(text = col_skip()), 
                      trim_ws = TRUE)

# deveria ter salvado id que eu gerei no script passado pra evitar erros

reports <- mutate(reports, report_id = row_number()) %>% 
  filter(is.na(link) == F) # retira os sem report



counted_reports <- read_csv2("counted_reports.csv")

# Organizando os dados sobre os eventos ----
reports <- reports %>% 
  select(-c(link, tags, time_scrape, event, event_url)) %>% 
  mutate(negotiation_day = datefy(title)) %>% # cria data do relatório
  filter(is.na(negotiation_day) == F) # retira erros
  # note-se que filter aqui reflete decisão de ignorar relatórios
  # que não contenham data no título
  # vale a pena revisar dependendo da intenção na pesquisa

# se quiser padronizar depois os nomes, vale notar que as COP
# aparecem de 2006 em diante como 'Climate Change Conference'
# há, ainda, climate change talks e os SB

# Visualizando total de menções e menções ao Brasil no tempo ----
total_mentions <- counted_reports %>% 
  mutate(total = rowSums(across(3:227))) %>% select(report_id, total)

total_mentions <- left_join(reports, total_mentions)

BR_mentions <- counted_reports %>% group_by(report_id) %>% 
  summarise(bra = sum(Brazil))

total_mentions <- left_join(total_mentions, BR_mentions)
total_mentions <- pivot_longer(total_mentions, c(bra, total),
                               names_to = "type")


# Dispersão por dia de negociação -----
# Total de menções
total_mentions %>% 
ggplot(aes(x=negotiation_day, y = value)) +
  geom_point() + facet_wrap(vars(type), ncol = 1)

# menções a brasil
total_mentions %>% filter(type == 'bra') %>% 
  ggplot(aes(x=negotiation_day, y = value)) +
  geom_point() 

# Por ano -----
# Somado
# Média

# Agrupar eventos ----
# Somado
# Média



