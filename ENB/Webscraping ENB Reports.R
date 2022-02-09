library(tidyverse)
library(rvest)

# Função coleta link relatórios ------

listar_relatorios <- function(url){
  # Coletando nome do evento 
  title <- read_html(url) %>% html_elements(css = "h1.c-banner__title") %>% 
    html_text2()
  
  # Coletando os links dos relatórios
  reports_list <- tibble()
  list_reports <- read_html(url) %>% 
    html_elements(css = "#acc-content-by-type-report") %>% #lista reports
    html_children()
  
  for (i in seq_along(list_reports)) {
    role = list_reports[[i]] %>% html_attr("role")
    link = list_reports[[i]] %>% html_attr("about")
    reports_list <- bind_rows(reports_list, tibble(role, link))
  }
  Sys.sleep(abs(rnorm(1,3,0.5)))
  reports_list$url <- url
  reports_list$event <- title
  reports_list

}

# Função filtrar relatórios relevantes -----
# (nos interessam os diários, não gerais, como curtain raiser ou summary)
filtrar_relatorios <- function(reports_list){
  lista_filtrada <- reports_list %>% 
    filter(str_detect(link, "daily-report")|
             str_detect(link, "main-proceedings")
    )
  lista_filtrada
}

# Função raspagem página de relatório ------
raspar_relatorio <- function(report_url){
  report_page <- read_html(report_url)
  
  report_title <- html_elements(report_page, css = "h1.c-node__title") %>% 
    html_text2()
  report_subtitle <- html_element(report_page, css = "h3.c-node__subtitle") %>% 
    html_text2()
  
  report_text_html <- html_element(report_page, css = "div.c-node__body.o-content-from-editor")
  report_text_noformat <- html_text(report_text_html) # essa solução não distingue headers no texto, só quebra de §
  
  # se aparecer vantagem/demanda por bold, header e afins, depois eu reconstruo
  # seria necessário pegar as tags separadas e depois reconstituir o texto
  # (e.g. colando um <b> </b> no bold; separando o texto por header como rows distintas em tibble)
  # outra possibilidade seria pegar com as tags gerais do html 
  # e depois apagar manualmente o que julgar útil apagar
  # aparentemente isso se faz com XML::toString.XMLNode()
  
  # A página inclui tags de participantes e de tema. Não vou separar por ora.
  tag_list <- html_elements(report_page, css = "dd.c-tags__item") %>% 
    html_text2()
  
  report_data <- tibble(title = report_title,
                        event = report_subtitle,
                        link = report_url,
                        text = report_text_noformat,
                        tags = paste(tag_list, collapse = ";"),
                        time_scrape = Sys.time())
  Sys.sleep(abs(rnorm(1,5,1.2)))
  report_data
}

# Função raspagem lista -------
raspar_lista <- function(i, report_list){
  report_url <- paste0(enb_url, report_list$link[i])
  report_data <- raspar_relatorio(report_url)
}

# Execução do código -------------
search_result <- "https://enb.iisd.org/search?search_api_fulltext=unfccc&f%5B0%5D=content_type%3Aevent&page=0"
url <- "https://enb.iisd.org/events/unfccc-cop-1"

enb_url <- "https://enb.iisd.org"
unfccc_urls <- read_csv("UNFCCC-ENBurls.csv")

# Loop para coleta dos reports
# [se for de interesse rodar com outros temas, pode funcionalizar depois]
all_reports <- tibble()

for(i in 1:nrow(unfccc_urls)){
  print(i)
  url_evento <- unfccc_urls[i,]$url
  data_evento <- unfccc_urls[i,]$`Event date`
  nome_evento <- unfccc_urls[i,]$`Event name`
  print(nome_evento)
  
  report_list  <- listar_relatorios(url_evento) %>% filtrar_relatorios()
  
  if(is.data.frame(report_list)==F | nrow(report_list)==0){#se nao houver relatórios além do resumo, registrar
    df_reports <- tibble(title = NA,
                          event = NA,
                          link = NA,
                          text = NA,
                          tags = NA,
                          time_scrape = Sys.time())
    
  }else{#se houver relatórios, raspar
    df_reports <- lapply(1:nrow(report_list), raspar_lista, report_list) %>% 
      bind_rows()
  }
  df_reports <- df_reports %>% mutate(event_name = nome_evento,
                                      event_url = url_evento,
                                      event_date = data_evento)
  
  all_reports <- bind_rows(all_reports, df_reports)
  Sys.sleep(abs(rnorm(1, 6, 1.8)))
}

write_csv2(all_reports, "UNFCCC_reports.csv")

