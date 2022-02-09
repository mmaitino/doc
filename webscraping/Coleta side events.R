library(tidyverse)
library(RSelenium)
library(rvest) #latest rvest version (1.0.2, 2021) w/ improved functions

# Simular navegador ----------
driver <- rsDriver(browser = "firefox", port = 4745L) #abre browser client
remdr <- driver[["client"]] # atribui cliente a um objeto
remdr$open() #abre a conexão

url_geral <- "https://seors.unfccc.int/applications/seors/reports/archive.html"
tabela_geral <- tibble()

##### Loop entre os vários eventos ----------------------
for(i in 2:50){ # n de eventos identificado adiante, p/ simplicidade input manual
  #lista_opcoes[[1]] é o "Please select", loop deve inicar no 2
  print(i)
  #.. Abrir a url geral do arquivo de side events -----------
  remdr$navigate(url_geral)
  #.. Clicar na barra dropdown e selecionar evento --------
  # identifica os elementos
  barra <- remdr$findElement(using = "name", value = "session_id") 
  lista_opcoes <- barra$findChildElements(using = "tag name", value = "option")
  botao_refresh <- remdr$findElement(using = "css selector", 
                                     value = ".mT > form:nth-child(1) > p:nth-child(2) > input:nth-child(2)")
  
  # clica no elemento e escolhe o evento
    barra$clickElement() # clique apenas para imitar navegação comum
  event_name <- lista_opcoes[[i]]$getElementText() %>% unlist() #salva nome evento
  lista_opcoes[[i]]$clickElement() #clica na opção
  botao_refresh$clickElement() #clica no botao
  
  # Coletar página html
  Sys.sleep(runif(1, min=2, max=5)) # esperar carregar
  pagina <- remdr$getPageSource()[[1]]
  
  #..Identificar tabelas de interesse ------------
  tabela1 <- read_html(pagina) %>% html_nodes(".mT > table:nth-child(5)")
  tabela2 <- read_html(pagina) %>% html_nodes(".mT > table:nth-child(9)")
    #confirmar replicabilidade em outras estruturas de página
  
  # Montar a tabela
  if(!is_empty(tabela1)){#evitar erros caso tabela 1 nao seja identificada
  lista_tabela1 <- tabela1 %>% html_table() 
  
  tabela_evento <- tibble(description = lista_tabela1[[1]]$`Title/theme/speakers`,
                          organizer = lista_tabela1[[1]]$Organizer,
                          date = lista_tabela1[[1]]$Date,
                          table_source = "Side events schedule"
  )
  }else{
    tabela_evento <- tibble(description = NA, organizer=NA, date = NA,
                            table_source = "Side events schedule")
  }
  # Se existir segunda tabela, montar o df da mesma
  if(!is_empty(tabela2)){
    lista_tabela2 <- tabela2 %>% html_table()
    tabela_exhibit <- tibble(description = lista_tabela2[[1]]$`Theme`,
                             organizer = lista_tabela2[[1]]$Organizer,
                             date = NA,
                             table_source = "Exhibits list"
    )
    tabela_evento <- rbind(tabela_evento, tabela_exhibit)
  }
  
  # anexos são as vezes lidos como description. retira linhas de anexo
  tabela_evento <- tabela_evento %>%
    filter(str_detect(description, "\\[\\d+?")==F)
  
  tabela_evento$data_coleta <- Sys.time() #adiciona hora coleta
  tabela_evento$event_name <- event_name #adiciona nome evento,
 
  # adiciona ao df geral
  tabela_geral <- rbind(tabela_geral, tabela_evento)
  
  print(paste(event_name, "coletado"))
  # Sleep para evitar problemas de bloqueio do scraper
  Sys.sleep(runif(1, min=8, max=15)) # deve dar uns 10min o loop total
  
  # Repetir processo para próximo evento no loop
}

# fechar navegador e conexão; matar processo interno do java pra liberar porta
remdr$close()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)


# Nota: html_table retira quebras de linha, negrito, links, etc.
# Se precisar de inline tags, é preciso implementar de outra forma

# e.g., substituir tag por um marcador no texto do html original
# pg <- read_html(pagina)
# xml2::xml_find_all(pg, ".//br") %>% xml2::xml_add_sibling("p", "\n") #<br> --> \n
# xml2::xml_find_all(pg, ".//br") %>% xml2::xml_remove()
# tabela1 <- rvest::html_nodes(pg, ".mT > table:nth-child(5)")
# agora a coleta do html_text (dento do html_table) vai pegar o \n


# outro exemplo, editando a função
# https://stackoverflow.com/questions/42119851/how-to-get-the-link-inside-html-table-using-rvest

# Exportar tabela com side events coletados
write.csv2(tabela_geral,
           paste0("side_events_raw","-",Sys.Date(),".csv"), row.names = F)
