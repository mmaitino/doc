library(tidyverse)
library(stringr)

# Funções (meio mambembes) para string -> datetime -------------
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
# Abrindo o banco de relatórios --------------
reports <- read_delim("UNFCCC_reports.csv",
                      delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

# Organizando datas ------------------------
reports <- reports %>% filter(is.na(title)==F) %>% 
  mutate(#data do evento relatado
    negotiation_date = datefy(title)
  ) %>% 
  mutate(
    event_date = case_when(str_detect(event_date,"-") == TRUE ~ event_date,
              str_detect(event_date,"-") == FALSE ~ 
                paste0(event_date, " - ", event_date)
                )
  ) %>% 
  separate(event_date, into = c("conf_startdate", "conf_enddate"),
           sep = " - ") %>% 
  mutate(conf_startdate = datefy(conf_startdate),
         conf_enddate = datefy(conf_enddate))

# criar id único para cada report
reports <- mutate(reports, report_id = row_number())

# Adaptando lista castro (países e coalizões) ------
country_list <- read_csv2("castro_countrylist.csv", col_select = 2) %>% 
  rename(country = x)

# Necessário ajustar alguns dos nomes, pois nao aparecem assim no texto
# e podem dar matches errados (quando um está incluído em outro, 
# como é o caso de R of Congo e DRC ou de G77/China e China).

country_adjustments <- tibble(
  country = c("Central Group", "Central Group Eleven",
              "Congo, Democratic Rep.", "Congo, Republic",
              "China","G77",
              "Czech Republic",
              "Cote d'Ivoire",
              "Guinea",
              "Guinea-Bissau",
              "Korea, Republic","Korea, Democratic Rep.",
              "Kyrgyz Republic","Lao PDR",
              "Slovak Republic",
              "Swaziland", "Timor-Leste",
              "Syrian Arab Republic",
              "St. Lucia", "St. Vincent and the Grenadines"
              
  ),
  regex = c("Central Group(?! Eleven)", "Central Group Eleven",
               "Democratic Republic of Congo", "(?<!Democratic )Republic of Congo",
               "(?<!G77 and )(?<!G-77\\/)China","G-?77((\\/| and )China)?",
               "Czech Republic|Czechia",
               "C(o|ô)te d'Ivoire", 
               "(?<! New )(?<!Equatorial )Guinea(?! Bissau)(?!-Bissau)",
               "Guinea(-| )Bissau",
               "(?<!People's )(?<!Peoples )Republic of Korea", "Democratic People'?s Republic of Korea",
               "Kyrgyz Republic|Kyrgistan", "Lao(s| People| PDR)",
               "Slovakia|Slovak Republic",
               "Swaziland|Eswatini", "Timor(-| )Leste",
               "Syria(|n Arab)",
               "Saint Lucia", "Saint Vincent and the Grenadines"
               
  )
)

# alguns países devem ser adicionados ao invés de modificados
new_countries <- tibble(
  country = c("Serbia", "Montenegro", "African Union"),
  regex = c("Serbia(?! and Mon)","(?<!bia and )Montenegro", "African Union")
)

# unindo tudo na lista de países
country_list <- left_join(country_list, country_adjustments) %>% mutate(
  regex = if_else(is.na(regex), country, regex)
) %>% rbind(new_countries) %>% mutate(regex = toupper(regex)) %>% 
  mutate(regex = paste0("(\\b|\\B)", regex, "(\\b|\\B)")) 
      #garantir que não está no meio de palavra (boundaries word e n-word)


# Contabilizando o número de menções a cada entidade de country_list -----
# NÃO ESQUECER DE RETIRAR WHITESPACE E QUEBRA DE LINHA ANTES DE RODAR
# Além disso, toupper tanto no texto do ENB como no regex.

#report <- reports[2,]

count_countries <- function(i){
  report <- reports[i,]
  text <- report$text %>% toupper() %>% str_trim() %>% str_squish()
  col_name <- paste0("rep", report$report_id)
  
  # primeiro report como coluna e países como linha
  country_count <- country_list %>% 
    mutate(count = str_count(text, regex))
  
  # girando o df para ter report como row e países como coluna
  country_count <- country_count %>% select(-regex) %>% 
    add_column(report_id = report$report_id, .before = 1) %>% 
    pivot_wider(names_from = country, values_from = count)
  
  country_count
}

# Agora rodar country_count para cada report ----
counted_reports <- map_df(1:nrow(reports), count_countries)

# Salvando dataframe ----
write.csv2(counted_reports, "counted_reports.csv")




# Testando a medida de contagem vs trabalho de Castro codificando ENB ----

# Em um primeiro momento, fiz um teste simples para Brasil.
# Agora, quero ver se a medida é equivalente (ou pelo menos parecida)

# Para isso, primeiro passo é identificar os report_id equivalentes
# aos eventos analisados por Castro

# Em seguida, selecionar esses ids e abrir o banco de Castro
# Calcular total intervenções por evento / total menções por evento
  # Verificar diferenças entre bases
# Calcular valores relativos. Comparar maiores em absoluto e relativo
  # Verificar diferenças entre bases
# Observar tendência no tempo de Brasil em menções e intervenções
  # Verificar diferenças entre bases
# Calcular diferença entre menções e intervenções e verificar maiores difs

# As diferenças são grandes? Há países/reports muito divergentes? Pq?




# Teste contagem menções/intervenções brasil ------
#df_reports %>% 
  #mutate(br_count = str_count(text, "Brazil|BRAZIL")) %>% View()
# checando aqui, num geral parece razoável só contar.
# o ideal seria fazer algo um pouco mais complexo, tipo pegar quando é sujeito.
# a conexão com a base de Castro é problemática na medida que Castro coloca
# a data de publicação do ENB (e o seu número) como ids.
# A data que eu tenho é a do DIA DE NEGOCIAÇÃO, que é em geral um dia antes.
# (cabe observar se isso vai diferir muito em algum caso, e.g. fds)
# Esses dados eu só teria se o scrape fosse do PDF, me parece desnecessário
# fazer o scrape mais complexo só pra isso.




# Anotações dos países ---------------
# ainda incerto se devo modificar: LDCs, US, EU, 


# Central Group #garantir que não conta duas vezes
# Central Group Eleven
# 
# Congo, Democratic Rep.  #garantir que não conta duas vezes
    # DEMOCRATIC REPUBLIC OF THE CONGO
# Congo, Republic
    # REPUBLIC OF CONGO

# Czech Republic #ver se muda nome (nao achei menções recentes)

# EU # ver se aparece por extenso tb
# G77 #+CHINA, ver como escrevem
    # G-77/CHINA
    # G77 and China
# 
# Guinea  #garantir que não conta várias vezes
# EQUATORIAL GUINEA
# Guinea-Bissau
# 
# Korea, Republic
    # REPUBLIC OF KOREA
# Korea, Democratic Rep.
    # DEMOCRATIC PEOPLE'S REPUBLIC OF KOREA
# 
# 
# Kyrgyz Republic #ver se muda nome
    # KYRGYZSTAN 

# Lao PDR 
    # LAOS, ver se tem outras grafias tb
    # LAO PEOPLE’S DEMOCRATIC REPUBLIC (PDR)

# Macedonia #ver se muda nome
    # FORMER YUGOSLAV REPUBLIC OF MACEDONIA
    #
# 
# Slovak Republic #ver se muda nome
    # SLOVAKIA 

# Swaziland ##ver se muda nome
    # SWAZILAND
    # ESWATINI

# Timor-Leste é sem o traço
# Syrian Arab Republic = Syria