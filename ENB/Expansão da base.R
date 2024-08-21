# Construção de BD de interações em negociações a partir de dados do ENB
# O projeto vai se iniciar com uma replicação do banco de dados de Castro (2017)
# Em seguida, após uma comparação básica dos dados obtidos de forma automatizada com os da codificação
# manual de Castro, vamos expandir a base no tempo. Castro cobre 1995-2013. Idealmente, expandiríamos
# para o período atual (2024)

# Pacotes e funções útes iniciais -----
library(tidyverse)

# Função (meio mambembe) para string -> datetime
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
    as.numeric(str_extract(string, "19\\d+|20\\d+"))
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
# A análise parte do banco de relatórios do ENB já coletados (p/ o processo, ver Webscraping ENB Reports)
reports <- read_delim("UNFCCC_reports.csv",
                      delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

# criar id único para cada report
reports <- mutate(reports, report_id = row_number())

# limpeza da base: retirar colunas desnecessárias para a análise
reports <- reports %>% select(-c(tags, time_scrape, event_url))

# Organizando datas ------------------------
reports <- reports %>% filter(is.na(text)==F) %>%  #retira observações sem dados
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


# Filtrando a base ------------------------
# Para agora, vou trabalhar apenas com os dados equivalentes a Castro [1995-2013]. 
# No futuro, período posterior será reincluído.
# Queremos apenas os Daily Reports das negociações. Pelo título dos salvos, 
# todos os relatórios salvos são "Report of main proceedings", então não vamos nos preocuar com isso.
reports <- reports %>% filter(negotiation_date <= "2014-01-01")
# Castro inclui COPs, SBs, Ad-Hoc Negotiation Groups, e "specific technical workshops"
# Por ora, vamos assumir que não é preciso filtrar mais. Se for o caso, retornamos ao tema.



#### 1.LIMPANDO O TEXTO -------------------
# Tudo isso será funcionalizado no futuro, sendo aplicado a cada documento no df reports
# Antes de funcionalizar, porém, vamos testar a análise para um documento específico

# Escolha aleatória de um relatório
# set.seed(156)
# set.seed(854)
analysed_id <- sample(reports$report_id, 1)
#analysed_id <- 263

organize_report <- function(analysed_id, report_df = reports){
  doc_texto <- report_df[report_df$report_id == analysed_id,] %>% pull(text) # objeto string com o texto do relatório
  # Seguindo o procedimento de Castro (2017), devemos limpar as partes do relatório que não oferecem
  # informações referentes à negociação do dia que analisamos.
  
  #...1.1 Retirada de parágrafo introdutório -------
  # Modo geral, os relatórios iniciam-se com um § inicial resumindo o evento
  # Após inspeção dos dados, notei que, em alguns casos mais raros, a quebra de § não está no local correto
  # Parte dos dados acabada cortada. Nesses casos, geralm. há um header com "CAIXA ALTA:" no §.
  # Outros casos não têm § de resumo e já se iniciam com "CAIXA ALTA\n"
  
  # docs testes para os casos desviantes
  # doc_texto_CAIXAALTA <- reports[reports$report_id==545,] %>% pull(text)
  # doc_texto_semquebra <- reports[reports$report_id==273,] %>% pull(text)
  
  # Se levarmos em conta esses casos, parece adequado retirar todos os §§ iniciais dos relatórios.
  # regex_parag1 <- ".+\\n"
  # regex_casos_sem_quebra <- "^(?s).*?(?=[A-Z ]+(:|\\n))" #cobre ambos casos desviantes
  # explicação da regex: 
  # "(?s)" é um modifier do DOTALL mode, que permite que o "." dê match em newline symbol
  # (como o relatorio inicia com um \n antes de entrar com texto, é necessário quando uso "^")
  # "(?=)" é um positive lookahead, pega tudo antes da expressão que se segue
  # "[A-Z ]+(:|\\n)" é uma sequencia de palavras em CAIXA ALTA finalizada em um ":" ou em um \n
  
  
  # Unindo todos os casos, chegamos à seguinte regex:
  regex_completa <- "^(?s)(.*?)(?=(\\n\\n|[A-Z ]+?(:|\\n)))"
  # juntando ambas, temos um positive lookahead que checa se o texto capturado é
  # \\n\\n (a primeira quebra de paragrafo) ou |  "[A-Z ]+(:|\\n)" (o texto em caps seguido de : ou \n)
  # (.*?) Non-greedy capture group to capture any text (including newlines due to DOTALL mode) up until..
  
  # Em seguida, podemos aplicar a regex em questão ao texto do documento
  doc_texto <- doc_texto %>% str_remove(regex_completa)
  
  
  #...1.2  Retirar seções desnecessárias do relatório -------
  
  #.....1.2.1 Separando o texto em seções por header ----------
  # O texto dos relatórios é todo dividido em headers. Uma possibilidade é já quebrar,
  # desde já, o texto em rows distintos para cada header. Pode ser uma vantagem pra não identificar de
  # forma errada alguma interação entre países ocorrendo em frases consecutivas.
  
  # Uma possível limitação, a estudar, é a presença, nos resultados dos headers, de nomes de países
  # É provável que a busca esteja pegando algumas siglas, como a da EU ou de grupos de negociação
  # É bom checar os casos no texto, porque se forem falas dos países (ou objeto de falas) isso pode
  # trazer problemas pra base. Ainda se for, é possível depois juntar de novo no row anterior com
  # um dicionário, mas é preciso saber que é isso que está acontecendo.
  
  # quebrar primeiro nível de header (CAIXA ALTA\\n, sem ":", só quebra de linha)
  doc_headers1 <- str_extract_all(doc_texto, "[A-Z ]+?\\n") %>% unlist
  doc_sectiontexts <- str_split(doc_texto, "[A-Z ]+?\\n") %>% unlist
  
  # seção inicial possivelmente não tem header, logo criamos um título placeholder
  if(length(doc_headers1) == (length(doc_sectiontexts) - 1)){doc_headers1 <- c("First section", doc_headers1)}
  doc_df <- tibble(section_h1 = doc_headers1, text = doc_sectiontexts)
  
  # quebrar 2o nível de header (CAIXA ALTA:)
  doc_df <- doc_df %>% mutate(section_h2 = str_extract_all(doc_df$text, "[A-Z ]+?:"),
                              text = str_split(doc_df$text, "[A-Z ]+?:"))
  
  doc_df <- doc_df %>% # necessario adicionar h2 placeholder
    mutate(ltxt = lengths(text), lh2 = lengths(section_h2)) %>% 
    rowwise() %>% #necessario para trabalhar com col lista
    mutate(section_h2 = if_else(ltxt - lh2 == 1,
                                list(append("First subsection", section_h2)),
                                list(section_h2))
    ) %>% 
    unnest(cols = c(section_h2, text)) %>% # abre as colunas de lista em novos rows
    mutate(section_order = row_number()) %>% # criar id sequencia das seções
    select(-c(ltxt, lh2)) %>% relocate(section_h2, .after = "section_h1") #muda ordem das col
  
  
  #.....1.2.2 Identificar headers de comentários ----------
  
  # Há algumas seções dos relatórios que devem ser removidas por não serem relatórios das negs
  # Castro aponta três headers típicos para esse tipo de seção:
  # “A brief history of the UNFCCC and the Kyoto Protocol” - Histórico e contexto
  # “In the corridors” - comentários informais de especialistas sobre status das negs
  # “A brief analysis of the XXX meeting” - análise e comentários sobre processo e resultados
  
  # Para limpar esses casos, preciso a) identificar se há uma divisória padrão de header (como caixa alta)
  # b) Buscar o texto entre esses headers e o próximo header
  
  # Em uma exploração rápida da base, vi que há várias instâncias do IN THE CORRIDORS, inclusive CAPS\n
  # As demais formulações não são tão comuns. History of the... foi raríssimo, ANALYSIS também poucos (36)
  # Vale explorar um pouco a base mais diretamente, lendo alguns relatórios para checar formulação
  
  # os headers são, por vezes, seguidos de \\n, por vezes seguidos de :. As vezes há espaço antes do \n
  # (se há outras formas de fazer o header, não encontramos)
  regex_header <- "[A-Z ]+?(:|\\n)"
  
  # Analisando os headers mais comuns, podemos identificar quais devemos cortar da análise
  # headers <- reports$text %>% str_extract_all(regex_header)
  # headers <- unlist(headers)
  # freq_headers <- table(headers)
  
  # "IN THE CORRIDORS", "THINGS TO LOOK FOR TODAY" [os mais comuns]
  # "ENB SUMMARY AND ANALYSIS", "IN THE BREEZEWAYS", " IN THE CORRIDORS (I|II)", " THINGS TO LOOK FOR"
  # Checar se é da negociação ou análise: "POTENTIAL CONSEQUENCES", "REPORT OF THE SESSION"
  headers_to_remove <- c("CORRIDORS", "THINGS TO LOOK", "SUMMARY", "BREEZEWAYS")
  
  #.....1.2.3 Filtrar o relatório ----------
  # Além dos comentários, é importante corrigir seções quebradas por erro (headers mal identificados). 
  # Para isso, um caminho é analisar os títulos, alguns são nomes de países.
  # Nchar também pode oferecer caminhos úteis para isso.
  
  # >>>>>>>>>> CHECAR SE ISSO OCORRE EM OUTROS DOCUMENTOS <<<<<<<<<<<<<<
  # Se for o caso, vamos ter que colar o texto (h2 + text) no final do texto da linha anterior
  
  # Corrigir seções vazias
  # Em vários casos, vemos seções vazias. 
  # Como já incluímos os headers em ambos níveis (varios sao por h1 antes de h2), podemos cortar
  doc_df <- doc_df %>% 
    filter(nchar(text) != 0) # retira se texto for vazio
  
  # Retirar seções que não dizem respeito à negociação
  doc_df <- doc_df %>% 
    filter(!str_detect(section_h1, paste0(headers_to_remove, collapse = "|"))) %>% 
    filter(!str_detect(section_h2, paste0(headers_to_remove, collapse = "|")))
  
  
  # Manter o report_id 
  doc_df$report_id <- analysed_id
  
  doc_df
}


  



# 2. INICIANDO A ANÁLISE DAS SEÇÕES ---------------------------------

#...2.1 Questões preliminares -----------------

#.....2.1.1 Lista de países e coalizões ---------
# Parto da lista de Castro (com países e coalizões)
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
              "Niger",
              "Russian Federation",
              "Slovak Republic",
              "Swaziland", "Timor-Leste",
              "Syrian Arab Republic",
              "St. Lucia", "St. Vincent and the Grenadines"
              
  ),
  regex = c("Central Group(?! Eleven)", "Central Group Eleven|CG-11",
            "Democratic Republic of Congo", "(?<!Democratic )Republic of Congo",
            "(?<!G77 and )(?<!G-77\\/)China","G-?77((\\/| and )China)?",
            "Czech Republic|Czechia",
            "C(o|ô)te d'Ivoire", 
            "(?<! New )(?<!Equatorial )Guinea(?! Bissau)(?!-Bissau)",
            "Guinea(-| )Bissau",
            "(?<!People's )(?<!Peoples )Republic of Korea", "Democratic People'?s Republic of Korea",
            "Kyrgyz Republic|Kyrgistan", "Lao(s| People| PDR)",
            "Niger(?!ia)",
            "Russia(n Federation)?",
            "Slovakia|Slovak Republic",
            "Swaziland|Eswatini", "Timor(-| )Leste",
            "Syria(|n Arab)",
            "Saint Lucia", "Saint Vincent and the Grenadines"
            
  )
)

# alguns países devem ser adicionados ao invés de modificados
# sigla US não estava na lista, mas é usada também
new_countries <- tibble(
  country = c("Serbia", "Montenegro", "African Union", "US"),
  regex = c("Serbia(?! and Mon)","(?<!bia and )Montenegro", "African Union", "US")
)



# unindo tudo na lista de países
country_list <- left_join(country_list, country_adjustments) %>% mutate(
  regex = if_else(is.na(regex), country, regex)
) %>% rbind(new_countries) %>% mutate(regex = toupper(regex)) %>% 
  mutate(regex = paste0("(\\b|\\B)", regex, "(\\b|\\B)")) %>% 
  mutate(regex = if_else(country == "US", "\\bUS(?![$\\w])", regex)) %>% 
  mutate(regex = if_else(country %in% c("LDCs", "EITs"), str_replace(regex, "S","[Ss]"), regex))
#garantir que não está no meio de palavra (boundaries word e non-word)
# \b = pattern é seguido de um NON-WORD CHARACTER. \B = pattern é seguido de um word char
# Provavelmente incluí o \B pra dar conta de erros como falta de espaço etc entre palavras
# Isso cria, porém, problema pros casos em que o nome do país está incluído em outro
# (na string NIGERIA, temos match em NIGER\B, por isso a adaptação mais acima 
# o mesmo vale para US como sigla, mas nesse caso vamos só retirar o \B pq há muitos erros
# possíveis. Para US foi adicionado um negative lookahead para eliminar tb "$" (evitar match US$)


coalition_list <- country_list %>% filter(
  country %in% c("AILAC", "ALBA", "AOSIS", "African Group", "Arab Group", 
                 "BASIC", "CACAM", "COMIFAC", "Cartagena Dialogue", "Central America", 
                 "Central Group", "Central Group Eleven", "Coalition of Rainforest Nations",
                 "Congo Basin Countries", "Environmental Integrity Group", "G77", "Group of 9",
                 "JUSCANZ", "LDCs", "Like Minded Developing Countries", 
                 "Mountain Landlocked Developing Countries", "OECD", "OPEC", "SICA", "Umbrella Group", 
                 "Visegrad Group", "Southern Africa Development Community", "Caribbean Community", "EITs")
)


#...2.2 Elimina seções sem interações entre países -----------------

# originalmente, eliminava as seções sem interações entre países primeiro
# com isso, reduz o tamanho da base e, com isso, facilitaria pra rodar
# como método para contagem dos países é bem lento, preferi fazer isso só no nível §

                    
#...2.3 Quebrar os textos por parágrafos -----------------

organize_byparagraph <- function(doc_df){
  # Assumimos que a unidade básica de sentido para a interação vai se dar no nível do §.
  # Vamos, então, quebrar a coluna 'text' em vários rows com col parag
  doc_df <- doc_df %>% 
    mutate(
      parag = str_split(text, "\\n")
    ) %>% unnest(parag) %>% ungroup %>% 
    mutate(parag_order = row_number())
  
  # Seguindo nosso objetivo de análise, filtraremos os §§ sem menção a países
  # (em outro objetivo, poderiam ser mantidas ONGs e outros que também falam, e.g.)
  # Primeira formulação ia só eliminar casos sem nenhuma menção a país.
  # Não sei ainda se vale restringir apenas a país SPEAKER ou se poe qualquer menção
  # Estava usando sem toupper, só com limpeza do texto (str_trim e str_squish)
  # portanto, só considerava se tem um SPEAKER 
  # (ignorando que podem existir outras formulações no texto)
  
  # As menções absolutas são úteis se queremos medida de atividade (quem fala, sobre que, etc)
  # No entanto, como queremos INTERAÇÕES, é preciso que haja ao menos 2 atores.
  # Por isso, estratégia: contar o número de matches do texto (SPEAKER ou não)
  # >>>>>>>>> Estou mantendo apenas §§ com 2+ (lembrando que só 2+ países em CAIXA ALTA)
  
  # A princípio, não parece ter grande perda (ao menos no doc testado, não houve erro)
  # backup <- doc_df
  
  # para acelerar os matches, é interessante substituir países por um placeholder
  doc_df <- doc_df %>% 
    mutate(parag_sub = str_replace_all(parag, #%>% str_trim %>% str_squish,
                                       paste0(country_list$regex, collapse = "|"),
                                       "COUNTRYNAME") #%>% toupper
           )
  
  # Collapse das regex e filter com numero de matches das regex
  doc_df <- doc_df %>%
    mutate(countrymatch_count = str_count(parag_sub, "COUNTRYNAME")) %>%
    filter(countrymatch_count > 1)
  
  doc_df$text <- NULL
  doc_df
}


# Esse texto em nível parágrafo, integrando com os metadados do documento (data, neg, etc)
# já me permitiria testar zero-shot classification e afins.
# Se eu for trabalhar com NLP e modelos mais refinados, nem que seja só POS, exportar
# o fluxo aqui já resolveria.
# doc_df <- doc_df %>% mutate(report_id = analysed_id) #%>% 
  # left_join(reports)
# write.csv(doc_df, "reportparag_test.csv")


#...2.4 Quebrar o texto em nível de frase ----------------

# Como estou montando a versão mais simples do código, uma espécie de proof of concept,
# vou fazer a análise só no nível de frase e apenas interações explícitas
# Se fosse fazer a versao mais complexa, a etapa 3 seria aplicada diretamente ao §
# (com mais complexidades correspondentes, como identificar a qual menção nos referimos)

organize_bysentence <- function(doc_df){
  doc_df <- doc_df %>% 
    mutate(sentence = tokenizers::tokenize_sentences(parag),
           sentence_sub = tokenizers::tokenize_sentences(parag_sub)) %>% 
    unnest(c(sentence, sentence_sub)) %>% ungroup %>% 
    mutate(sent_order = row_number())
  
  # # p/ manter apenas um unnest e evitar erros, refazemos o sentence_sub via sentence
  # doc_df <- doc_df %>% 
  #   mutate(sentence_sub = str_replace_all(sentence %>% str_trim %>% str_squish,
  #                                      paste0(country_list$regex, collapse = "|"),
  #                                      "COUNTRYNAME") %>% toupper
  #   )
  doc_df <- doc_df %>% 
    mutate(sentence_sub = toupper(sentence_sub) %>% str_trim %>% str_squish)
  
  doc_df <- doc_df %>% # manter apenas frases com 2 ou + menções a países
    mutate(countrymatch_count = str_count(sentence_sub, "COUNTRYNAME")) %>%
    filter(countrymatch_count > 1)
  
  doc_df$parag <- NULL
  doc_df$parag_sub <- NULL
  doc_df
}



# 3. IDENTIFICANDO AS RELAÇÕES DE SPEAKER/OBJETO ------------
# A identificação das relações de speaker e objeto é feita da seguinte forma:
# 1) A partir de uma unidade de texto definida (seção, §, frase),
# identificar países mencionados
# 2) Desdobrar em um row por país mencionado
# 3) Manter rows quando o país correspondente é o 'sender' da interação
# 4) Identificar qual ou quais seriam os 'target' da interação
# 5) Identificar tipo de interação

#...3.1 Criar uma lista com países 'agentes' para cada seção -----------------
# no caso, estamos trabalhando a nível de sentence - só trocar o argumento se for parag
identify_speaker <- function(doc_df){
  doc_df <- doc_df %>% mutate(speakerlist = str_extract_all(sentence,
                                                            paste0(country_list$regex, 
                                                                   collapse = "|")))
  
  speaker_df <- doc_df %>% unnest(speakerlist) %>% rename(speaker = speakerlist) %>% ungroup
  speaker_df <- speaker_df %>% 
    mutate(interaction = "", sender = "", target = "")
  speaker_df
}




#......3.1.1 Limpar texto da base: manter apenas o trecho no qual o speaker aparece -------------
# Como pode haver múltiplas menções ao país no texto, é preciso identificar a qual
# trecho se refere essa menção.
# speaker_df <- mutate(speaker_df, rownumber = row_number())
# 
# speaker_df <- speaker_df %>% group_by(section_h1, section_h2, speaker) %>%
#   mutate(nrow_vec = paste(rownumber, collapse = " ")) %>% 
#   rowwise() %>% 
#   mutate(
#     countrymatch_order = match(rownumber, unlist(str_split(nrow_vec, " ")))
#   ) %>% select(-c(rownumber, nrow_vec))
# 
# # extraindo o texto do parágrafo
# # regex_parag <- "(?m)^.*?KEYWORD.*?$"

#   # falta incluir a extração de qual parágrafo eu quero na regex
#   # nao funcionou o § no método com extract_all. [[1]][countrymatch_order]
#   # Problema: pode ter mais de um por §!  #
# 
# speaker_df <- speaker_df %>% 
#   rowwise() %>% 
#   mutate(sent = str_extract_all(text, 
#                                  gsub("KEYWORD", speaker, regex_sentence))
#          )
# 
# speaker_df


# 4. IDENTIFICANDO O TIPO DE INTERAÇÃO -------------------
# 4.0 Discutindo características do problema ---------
# Aqui temos a primeira limitação: vamos tentar usar casos em que há interação em frases distintas?
    # Se for usar, vamos ter que criar um segundo fluxo pra esses casos.
    # Se não, podemos interromper o fluxo aqui e ignorar as falas com essa característica
    # (ou criar algo como 'objeto = NA', permitindo análise das falas totais pela mesma planilha)
  # P/ cada seção do texto, montar um DF com todos os SPEAKERS e os objetos correspondentes

# A identificação de quem é o speaker e quem é o objeto vai depender do tipo de interação
# Para alguns tipos de interação, a relação é bidirecional (mantém ambos como speaker)
# Isso ocorre p/ agreement e with
# Para outros, não. Ordem de apresentação importa, mas também do tipo de relação
# Em "on behalf of", o speaker vem primeiro e o(s) objeto(s) depois
# Em "support" ou "opposition", é o inverso. Em "criticism", aparentemente o speaker é o primeiro.
# Portanto, vamos fazer a identificação conforme o tipo de interação.
# Identificação a princípio vai das mais fáceis para as mais difíceis.
# Depois posso rearranjar se tiver vantagem computacional.

# Pela estrutura dos dados, temos tipo de interação como variável binária
# e sender/target em todos os rows
# speaker_df$sender <- ""
# speaker_df$target <- ""
# Para evitar erros, vou criar colunas lógica/sender/target pra todos os tipos de interação
# depois faço um pivot pra organizar, mas com isso posso checar se estou classificando 2x a mesma frase


# No codebook, 
# SENDER = COUNTRY1 - Country judging the statement by country2
# TARGET = COUNTRY2 - Country whose statement is being judged by country1

# 4.1 "ON BEHALF OF" ------
# Verificar se alguma das seguintes estruturas aparece:
# COSTA RICA, for Chile, Colombia and Peru
# PANAMA, also speaking for Colombia, Chile, Mexico, Guatemala, Peru, Uruguay and the Dominican Republic,
# COLOMBIA, on behalf of Peru and Guatemala
# test_strings <- c("ROMANIA, speaking for the Group of Eastern European countries, and supported by the US, said JI and CDM have distinct roles and should not be treated in the same way.",
#                   "COSTA RICA, for Chile, Colombia and Peru, encouraged the UNFCCC to further engage with other multilateral bodies, namely ICAO and IMO",
#                   "PANAMA, also speaking for Colombia, Chile, Mexico, Guatemala, Peru, Uruguay and the Dominican Republic, stressed the importance of making progress on REDD",
#                   "COLOMBIA, on behalf of Peru and Guatemala, proposed that representatives of the COP work on drafting the arrangements to complete work by COP 19")
# 
# df <- tibble(speaker = c("COLOMBIA", "PERU", "GUATEMALA"),
#              sentence = "COLOMBIA, on behalf of PERU and GUATEMALA, proposed that representatives of the COP work on drafting the arrangements to complete work by COP 19")


# SENDER/COUNTRY1 = Quem fala pelo outro; TARGET/COUNTRY2 = quem é 'representado'
# Não vale para country grouping


# PONTOS A ATENTAR:
# eliminar rows quando o objeto é um country grouping
# importante: não posso manter como objeto de 'on behalf of' um terceiro país da frase (e.g. '1, for 2, xyz, but 3 opposed)


# Teste lógico está pegando países que não foram capturados no SPEAKER (o 1o usou toupper o segundo nao)
# Optei por ignorar esses casos >> solução talvez mais refinada do que a escolhida: 
# revisar a captura dos SPEAKERS para incluir casos com país em lowercase e só depois de tudo
# usar lista de coalitions distinta dos países, ignorando casos de spokewith com target GRUPO
  

parse_onbehalf <- function(dfspeakersentence){
  patterns_onbehalf <- "COUNTRYNAME,? (SPEAKING FOR|FOR|ON BEHALF OF) (THE )?COUNTRYNAME" 
  df <- dfspeakersentence %>% mutate(behalf = str_detect(sentence_sub, patterns_onbehalf))
  # identifica frases com 'on behalf of' e padroes afins
  # vi um padrão de on behalf com "C1 (on behalf of ...)". Checar se pega.
  
  # Isolar os rows que não têm spokewith
  df_onbehalf <- df %>% filter(behalf == T)
  df_noonbehalf <- df %>% filter(behalf == F)

  # Retirar falsos positivos de COUNTRYNAME on behalf of COALITION
  if(nrow(df_onbehalf)!=0){
    # Como SPEAKER foi construído só a partir de países em CAIXA ALTA, mas o teste faz tudo em toupper,
    # temos casos de 'speaking for' em que só aparece um país mencionado. 
    # Na inspeção, eram casos de 'País, speaking for GROUP', o que na base não configura interação
    # (quando se fala por grupo, Castro só classifica como fala do grupo)
    
    df_onbehalf <- df_onbehalf %>%
     # como estou usando direto nome do país, pode dar problemas se o nome está dentro de outro país
      mutate(speaker = if_else(#colocar boundaries para países que podem estar dentro de outros
        speaker %in% c("US","EU","NIGER"), paste0("(THE |\\b)", speaker, "(\\b|,)"), speaker)
      ) %>% # \b sozinho vai impedir match se COUNTRYNAME for seguido de vírgula
      # talvez precise colocar "^" também pra quando inicia a frase
      group_by(sentence) %>% #juntar os mencionados em uma regex
      mutate(mentioned_countries = paste0("(",
                                          paste0(speaker, collapse = "|"), 
                                          ")")) %>% 
      ungroup()
    
    # Extrair clause do objeto de 'behalf'
    df_onbehalf <- df_onbehalf %>% rowwise() %>% 
      mutate(behalftarget_clause = str_extract_all(toupper(sentence),
                                                   gsub("COUNTRYNAME", mentioned_countries, 
                                                        "(SPEAKING FOR|FOR|ON BEHALF OF) (THE )?COUNTRYNAME"))
             )
    
    # behalftarget_clause sai como lista, pq pode ter mais de uma na frase. 
    
    # Se COUNTRYNAME da behalftarget_clause for COALITION, os que compartilham a behalf clause devem ser
    # retirados de df_onbehalf, pois são falsos positivos
    
    df_onbehalf <- df_onbehalf %>% 
      mutate(behalfcoalitiontest = list(str_detect(behalftarget_clause,
                                          paste0(coalition_list$regex, collapse = "|")))
             )
    
    df_onbehalf %>% filter(all(behalfcoalitiontest) == TRUE) -> false_onbehalf
    false_onbehalf <- false_onbehalf %>% mutate(behalf = FALSE) %>%
      select(-c(mentioned_countries, behalftarget_clause, behalfcoalitiontest))
    
    df_onbehalf %>% filter(all(behalfcoalitiontest) == FALSE) -> df_onbehalf
    df_onbehalf <- df_onbehalf %>%
      select(-c(mentioned_countries, behalftarget_clause, behalfcoalitiontest))
    
    if(nrow(df_onbehalf) != 0){ # só roda se ainda tiver onbehalf após o filtro
      # se tiver behalf válido e inválido na mesma frase, correção se complexifica
      # parsear on_behalf
      
      # Montar a solução: possibilidade de trabalhar via behalf_clause, como nas demais funções
      # (ver funções agreement ou with para exemplo)
      
      # Ou em usar uma variação do método proposto inicialmente, de detectar se o SPEAKER é send ou tgt
      # no behalf e parsear a partir daí
      
      
      # método inicial (não deve funcionar):
      # df %>% rowwise() %>% 
      #   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      #   # preciso reformar o método por aqui!!!! não vai mais funcionar como estava feito o teste
      #   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      #   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      #   mutate(lgl_sender = str_detect(sentence %>% toupper() %>% # logical: speaker é o sender?
      #                                    str_remove_all("[[:punct:]]+?" %>% str_squish()),
      #                                  paste(speaker, patterns_onbehalf)),
      #          lgl_target = str_detect(sentence %>% toupper() %>% # logical: speaker é o target?
      #                                    str_remove_all("[[:punct:]]+?" %>% str_squish()),
      #                                  paste0(patterns_onbehalf, "(.+ and)? ", speaker))) %>% 
      #   mutate(sender_behalf = if_else(behalf == T & lgl_sender == T, speaker, ""), # substitui send/tgt pelo país
      #          target_behalf = if_else(behalf == T & lgl_target == T, speaker, "")) %>% 
      #   ungroup() %>% 
      #   group_by(sentence) %>%  # vai dar problema se tiver mais de um 'on behalf of' na frase.
      #   mutate(sender_behalf = paste0(sender_behalf, collapse = "")) %>% # inclui o sender em todos os rows
      #   select(-c(lgl_sender, lgl_target)) %>% # retira colunas intermediárias
      #   filter(behalf == F | behalf == T & target_behalf != "") # mantém só linhas de targets, sender já está lá
    
      } # segundo if(nrow(df_onbehalf) != 0)
    
    # juntar os falsos positivos ao df de noonbehalf
    df_noonbehalf <- bind_rows(df_noonbehalf, false_onbehalf)
    } # primeiro if(nrow(df_onbehalf) != 0)
  
  # juntar os dois df (onbehalf e noonbehalf)
  df_noonbehalf$sender_behalf <- ""
  df_noonbehalf$target_behalf <- ""
  
  df <- bind_rows(df_noonbehalf, df_onbehalf) %>% 
    arrange(report_id, section_order, parag_order, sent_order)
  
}

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Ainda está incompleta a função!!! Corrigir o método de parsear quando behalf está correto
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#speaker_df <- parse_onbehalf(speaker_df)



# 4.2 "WITH" --------------

parse_spokewith <- function(dfspeakersentence){
  # Verificar se "with" aparece logo antes ou logo depois do nome do país
  # note-se que pode ser 'COUNTRY1 with the COUNTRY2' ou 'COUNTRY1, with COUNTRY2,',
  # 'COUNTRY1, with COUNTRY2, C3, C4, and C5'. Ou, ainda, "With COUNTRY2, COUNTRY 1 abc'
  
  patterns_with <- "^WITH (THE )?COUNTRYNAME,? (THE )?(COUNTRYNAME|S?HE)|COUNTRYNAME,? WITH (THE )?COUNTRYNAME"
  
  df <- dfspeakersentence %>% mutate(spokewith = str_detect(sentence_sub, patterns_with))
  # 'With' is bi-directional: when 'C1 speaks with C2', we should have both 
  # row1 Send: C1 Tgt:2 Spokewith / r2: Send: C2 Tgt: C1
  # Thus, we don't need to worry about sequence. We need to get all countries in the 'with' sentence
  # and make sure we have all possible row combinations
  # These should substitute original rows with the sentence if necessary, but we should not eliminate rows
  # when we have other interactions as well ('With A, B said xyz and C opposed it')
  
  # Isolar os rows que não têm spokewith
  df_spokewith <- df %>% filter(spokewith == T)
  df_nospokewith <- df %>% filter(spokewith == F)
  
  # Criar coluna com WITH CLAUSE para auxiliar análise
  with_clause_pattern1 <- "WITH (THE )?COUNTRYNAME,?(.+AND (THE )?COUNTRYNAME)?,? (THE )?(COUNTRYNAME|S?HE)"
  with_clause_pattern2 <- "COUNTRYNAME,? WITH(?:.+AND)? (?:THE )?COUNTRYNAME"
  
  if(nrow(df_spokewith)!=0){
    # To make function faster, instead of testing w/ all country names, we use only the ones found in sentence
    df_spokewith <- df_spokewith %>% 
      # como estou usando direto nome do país, pode dar problemas se o nome está dentro de outro país
      mutate(speaker = if_else(#colocar boundaries para países que podem estar dentro de outros
        speaker %in% c("US","EU","NIGER"), paste0("(THE |\\b)", speaker, "(\\b|,)"), speaker)
      ) %>% # \b sozinho vai impedir match se COUNTRYNAME for seguido de vírgula
      # talvez precise colocar "^" também pra quando inicia a frase
      group_by(sentence) %>% mutate(mentioned_countries = paste0("(", 
                                                                 paste0(speaker, collapse = "|"), 
                                                                 ")")) %>% 
      ungroup() %>% 
      rowwise() %>% 
      mutate(with_clause = str_extract_all(toupper(sentence), 
                                           paste0("(",
                                                  gsub("COUNTRYNAME", mentioned_countries, with_clause_pattern1),
                                                  ")|(",
                                                  gsub("COUNTRYNAME", mentioned_countries, with_clause_pattern2),
                                                  ")"
                                                  )
                                           )
             ) %>% unnest(with_clause)
    
    # Now, we transfer rows with SPEAKER not in with_clause to df_nospokewith
    # And create SENDER/TARGET for spokewith countries.
    # Note: sometimes, one of the countries is referred to as "he" or "she". We will ignore these cases
    # but should include them in future versions of the code.
    # In these cases, we might include HE/SHE in the speaker/country list and substitute for proper country
    # either in earlier or future steps
    
    # Separate false positives from true positives
    false_spokewith <- df_spokewith %>% filter(str_detect(with_clause, speaker) == F) %>% 
      select(-c(mentioned_countries, with_clause)) %>% mutate(spokewith = F)
    df_nospokewith <- bind_rows(df_nospokewith, false_spokewith)
    
    # Now we need to create rows with all two-wise permutations of mentioned countries
    df_spokewith <- df_spokewith %>% filter(str_detect(with_clause, speaker) == T) %>%
      # limpar novamente os speakers problematicos, pq estavam quebrando o código
      mutate(speaker = if_else(str_detect(speaker, "\\)EU\\("), "EU", speaker)) %>% 
      mutate(speaker = if_else(str_detect(speaker, "\\)US\\("), "US", speaker)) %>% 
      mutate(speaker = if_else(str_detect(speaker, "\\)NIGER\\("), "NIGER", speaker)) %>% 
      
      group_by(sentence) %>% mutate(speaker = paste0(speaker, collapse = ",")) %>% 
      mutate(mentioned_countries = str_split(speaker, ",")) %>% 
      ungroup() %>% distinct() %>%  # eliminar duplicações da mesma with_clause
      rowwise() %>% 
      mutate(mentioned_countries = list(# generate all possible permutations
        expand.grid(mentioned_countries, mentioned_countries, stringsAsFactors = F))) %>% 
      unnest(mentioned_countries) %>% #create row for each element in list
      filter(Var1 != Var2)  %>% #exclude country repetitions
      rename(sender_spokewith = Var1, target_spokewith = Var2) %>% 
      mutate(sender_spokewith = as.character(sender_spokewith),#expand.grid cria como factor
             target_spokewith = as.character(target_spokewith)) %>% 
      select(-with_clause)
  }
  
  # Rejoin the parts of the dataframe
  df_nospokewith$spokewith <- NULL
  
  df_spokewith <- df_spokewith %>% 
    mutate(interaction = if_else(spokewith, "spokewith", interaction),
           sender = sender_spokewith,
           target = target_spokewith) %>% 
    select(-c(spokewith, sender_spokewith, target_spokewith))
  
  
  df <- bind_rows(df_nospokewith, df_spokewith) %>% 
    arrange(report_id, section_order, parag_order, sent_order)
  
  df
}

# speaker_df <- parse_spokewith(speaker_df)

# A fazer na função:
# Vale tentar capturar casos como 'JAPAN proposed abcd, and with A and B, said xyz'?
# (Hoje só captura o A e B como with, não inclui o primeiro)

# >>>>>> da forma como está rodando o código, a coluna speaker fica 'coletiva' após o parse. 
# vale pensar em como isso afeta as funções seguintes!


# Notas:
# um problema, mencionado anteriormente e temporariamente ignorado, é o caso de 'he' no lugar de PAÍS
# Há, porém, um caso de maior complexidade:
# quando temos 'C1, with C2, C3, C4, supported C5', teríamos que incluir uma linha pra cada um no support
# Uma possibilidade seria fazer algo como uma sentence substituindo withclause
# e quando identificarmos support / opposition, distribuir a relação para todos membros da withclause


# 4.3 "AGREEMENT" -------------
# From codebook:
# Agreement: when several countries are reported to hold the same position on an issue. 
# This may be in text like “several parties, including Country 1, Country 2 and Country 3, proposed …”. 
# Agreement may be coded also when two different sentences refer to the same position being held by 
# different countries, even though the relationship (agreeing with each other) is not explicitly written

# Examples:
# - “The EU, the US and CANADA stressed the need to ensure consistency with the capacity building aspects of
# other discussions on technology transfer and adaptation” (ENB No. 145).
# - “He (the EU) also noted that paragraphs proposed by a number of Parties including Norway, Iceland, New
# Zealand and Switzerland could be integrated into the EU proposal” (ENB No. 42).
# - “MALAWI, URUGUAY, ETHIOPIA, COLOMBIA and ROMANIA welcomed activities implemented jointly (AIJ)”


# Fatores de complexidade:
# No momento, vou ignorar os casos não explícitos
# Como spokewith, 'agreement' é bidirecional
# A formulação de agreement é similar usada em enumerações de outros tipos de interação, como SPOKEWITH
  # se não houver essas enumerações em outros tipos de interação, podemos fazer a busca eliminando with

parse_agreement <- function(dfspeakersentence){
  patterns_agreement <- "(?<!FOR THE |FOR |WITH THE |WITH )COUNTRYNAME(?!, WITH)(?<!WITH COUNTRYNAME, COUNTRYNAME|WITH COUNTRYNAME, COUNTRYNAME, COUNTRYNAME).+?(?<!, )AND ((MANY )?OTHERS|(THE )?COUNTRYNAME)"
    #"(?<!FOR THE |FOR |WITH THE |WITH )COUNTRYNAME(?!, WITH).+?(?<!, )AND ((MANY )?OTHERS|(THE )?COUNTRYNAME)"
    # Padrão ainda pega trechos dentro de WITH CLAUSE quando ela é muito longa (ou se tem THE).
    # Tentei cobrir mais, mas lookbehind nao permite maior complexidade.
    # Padrão também captura casos como "C1 EXPRESSED CONCERN, WHILE C2 AND THE C3 URGED"
    # Casos como "COUNTRYNAME ABC AND COUNTRYNAME XYZ" são capturados se não há vírgula no ", AND"
    # Para os casos do WHILE e de BEHALF capturados, é possível limpar a clause.
    
  
  df <- dfspeakersentence %>% 
    mutate(agreement = str_detect(sentence_sub, patterns_agreement)) 
  
  # Isolar os rows que não têm agreement
  df_noagreement <- df %>% filter(agreement == F)
  df_agreement <- df %>% filter(agreement == T)
  
  if(nrow(df_agreement)!=0){
    # Criar coluna com AND CLAUSE para auxiliar análise
    # primeiro fazemos uma regex só com os países mencionados para acelerar teste
    df_agreement <- df_agreement %>%
      separar_paiseschatos() %>%  # corrigir regex países que podem estar dentro de palavra (EU, US, NIGER)
      group_by(sentence) %>% #juntar os mencionados em uma regex
      mutate(mentioned_countries = paste0("(",
                                          paste0(speaker, collapse = "|"), 
                                          ")")) %>% 
      ungroup()
    
    df_agreement <- df_agreement %>% 
      rowwise() %>% 
      mutate(and_clause = str_extract_all(toupper(sentence), 
                                          gsub("COUNTRYNAME", mentioned_countries, patterns_agreement)
                                          )
             ) %>% 
      # criar "" nos rows cuja clause não é capturada, para nao eliminar com unnest
      mutate(and_clause = if_else(length(unlist(and_clause)) == 0,
                                      list(""),
                                      list(and_clause))) %>% 
      unnest(and_clause) #multiplica rows em caso de mais de uma and_clause
    
    # Verificar se há WHILE clauses ("A said xyz, WHILE B and C said abc")
    # (será que há outras formulações, como ", but ..."?)
    # e corrigir de acordo
    df_agreement <- df_agreement %>% 
      mutate(and_clause = if_else(str_detect(and_clause, ", WHILE"),
                                  str_remove(and_clause, ".+, WHILE"),
                                  and_clause))
    
    
    # Transferir falsos positivos de volta para noagreement
    false_agreement <- df_agreement %>% 
      filter(str_detect(and_clause, speaker) == F) %>% 
      select(-c(mentioned_countries, and_clause)) %>% mutate(agreement = F)
    df_noagreement <- bind_rows(df_noagreement, false_agreement)
    
    # Criar rows com as permutações dos países mencionados
    df_agreement <- df_agreement %>% 
      filter(str_detect(and_clause, speaker) == T) %>% #retira os falsos agreement
      corrigir_paiseschatos() %>% # limpar novamente os speakers problematicos, pra nao quebrar o código
      group_by(sentence) %>% mutate(speaker = paste0(speaker, collapse = ",")) %>% 
      mutate(mentioned_countries = str_split(speaker, ",")) %>% 
      ungroup() %>% distinct() %>%  # eliminar duplicações da mesma and_clause
      rowwise() %>% 
      mutate(mentioned_countries = list(#todas permutações possiveis
        expand.grid(mentioned_countries, mentioned_countries, stringsAsFactors = F))) %>% 
      unnest(mentioned_countries) %>% #um row por elemento na list
      filter(Var1 != Var2)  %>% #retira repetiçoes de país
      rename(sender_agreement = Var1, target_agreement = Var2) %>% 
      mutate(sender_agreement = as.character(sender_agreement),# expand.grid cria como factor, corrigir
             target_agreement = as.character(target_agreement)) %>% 
      select(-and_clause)
  }
  
  # Juntar novamente os dataframes
  df_noagreement$agreement <- NULL
  
  df_agreement <- df_agreement %>% 
    mutate(interaction = if_else(agreement, "agreement", ""),
           sender = sender_agreement,
           target = target_agreement) %>% 
    select(-c(sender_agreement, target_agreement, agreement))
  
  df <- bind_rows(df_noagreement, df_agreement) %>% 
    arrange(report_id, section_order, parag_order, sent_order)
  
  df
}

# A fazer na função:
# >>>>>>>>>> Casos em que há behalf na enumeração ('C1, for C2, C1, C1, and C1, for C2') quebram and_clause
# Vi uma formulação em que temos ";" no lugar de "," na enumeração dos países
# - arrumar casos em que ocorre 'andCOUNTRY' (esse é problema anterior, de limpeza do texto)
# investigar outros problemas, como os do tipo 'while clause' ou 'a and b, opposed by c and d'

# speaker_df <- parse_agreement(speaker_df)




# 4.4 SUPPORT --------------
# 4.4.1 Descrição do problema ----------
# From codebook:
# is used when the text explicitly says that country2 (or its statement) was supported by country1, even when
# this support is expressed in different sentences.
# Examples:
# "He (the EU) said additional effort should be made to reduce uncertainty in GWPs but that parties should use
# them if they wish. Japan supported the GWP position (…). Australia (…) also supported continued use of
# GWPs" (ENB No. 2).
# "MAURITIUS, supported by MOROCCO and ZIMBABWE, welcomed AIJ contributions to capacity building
# in developing countries, if they adhere to national objectives, and asked the Secretariat to facilitate the
# initiation of AIJ projects to African countries" (ENB No. 39).


# Fatores de complexidade:
# No momento, vou ignorar os casos não explícitos e os support em frases diferentes
# Também vou ignorar os casos de HE no lugar do país (With C1, he supported abc)
# Ficaria, portanto, com as formulações 'C1 supported C2', 'C1, supported by C2,'
# Problema maior são os casos em que o support vem em grupo. Nesses casos, vamos ter uma estrutura
# de 'with' ou 'agreement' + uma estrutura de support. 
  # With C1 and C2, C3 supported C4; C1, supported by C2, C3, and C4, xyz
  # Aqui, uma sentence_sub trocando as enumerações por um placeholder poderiam ser úteis.

# 4.4.2 Implementação de solução --------------
# Formulação "Welcomed by C2, C1.." - isso é support?

separar_paiseschatos <- function(subdfspeakersentence){
  #colocar boundaries para países que podem estar dentro de outros
  subdfspeakersentence <- subdfspeakersentence %>% 
    # mutate(speaker = if_else(
    #   speaker %in% c("US","EU","NIGER"), paste0("(^| )?(THE )?", speaker#, "( |[,\\.])"
    #                                             ), speaker)
    # ) %>% 
    mutate(speaker = if_else(speaker == "US", "(^| )?(THE )?US(?!TRALIA)", speaker)) %>% 
    mutate(speaker = if_else(speaker == "EU", "(^| )?(THE )?EU", speaker)) %>% 
    mutate(speaker = if_else(speaker == "NIGER", "(^| )?(THE )?NIGER(?!IA)", speaker)) %>% 
    mutate(speaker = if_else(speaker == "CHINA", "(?<!G77 AND )(?<!G-77\\/)CHINA", speaker)) %>% 
    mutate(speaker = if_else(speaker == "LDCs", "LDCS", speaker)) %>% 
    mutate(speaker = if_else(speaker == "EITs", "EITS", speaker)) 
  subdfspeakersentence
}

corrigir_paiseschatos <- function(subdfspeakersentence){
  subdfspeakersentence <- subdfspeakersentence %>% 
    mutate(speaker = if_else(str_detect(speaker, "\\)\\?EU"), "EU", speaker)) %>% 
    mutate(speaker = if_else(str_detect(speaker, "\\)\\?US"), "US", speaker)) %>% 
    mutate(speaker = if_else(str_detect(speaker, "\\)\\?NIGER"), "NIGER", speaker)) %>% 
    mutate(speaker = if_else(speaker == "(?<!G77 AND )(?<!G-77\\/)CHINA", "CHINA", speaker)) %>% 
    mutate(speaker = if_else(speaker == "LDCS", "LDCs", speaker)) %>% 
    mutate(speaker = if_else(speaker == "EITS", "EITs", speaker))
  subdfspeakersentence
}

colar_padroes <- function(pattern_list){
  paste0(paste0("(",pattern_list, ")"), collapse = "|")
}

parse_support <- function(dfspeakersentence){
  # Antes de tratar dos casos mais complexos, vamos para as formulações mais simples possíveis:
  #(pra testar caso com WITH CLAUSE, ver report_id == 491)
  
  # a) formulação país-verbo-país e b) aposto 'supported by'
  patterns_support <- "(?:COUNTRYNAME.+AND )?( \\w+, FOR)?(\\bS?HE|(THE )?COUNTRYNAME|(MANY )?OTHERS),? SUPPORT(S|ED|ING)?( BY)?( \\w+, FOR)? (THE )?COUNTRYNAME(.+AND( \\w+, FOR)? ((THE )?COUNTRYNAME|(MANY )?OTHERS))?"
  patterns_support2 <- "SUPPORTING( \\w+, FOR)? (THE )?COUNTRYNAME,?( \\w+, FOR)? (THE )?COUNTRYNAME,?(.+AND( \\w+, FOR)? ((THE )?COUNTRYNAME|(MANY )?OTHERS))?"
  patterns_support3 <- "^SUPPORTED BY( \\w+, FOR)? (THE )?COUNTRYNAME(.+AND( \\w+, FOR)? ((THE )?COUNTRYNAME|(MANY )?OTHERS))?,?( \\w+, FOR)? (THE )?COUNTRYNAME"
  # incluir na formulação a garantia que não seja 'not support'?
  # incluir na formulação a possibilidade de que seja 'country said (s?he|they) support'?
  # (só vi o 'not support' com uma formulação de 'country said')
  
  df <- dfspeakersentence %>% 
    mutate(support = str_detect(sentence_sub, colar_padroes(c(patterns_support, patterns_support2,
                                                              patterns_support3))
                                )
           ) 
  
  # Isolar os rows que não têm agreement
  df_nosupport <- df %>% filter(support == F)
  df_support <- df %>% filter(support == T)
  
  
  if(nrow(df_support)!=0){
    # Criar coluna com SUPPORT CLAUSE para auxiliar análise
    # primeiro fazemos uma regex só com os países mencionados para acelerar teste
    df_support <- df_support %>%  
      separar_paiseschatos() %>%  # corrigir regex países que podem estar dentro de palavra (EU, US, NIGER)
      group_by(sentence) %>% #juntar os mencionados em uma regex
      mutate(mentioned_countries = paste0("(",
                                          paste0(speaker, collapse = "|"), 
                                          ")")) %>% 
      ungroup()
    # extrair support_clause
    # tem um problema das frases "SUPP BY A, B, C AND D, HE..."
    # a captura da clause para em "SUPP BY A, B"
    
      df_support <- df_support %>% 
      rowwise() %>%
      mutate(support_clause = str_extract_all(toupper(sentence), 
                                              paste0("(", #substitui em todos padrões
                                                     gsub("COUNTRYNAME", mentioned_countries, 
                                                          patterns_support),
                                                     ")|(",
                                                     gsub("COUNTRYNAME", mentioned_countries, 
                                                          patterns_support2),
                                                     ")|(",
                                                     gsub("COUNTRYNAME", mentioned_countries, 
                                                          patterns_support3),
                                                     ")"
                                                     )
                                              )
             ) %>% # criar "" nos rows cuja clause não é capturada, para nao eliminar com unnest
      mutate(support_clause = if_else(length(unlist(support_clause)) == 0,
                                      list(""),
                                      list(support_clause))) %>% 
      unnest(support_clause) %>% #multiplica rows em caso de mais de uma and_clause
      distinct()

      
      
    # Retirar casos da frase fora da SUPPORT CLAUSE, devolvendo-os para df_nosupport
    false_support <- df_support %>% filter(str_detect(support_clause , speaker) == F) %>% 
      select(-c(mentioned_countries, support_clause)) %>% mutate(support = F)
    df_nosupport <- bind_rows(df_nosupport, false_support)
    
    df_support <- df_support %>% filter(str_detect(support_clause, speaker) == T)
    
    # Caso haja casos de opposition capturados por erro na SUPPORT CLAUSE
    # Retiramos os trechos da support clause e parseamos a parte de opposition,
    # devolvendo-os para df_nosupport
    patterns_supoppose <- ", AND OPPOS.+$"
    # HOJE, SÓ FUNCIONA PARA A, SUPPORTED BY B, AND OPPOSED BY C <<<<<<<<<<<<<<<<<<  
      # idealmente, dar conta de OPPOSE antes (A, opposed by B, and supported by C..)
      # e depois do support (A, support by B, AND OPPOSED BY C...).
      if(nrow(filter(df_support, str_detect(support_clause, patterns_supoppose))) > 0){

        df_support <- df_support %>% #filter(str_detect(support_clause, ", AND OPPOS")) %>%
          mutate(clean_clause = str_remove(support_clause, patterns_supoppose)) %>%
          mutate(suplem_clause = str_remove(support_clause, clean_clause)) %>%
          mutate(keepinsupport = str_detect(clean_clause, speaker))


        df_supoppose <- df_support %>% filter(keepinsupport == F) %>%
          # parsear o df_supoppose: pegar o target e depois parsear como oppose?
          mutate(tgt_clause = str_extract(support_clause, "(.+)(?:, SUPPORTED BY)", group = 1),
                 #pega o 1o grupo da regex (o que vem antes da vírgula do supported by)
                 # sender pega o 2o grupo da regex, após 'opposed by'
                 sender_clause = str_extract(suplem_clause, "(?:OPPOSED BY) (.+)", group = 1)) %>%
          mutate(opposition_targets = str_extract_all(tgt_clause, mentioned_countries)) %>%
          corrigir_paiseschatos() %>%
          mutate(sender_opposition = if_else(str_detect(sender_clause, speaker),
                                             speaker, ""),
                 target_opposition = str_extract(mentioned_countries, tgt_clause)) %>%
          mutate(support = F, opposition = T) %>%
          select(-c(mentioned_countries, support_clause, clean_clause, suplem_clause, keepinsupport,
                    tgt_clause, sender_clause, opposition_targets)) %>%
          ungroup()
        
        df_supoppose <- df_supoppose %>% 
          mutate(interaction = if_else(opposition, "opposition", "")) %>% 
          mutate(sender = sender_opposition, target = target_opposition) %>% 
          select(-c(sender_opposition, target_opposition, opposition))

        # junta resultado em df_nosupport
        df_nosupport <- bind_rows(df_nosupport, df_supoppose) %>% 
          corrigir_paiseschatos()
        
        
        # Retoma a análise do df_support
        df_support <- df_support %>% filter(keepinsupport) %>%
          mutate(support_clause = clean_clause) %>%
          select(-c(clean_clause, suplem_clause, keepinsupport))
      }
    
    # Identificar se SPEAKER é o SUPPORTED (Country2/Target) ou o SUPPORTER (Country1/Sender)
    # Pelo que entendo, há sempre um único supported, mesmo que haja muitos supporters.
    # Logo, seria mais fácil identificar o supported e os demais viriam por exclusão.
    # Para padrões 'C2, supported by C1, C1, and C1'; 'Supporting C2, C1 and C1':
    #       Pegar o primeiro mencionado como SUPPORTED (Target). Demais viram SUPPORTERS (Sender)
    # Para os padrões 'C1 and C1, supporting C2', é preciso identificar o objeto - país após 'supporting'.
    
    df_support <- df_support %>%
      mutate(countries_vector = str_extract_all(support_clause, mentioned_countries)) %>% 
      rowwise %>% 
      mutate(#solução distinta nas formulações em que PAÍS APOIADO aparece primeiro ou por último
        lgl_supported = if_else(str_detect(support_clause, "^SUPPORTING|, SUPPORTED BY"), # qdo aparece primeiro
                                str_detect(countries_vector[1], speaker), #teste se speaker é 1o do vetor
                                str_detect(support_clause, 
                                           paste0("(", #colar identificador de target 
                                                  "SUPPORT(S|ED|ING) ", speaker, ")|(", #pro padrao2
                                                  "^SUPPORTED BY.+(AND.+)?,? ", speaker,# e pro padrao 3
                                                  "(?! AND|,)",")") #neg look pra evitar erro em enumeraçao
                                           )
                                )
        )


    
    df_support <- df_support %>% 
      rowwise() %>% 
      corrigir_paiseschatos() %>% # limpar novamente os speakers problematicos
      mutate(sender_support = if_else(lgl_supported == F, speaker, ""), #incluir nomes dos países
             target_support = if_else(lgl_supported == T, speaker, "")) %>% 
      ungroup %>% 
      group_by(sent_order, support_clause) %>% 
      mutate(target_support = paste0(target_support, collapse = "")) %>% # inclui o target em todos os rows
      ungroup() %>% 
      filter(sender_support != "") %>% # Garantir que mesma interação nao aparece duplicada
      # retira cols intermediárias
      select(-c(mentioned_countries, support_clause, countries_vector, lgl_supported))
  }
  
  # Juntar os dataframes support e os no support
  # Juntar novamente os dataframes
  df_nosupport$support <- NULL
  
  df_support <- df_support %>% 
    mutate(interaction = if_else(support, "support", ""),
           sender = sender_support,
           target = target_support) %>% 
    select(-c(sender_support, target_support, support))
  
  df <- bind_rows(df_nosupport, df_support) %>% 
    arrange(report_id, section_order, parag_order, sent_order)
  
  
  
  df
}


# speaker_df <- parse_support(speaker_df)


# A fazer na função:
# Verificar funcionamento para os casos de apoio coletivo (com with/and_clauses)
# Checar se há outras formulações não capturadas para 'SUPPORT'
# Incluir 'and (many )?others'


# 4.5 OPPOSITION -----------
# From codebook:
# when the text reports country1 opposing the statement or position expressed by country2. This has also
# been coded when the word “opposition” is not explicitly mentioned, but it is clear from the statements
# that they oppose each other.

# Examples:
# “The G-77/CHINA supported this approach while the US, CANADA and JAPAN opposed it, stressing the
# need for a new and different mandate” (ENB No. 347).
# "MEXICO underscored its commitment to mechanisms and processes that increase the participation of
# observers. (…) NIGERIA noted that although participation of stakeholders has been positive, the UNFCCC is
# an intergovernmental process" (ENB No. 489).
# "Parties also discussed three annexes to the draft COP/MOP decision. BOLIVIA requested bracketing all of
# them, saying parties had not had time to analyze them (…). JAPAN stressed the “enormous effort” to provide
# text in the annexes to everyone a month before the meeting, and that the annexes “were adopted two days
# ago.”" (ENB No. 497).
# "The G-77/CHINA stressed the importance of a paragraph on evaluating the real cost of preparing national
# communications. The EU, with AUSTRALIA, highlighted that this skill-set was not present in the CGE, and
# supported deleting the paragraph" (ENB No. 185).



# Como em support, só trabalhamos com os casos de oposição explícita em mesma frase
# (isto é, marcados com verbo 'to oppose')



parse_opposition <- function(dfspeakersentence){
  # Antes de tratar dos casos mais complexos, vamos para as formulações mais simples possíveis:
  #(pra testar caso com WITH CLAUSE, ver report_id == 491)
  
  # formulação: c1 opposed c2, c2, opposed by, c1 /opposing c2, c1 abc
        # se tiver enumeraçao com FOR no meio, pode perder os anteriores dependendo da formulação.
  patterns_opposition1 <- "(?:COUNTRYNAME.+AND )?((MANY )?OTHERS|(\\w+, FOR )?(THE )?COUNTRYNAME).+,?( BUT)? OPPOS(ES|ED|ING)?( BY)? (\\w+, FOR )?(THE )?COUNTRYNAME(.+AND ((MANY )?OTHERS|(\\w+, FOR )?(THE )?COUNTRYNAME))?"
  patterns_opposition2 <- "OPPOSING (\\w+, FOR )?(THE )?COUNTRYNAME(.+AND (\\w+, FOR )?((THE )?COUNTRYNAME|(MANY )?OTHERS))?,? (\\w+, FOR )?COUNTRYNAME,?(.+AND ((\\w+, FOR )?(THE )?COUNTRYNAME|(MANY )?OTHERS))?"
  

  df <- dfspeakersentence %>% 
    mutate(opposition = str_detect(sentence_sub, paste0("(", patterns_opposition1, ")|(", 
                                                        patterns_opposition2, ")")
                                   )
           ) 
  
  
  # Isolar os rows que não têm agreement
  df_noopposition <- df %>% filter(opposition == F)
  df_opposition <- df %>% filter(opposition == T)
  
  
  if(nrow(df_opposition)!=0){
    # Criar coluna com SUPPORT CLAUSE para auxiliar análise
    # primeiro juntamos só os países mencionados pra acelerar o código
    df_opposition <- df_opposition %>%  
      separar_paiseschatos() %>% #corrigir regex países que podem estar dentro de palavra (US, EU, NIGER)
      group_by(sentence) %>% #juntar os mencionados em uma regex
      mutate(mentioned_countries = paste0("(",
                                          paste0(speaker, collapse = "|"), 
                                          ")")) %>% 
      ungroup() %>% 
      rowwise() %>% # aqui está incompleto, tem mais de um pattern! >>>>>>>>>>>>>>>>>>>>>>>>
      mutate(opposition_clause = str_extract_all(toupper(sentence), 
                                                 paste0("(", #substitui em todos padrões
                                                        gsub("COUNTRYNAME", mentioned_countries,
                                                             patterns_opposition1),
                                                        ")|(",
                                                        gsub("COUNTRYNAME", mentioned_countries,
                                                             patterns_opposition2),
                                                        ")"
                                                        )
                                                 
                                              # gsub("COUNTRYNAME", mentioned_countries, patterns_opposition1)
                                              ) # aqui está incompleto, tem mais de um pattern!
             ) %>% # criar "" nos rows cuja clause não é capturada, para nao eliminar com unnest
      mutate(opposition_clause = if_else(length(unlist(opposition_clause)) == 0,
                                      list(""),
                                      list(opposition_clause))) %>% 
      unnest(opposition_clause) %>% #multiplica rows em caso de mais de uma clause
      distinct() #retira repetidos (e.g. mesmo país fala duas vezes na frase)
    
    # Retirar casos da frase fora da OPPOSITION CLAUSE, devolvendo-os para df_noopposition
    false_opposition <- df_opposition %>% filter(str_detect(opposition_clause , speaker) == F) %>% 
      select(-c(mentioned_countries, opposition_clause)) %>% mutate(opposition = F)
    df_noopposition <- bind_rows(df_noopposition, false_opposition)
    
    df_opposition <- df_opposition %>% filter(str_detect(opposition_clause, speaker) == T)
    
    # Identificar se SPEAKER é o OPPOSED (Country2/Target) ou o OPPOSER (Country1/Sender)
    
    # No padrão "C2, OPPOSED BY C1,", todos que aparecem ANTES de "OPPOSED BY" são tgt, DEPOIS, sender
    # No padrão "C1, OPPOSING C2", todos que aparecem ANTES são sender, DEPOIS, target
    # No padrão "OPPOSING C2, C1 PROPOSED", o separador é a vírgula. Mas em casos de enumeração, é vírgula
    # após o AND COUNTRYNAME/OTHERS ("OPPOSING COUNTRYNAME AND COUNTRYNAME, COUNTRYNAME AND COUNTRYNAME DID THIS.")
    # Esse último é mais complicado.
    
    df_opposition <- df_opposition %>% 
      # primeiro, identifica a localização do OPPOSE na string
      mutate(oppose_position = str_locate(opposition_clause, "(OPPOSED BY|, OPPOSING )")) %>%
      # estou ignorando no momento o padrão 'OPPOSING C2, C1 ABC'.
      
      # parse distinto cf padrão
      mutate(lgl_opposer = if_else(str_detect(opposition_clause, "OPPOSED BY"),#se opposed by, opposer depois
                                   str_locate(opposition_clause, speaker)[,"start"] > oppose_position[,"end"],
                                   str_locate(opposition_clause, speaker)[,"end"] < oppose_position[,"start"]
                                   ),
             lgl_opposed = if_else(str_detect(opposition_clause, "OPPOSED BY"),#se opposed by, opposer depois
                                   str_locate(opposition_clause, speaker)[,"end"] < oppose_position[,"start"],
                                   str_locate(opposition_clause, speaker)[,"start"] > oppose_position[,"end"]
                                   )
             )
               
    
    # Se tivermos múltiplos OPPOSERS/OPPOSED, é preciso distribuí-los em todas combinações de sender-target
    # agrupa por opposition_clause
    df_opposition <- df_opposition %>% 
      corrigir_paiseschatos() %>% # limpar os speakers problematicos, pra nao quebrar o código
      rowwise() %>% 
      mutate(sender_opposition = if_else(lgl_opposer == T, speaker, ""), #incluir nomes dos países
             target_opposition = if_else(lgl_opposed == T, speaker, "")) %>% 
      ungroup %>%
      group_by(sent_order, opposition_clause) %>% #nao pode ser só op_clause, pq pode ser igual em + frases
      # junta os sender e target em todos os rows. unique p/ nao vir repetido
      mutate(sender_opposition = paste0(unique(sender_opposition), collapse = ","),
             target_opposition = paste0(unique(target_opposition), collapse = ",")) %>% 
      # evitar a duplicação: manter apenas um row por opposition_clause
      select(-c(lgl_opposer, lgl_opposed)) %>%  # retira as colunas do teste lógico agora, p/ distinct()
      mutate(speaker = paste0(unique(speaker), collapse = ",")) %>% # p/ distinct() funcionar, speaker igual
      ungroup() %>% distinct()
    
    
    df_opposition <- df_opposition %>% 
      group_by(sent_order, opposition_clause) %>%
      mutate(mentioned_senders = str_split(unique(sender_opposition), ","),
             mentioned_targets = str_split(unique(target_opposition), ",")) %>%
      # retirar eventuais elementos vazios no vetor
      mutate(mentioned_senders = list(unlist(mentioned_senders) %>% stringi::stri_remove_empty() ),
             mentioned_targets = list(unlist(mentioned_targets) %>% stringi::stri_remove_empty() )
             )
    
    # gerar as combinações 2 a 2, mantendo os grupos separados
    df_opposition <- df_opposition %>% ungroup %>% 
      rowwise() %>% 
      mutate(mentioned_countries = list(# generate all possible permutations
        expand.grid(unlist(mentioned_senders), 
                    unlist(mentioned_targets), 
                    stringsAsFactors = F))) %>% 
            unnest(mentioned_countries) %>% #create row for each element in list
      filter(Var1 != Var2)  %>% #exclude country repetitions
      mutate(sender_opposition = Var1,
             target_opposition = Var2) %>% 
      select(-c(opposition_clause, Var1, Var2, oppose_position, mentioned_senders, mentioned_targets))

  }
  
  # Juntar os dataframes support e os no support
  # Juntar novamente os dataframes
  # df_noopposition$sender_opposition <- ""
  df_noopposition$opposition <- NULL
  
  df_opposition <- df_opposition %>% 
    mutate(interaction = if_else(opposition, "opposition", ""),
           sender = sender_opposition,
           target = target_opposition) %>% 
    select(-c(sender_opposition, target_opposition, opposition))
  
  
  df <- bind_rows(df_noopposition, df_opposition) %>% 
    arrange(report_id, section_order, parag_order, sent_order)
  
  df
}

# Em casos como "JAPAN, supported by the US and others, but opposed by Tanzania, for the G-77/CHINA, proposed inviting"
# (report_id == 239), opposition target está pegando US além de Japão. Deveria corrigir como em sup-oppose?
# speaker_df <- parse_opposition(speaker_df)


# 4.6 CRITICISM ------------
# From codebook:
# when country1 directly criticizes country2 or its position / statement.
# Examples:
# “He (EU) said some developed countries, particularly the US, have not included binding measures in their
# proposals and emphasized the EU’s conviction that P&Ms should be included to fully encompass the Berlin
# Mandate and Geneva Declaration” (ENB No. 42).
# “The MALDIVES lamented that reliance on the phrase “form should follow function” [used by China] is
# slowing down the negotiations” (ENB No. 494).
# “CHINA criticized the US presentation for changing the direction of the AGBM, failing to link development
# with the existing economic structure of a country and considering only the industrial development that has
# occurred since 1990” (ENB No. 24).


# A princípio, vou fazer só o caso mais simples de todos ("C1 criticized C2", "Criticizing C2, C1...")




# 4.7 DELAY PROPOSAL -------
# From codebook:
# when country1 proposes that country2’s idea or proposal be discussed at a later time.
# Examples:
# - "The EU recognized Kazakhstan’s aspiration to join Annex B, while highlighting the need to comply with
# legal requirements relating to Annex B amendments. She supported deferring the issue to COP/MOP 6"
# - "TOGO, supported by MALAYSIA, proposed adjourning until numbers were proposed"

# 5. RODANDO AS FUNÇÕES ------------------------------------------
# a corrigir: primeiros relatórios têm estrutura diferente e devem ser parseados diferente
# (se notar, nenhum resultado em tst pré 96)
# em support/opposition, adicionar os "text, proposal, suggestion, ammendment" 
# (todos com/sem 's antes e singular e plural) seguindo os países. não é tao comum, mas perde os casos


analysed_ids <- sample(reports$report_id, 10)
# analysed_ids <- reports$report_id

map_df(analysed_ids, organize_report) %>% 
  organize_byparagraph() %>% 
  organize_bysentence() %>% 
  identify_speaker() -> tst


# 5.1 pipe para processar os dados ----------------
# a princípio, vou parsear os dados segundo uma sequencia específica
# tanto support como opposition vão rodar com os dados completos
# em seguida, vou juntar os dois e retirar duplicados (lembrando que há alguns opposition em support)
# depois, p/ os casos sem classificação, vou rodar as funções menos organizadas
# (no caso, só agreement, pq nao arrumei spokewith ainda) - interessante manter assim, pq
# agreement pega enumerações dentro de support/oppose ('A, B, and C opposed D' -> oppose parseia, agree tb)
# ao final, juntamos tudo, limpamos eventuais duplicados, pegamos dados do relatorio e salva
sup <- parse_support(tst)
sup <- sup %>% corrigir_paiseschatos() %>% distinct()
op <- parse_opposition(tst)
op <- op %>% corrigir_paiseschatos() %>% distinct()

dados <- bind_rows(sup, op)

agre <- dados %>% filter(interaction != "") %>% 
  parse_agreement()
agre <- agre %>% corrigir_paiseschatos() %>% distinct()

dados <- bind_rows(dados, agre)
dados <- dados %>% filter(interaction != "") %>% 
  filter(target != ""|sender != "") %>% 
  select(-speaker) %>% distinct()
dados <- left_join(dados, reports)
dados <- arrange(dados, negotiation_date) %>% mutate(row_id = row_number())
dados <- select(dados, -c(section_order, parag_order, countrymatch_count,
                          sentence_sub, link, text))

# precisaria padronizar os países pra nao ter problema de várias grafias (G77)


write.csv2(dados, "interacoesscraped96-13.csv")



# # testar as funções de parse com tst
# supported <- parse_support(tst) %>% filter(interaction != "")
# opposed <- parse_opposition(tst) %>% filter(interaction != "")
# interactions <- bind_rows(supported, opposed) %>% select(-speaker) %>% distinct
# interactions <- left_join(interactions, select(reports, c(event, negotiation_date, report_id)))
# interactions <- interactions %>% select(-c(section_order, parag_order, countrymatch_count,
#                                            sentence_sub, sent_order))
# View(parse_opposition(tst))
#parse_agreement(tst) %>% View
# 
# # 6. Correção de problemas -----------------
# # Rodando para tudo, identificamos alguns erros recorrentes:
# 
# # 2) Em vários casos, não temos a interação completa. pode ser útil ver o tipo de padrão que quebrou
# # faltando o sender só um caso; faltando target, foram 103 vazios
# interactions %>% filter(target == "" | sender == "") %>% 
#   select(c(report_id, sentence, interaction, sender, target)) -> problem_sentences
# 
# # 3) Além desses, é preciso inspecionar mais manualmente pra entender o que mais está falhando
# # isso deve ser feito antes dos filtros, não nos dfs supported/opposed.
# op <- parse_opposition(tst)
# op <- distinct(op) 
# oppose_patterns_toexplore <- op %>% filter(interaction != "opposition") %>% 
#   filter(str_detect(toupper(sentence),"OPPOS"))
#   # Alguns padrões: 'he', oposição menos explícita (fala o conteúdo das posições), proposal
# 
# 
# sup <- parse_support(tst) 
# sup <- distinct(sup)
# support_patterns_toexplore <- sup %>% filter(interaction != "support") %>% 
#   filter(str_detect(toupper(sentence),"SUPPOR"))
