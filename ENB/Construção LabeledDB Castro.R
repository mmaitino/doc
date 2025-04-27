library(tidyverse)

## IMPORTAÇÃO E PREPARO DA BASE -----------
#...Castro database (.dta) -----
path1 <- here::here("C:/Users/marti/Documents/Doutorado/controle_doc/extradbs", "dbs", "Castro", "ENB_relationships.dta")
relationdb <- haven::read_dta(path1)

# relationdb <- haven::read_dta(here::here("dbs","Castro", "ENB_relationships.dta"))

db <- relationdb %>% filter(quote != "") %>% # manter só os com quote
  select(c(obs_id, Country1, Country2, relation, cooperation, topic, e_date, ENB_Nr, quote, coalition)) %>% 
  filter(coalition == 0) # p/ simplificar, retirar os casos em que coalition é sender/tgt <<<<<<


#...Funções e dados úteis  ---------
#.......Lista de países e coalizões ---------
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
              "Korea, Republic",
              "Korea, Democratic Rep.",
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
            "(?<!North |Democratic |Democratic Republic of |People's Republic of |Peoples Republic of )Korea",
            "(North|Democratic) (People'?s )?(Republic of )?Korea",
            "Kyrgyz Republic|Kyrgyzstan", "Lao(s| People| PDR)",
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
# garantir que não está no meio de palavra (boundaries word e non-word)
# \b = pattern é seguido de um NON-WORD CHARACTER. \B = pattern é seguido de um word char
# Provavelmente incluí o \B pra dar conta de erros como falta de espaço etc entre palavras
# Isso cria, porém, problema pros casos em que o nome do país está incluído em outro
# (na string NIGERIA, temos match em NIGER\B, por isso a adaptação mais acima 
# o mesmo vale para US como sigla, mas nesse caso vamos só retirar o \B pq há muitos erros
# possíveis. Para US foi adicionado um negative lookahead para eliminar tb "$" (evitar match US$)

# Há poucos casos em que United States aparece por extenso. Vou juntar os regex
# (estou mantendo o procedimento acima pq foi usado em outro código e o regex de US era bastante
# propenso a criar erros. Não sei se houve motivo pra usar US como segundo país)
country_list <- country_list %>% mutate(regex = if_else(country == "United States", 
                                        "\\bUS(?![$\\w])|(\b|\\B)UNITED STATES(\\b|\\B)", regex))

coalition_list <- country_list %>% filter(
  country %in% c("AILAC", "ALBA", "AOSIS", "African Group", "Arab Group", 
                 "BASIC", "CACAM", "COMIFAC", "Cartagena Dialogue", "Central America", 
                 "Central Group", "Central Group Eleven", "Coalition of Rainforest Nations",
                 "Congo Basin Countries", "Environmental Integrity Group", "G77", "Group of 9",
                 "JUSCANZ", "LDCs", "Like Minded Developing Countries", 
                 "Mountain Landlocked Developing Countries", "OECD", "OPEC", "SICA", "Umbrella Group", 
                 "Visegrad Group", "Southern Africa Development Community", "Caribbean Community", "EITs")
)

##...Placeholder países -------
# para acelerar os matches, é interessante substituir países por um placeholder
substituir_paises <- function(txt){
  # nota: função só substitui quando está em caixa alta
  sub_str <- str_replace_all(txt,
                             paste0(country_list$regex, collapse = "|"),
                             "COUNTRYNAME")
  sub_str
}


## LIMPANDO AS QUOTES ---------
# São 4405 unique quotes, com 25738 relações extraídas delas.

db <- db %>% mutate(quote = str_remove_all(quote, '\"')) #padronizar sem aspas

## Limpeza de quotes 
db <- db %>% filter(!str_detect(quote, "->")) # retirar o padrão mais óbvio

# alguns textos em quote não seguem o padrão esperado do relatório, sendo escritos pelos anotadores
# idealmente, teríamos só os do relatório do ENB. Em muitos casos, temos citações dos relatórios, mas não é
# uma frase única e sim combinação de vários trechos. Como esse não será o formato do banco final,
# idealmente, deveria separar esses formatos de string pra não atrapalhar o treinamento.



# fazer só uma vez por quote pra reduzir tempo de process
db <- db %>% group_by(quote) %>% mutate(quote_id = cur_group_id()) %>% ungroup()
db_quotes <- db %>% select(quote, quote_id) %>% distinct()

# separar quotes com estrutura País: "..." País: "..." ou Position: "...", País: "..."
db_quotes <- db_quotes %>% mutate(quote_type = if_else(str_detect(sub_quote, "COUNTRYNAME:"),
                                          "coder_multisentence","original_continuo"))
# idem para quotes com estrutura "Texto original (...) Texto original"
db_quotes <- db_quotes %>% mutate(quote_type = if_else(str_detect(sub_quote, "\\.\\s?\\(\\.\\.\\.\\)"),
                                          "original_multisentence", quote_type))
  

# retirar quotes com menos de duas entidades (país ou coalizão) mencionadas
db_quotes <- db_quotes %>% mutate(sub_quote = substituir_paises(toupper(quote)))
db_quotes <- db_quotes %>% mutate(country_count = str_count(sub_quote, "COUNTRYNAME"))


# trabalhar só com 2country quotes
# >> nota: às vezes as duas entidades mencionadas na cita não são as de C1 e C2. Vai depender
# de outro filtro mais adiante.
twocountry_quotes <- db_quotes %>% filter(country_count == 2)
twocountry_quotes <- twocountry_quotes %>% left_join(db)

# MASKING DAS QUOTES ----
# Tenho que editar as citações de forma a garantir duas coisas:
# 1) Não queremos que modelo aprenda "alianças" ou relações recorrentes de cooperação/conflito entre
# os países. Queremos que isso seja aprendido pelo texto, não pelo nome dos países.
# O modo mais fácil é garantir que o modelo não acesse o nome de cada país naquela interação

# 2) Uma dificuldade do projeto é o fato de as relações serem direcionais e podermos ter múltiplos países
# citados em uma mesma frase. Isso significa que precisamos diferenciar sender, target e outros países citados
# e que um mesmo trecho pode dar origem a resultados distintos a depender do país escolhido como sender/tgt. 

# Talvez precise montar dois modelos distintos: o primeiro identifica se há relação relevante
# e o segundo identifica o tipo de relação. 
# O modelo de relevância vai exigir "expandir" os dados de Castro pra um desenho mais similar
# ao que fiz na minha coleta(i.e., incluir todos os arranjos entre países citados em cada trecho, 
# sendo a presença no banco final a variável a ser prevista)

# Aqui eu vou montar só pensando no segundo modelo (tipo de interação).

##...Masking de Sender e Target cf colunas ----
# Como vou iniciar com casos só de 2 países, não preciso me preocupar com o masking dos terceiros.
# Ideia agora é substituir o país das colunas Country1 por SENDER e Country2 por TARGET
# Idealmente, faria isso sem ter que converter o texto completo em caixa alta, o que pode prejudicar modelos
# Sender tem, em teoria (pelas regras do report, mas nem sempre), caixa alta. Os demais não.

# Incluir os regex correspondentes para cada país

## checar alguns erros em no masking (US e THE US não estão funcionando, pq essas correções entraram em
## country list sob o row "US"

twocountry_quotes <- twocountry_quotes %>% mutate(country = Country1) %>% left_join(country_list) %>% 
  rename(sender = country, sender_regex = regex)
twocountry_quotes <- twocountry_quotes %>% mutate(country = Country2) %>% left_join(country_list) %>% 
  rename(target = country, target_regex = regex)
# Mascarar os nomes dos países
twocountry_quotes <- twocountry_quotes %>%  
  mutate(masked_quote = str_replace_all(toupper(quote), sender_regex, "SENDER"))
twocountry_quotes <- twocountry_quotes %>%  
  mutate(masked_quote = str_replace_all(masked_quote, target_regex, "TARGET"))


# Há alguns casos de quote insuficiente (ou errada) pra classificação
# Uma solução simples é retirar casos em que Sender ou Tgt não são mencionados na quote
# (classificação nesses dependeria do contexto, que não temos)

no_sender <- twocountry_quotes %>% filter(str_detect(toupper(quote), sender_regex) == F)
no_target <- twocountry_quotes %>% filter(str_detect(toupper(quote), target_regex) == F)
twocountry_quotes <- twocountry_quotes %>% 
  filter(! quote_id %in% c(no_sender$quote_id, no_target$quote_id))
  # o problema é que reduz em muito o tamanho da base (2871 - 371 sem sender - 706 sem target)


###......Exportando primeira versão do banco pra explorar modelos ----
twocountry_quotes %>% 
  select(obs_id, quote_id, e_date, ENB_Nr, Country1, Country2, 
         quote, masked_quote, cooperation, relation) %>% 
  write.csv2("twocountry_v1.csv")

##...Masking de demais países e coalizões ----
# Para 3+ entidades mencionadas, vou precisar fazer o masking dos demais. Idealmente, com as variações
# o modelo entenderia que "SENDER, with TGT, opposes OTHER" é diferente de "SEND, with OTHER, opposes TGT"
# Em termos de tempo de processamento, esse é o pior pra fazer, esp. se considerar variações de caps.




## feito o masking, posso treinar modelo
