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
                   trim_ws = TRUE)

class <- read_delim("class-2021-04-21.csv", 
                    ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE)

eventos <- read_delim("eventos_v2.csv", 
                      ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                      trim_ws = TRUE)

# Criar deleg_completo (deleg+orgs+class) --------


# Descrição da base dos eventos --------

### Limpar e renomear colunas

### Linha: evolução eventos no tempo, dividido total e coletados

### Colunas empilhadas: número de eventos coletados/total, divididos por temas


# Descrição das delegações ---------