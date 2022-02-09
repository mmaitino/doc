library(tidyverse)
library(haven)

# open Castro Database
statements_count <- read_dta(
  "C:/Users/marti/Documents/Doutorado/Dados/Base Castro - relational UNFCCC/statements_count.dta")
# list all mentioned coalitions and countries
country_list <- statements_count %>% pull(country) %>% unique

# save as csv
write.csv2(country_list, "castro_countrylist.csv")

# antes de usar a lista para identificar menções no texto, 
# vamos ter que rearranjar os nomes de alguns países
# atenção especial ao republic of the congo, 
# que estaria incluso em DRC (idem DPKR). Regex vai ser necessário
