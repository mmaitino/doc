library(here)
library(tidyverse)

# Leitura dos arquivos ----

arq_pep <- list.files(here("Dados originais"), "PEP.+xlsx", full.names = T)
base_pep <- tibble()


for(i in seq_along(arq_pep)){
  tmp <- readxl::read_xlsx(arq_pep[i])
  tmp <- tmp[-1,] #ret linha inicial
  tmp <- tmp[1:(nrow(tmp) - 5),] #ret linhas finais
  names(tmp) <- c("ano", "vinc","cont_servidores","orgao_superior",
                  "orgao","escol_servidores")
  base_pep <- bind_rows(base_pep, tmp)
}


# Para cada arquivo da lista, leio o xlsx, limpo e, ao final, unifico todos.
# Retirar segunda linha do arquivo (não são dados, só "total de seleçao")
# Retirar cinco linhas finais do arquivo (Detalhes da consulta ao PEP)

# P/ registro, esses detalhes e linhas  são os mesmos para todos os arquivos:
# [Linha vazia separando resultados]
# Selection Status:
# Grupo Situação do Vinculo: Ativo
# Sem GDF (Servidores): Sim
# Ano Servidores: [variado]


# Unificar categorias superior ou mais
categ_super <- c("Doutourado", "Mestrado", 
                 "Superior Completo ou habilitação legal equivalente",
                 "Especialização Nível Superior (Titulação)",
                 "Mestrado (Titulação)",
                 "Doutorado (Titulação)",
                 "Aperfeiçoamento Nível Superior (Titulação)",
                 "Pós-Doutorado (Titulação)",
                 "Graduação Nível Superior Completo (Titulação)",
                 "Licenciatura (Titulação)",
                 "Licenciatura Plena (Titulação)",
                 "Especialização - Na (Titulação)",
                 "Bacharel (Titulação)",
                 "Pós-Graduação (Titulação)",
                 "Mestre+Rsc-III Lei 12772/12",
                 "Pós-Graduação Rsc-II L12772/12")

base_pep <- base_pep %>% 
  mutate(escol_servidores = if_else(escol_servidores %in% categ_super,
                                    "Superior completo ou maior", 
                                    "Superior incompleto ou menor"))

# Organizar as variáveis da base ----
base_pep <- base_pep %>% group_by(ano, orgao_superior, escol_servidores) %>% 
  summarise(total = sum(cont_servidores))

base_pep <- base_pep %>% pivot_wider(names_from = escol_servidores, 
                                     values_from = total)
names(base_pep)[3] <- "servidores_superior"
names(base_pep)[4] <- "servidores_naosup"

# Exportar arquivo ---
write.csv2(base_pep, "base_pep.csv")

# Retirada dos institutos federais e CNPq virá quando unificar
# Adição do id-unico será manual?
  
