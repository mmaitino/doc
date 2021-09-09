library(tidyverse)


# Join ministerios e participaçao ------------
min_ano <- mutate(min_ano, id_org_ano = paste0(ano, id_org_unica))

part_min_ano <- mutate(part_min_ano, id_org_ano = paste0(ano, id_org_unica)) %>% 
  distinct() #checar cód que gerou: pq estao duplicadas as observações de part_min_ano?


# Limita observaçoes a partir de 1997 (qdo começa o BEP)
# Join a partir de part pra manter atores completos (evita um 'complete' em min_ano)
min_ano <- part_min_ano %>% filter(ano >= 1997) %>% left_join(min_ano)
# Retirar ministerios que nao existem no período 1997-2019
ids_retirar <- min_ano %>% group_by(id_org_unica) %>%
  summarise(exist_count = sum(existencia)) %>% 
  filter(exist_count == 0) %>% pull(id_org_unica)

min_ano <- min_ano %>% filter(id_org_unica %in% ids_retirar == FALSE)

# Gerar tabela existência min-ano -----------
duracao <- mutate(duracao, 
                  extinçao = if_else(
                    is.na(extinçao), 
                    as.numeric(format(Sys.Date(), "%Y"))+1,
                    extinçao)
)


# Primeiro preciso identificar quando há múltiplas existencias do órgao
# Possibilidade: criar coluna 'duracao' com um vetor de números entre criaçao e ext
# Agrupar por id e adicionar os novos elementos aos vetores
durabup <- duracao



duracao <- duracao %>% rowwise(id_org_unica) %>% 
  summarise(duracao = list(criaçao:(extinçao-1))
  )

duracao %>% group_by(id_org_unica) %>% 
  summarise(duracao_completa = duracao)
names(duracao$duracao) <- duracao$id_org_unica


duracao <- duracao %>% group_by(id_org_unica) %>% summarise(
  lista_duracao = list(c(duracao)))

# Criada a coluna com as listas de anos duração, join com df principal
min_ano <- left_join(min_ano, duracao)

# Criar a coluna existencia a partir da lista
min_ano <- min_ano %>% rowwise %>% 
  mutate(
    existencia = if_else(ano %in% unlist(lista_duracao), 1, 0)
  ) %>% 
  select(-lista_duracao) #drop da coluna de lista



save_updated <- function(nome_base){
  write.csv2(get(paste0(nome_base), globalenv()),
             paste0(nome_base,"-",Sys.Date(),".csv"), row.names = F
  )
}
save_updated("min_ano")

# Brincando com um primeiro modelo ------------
nunca_part <- min_ano %>% group_by(id_org_unica) %>% 
  summarise(soma_parts = sum(part_binaria)) %>%
  filter(soma_parts == 0) %>% pull(id_org_unica)

probit_part <- glm(part_binaria ~ dotacao_atual_total + dotacao_atual_invest +
                     servidores_superior + forca_trabalho, 
                   family = binomial(link = "probit"),
                   data = min_ano %>% filter(existencia ==1 & 
                                               (id_org_unica %in% nunca_part == FALSE)
                                             )
)
summary(probit_part)

#zero infl
library(pscl)
# modelo nao conseguiu estimar erro padrao sem transformar preditores
# provavel que seja um range muito grande quando comparado com o do count
zero_poisson <- zeroinfl(part_count ~ log10(1+dotacao_atual_invest) +
                           log10(1+servidores_superior),
                         data = min_ano %>% filter(existencia ==1 & 
                                                     (id_org_unica %in% nunca_part == FALSE)
                         ))
summary(zero_poisson)
