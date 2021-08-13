library(tidyverse)

total_pago %>%
# total_dot %>% 
  pivot_longer(
    cols = c(`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, 
             `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
             `2018`, `2019`,`2020`),
    names_to = "ano"
  ) mutate(
    ano = as.double(ano)
  ) %>% rename(
    orgao = `Rótulos de Linha`
    pago_total = value
    # dotacao_atual_total = value
  ) -> total_pago #total_dot


orcamento %>% filter(grupo_despesa == "4 - Investimentos") %>% 
  rename(pago_invest = pago, dotacao_atual_invest = dotacao_atual) %>% 
  select(-grupo_despesa) -> orcamento

left_join(total_dot, total_pago) %>% 
  mutate(orgao = `Rótulos de Linha`,
         ano = as.double(ano)
  ) %>% select(-`Rótulos de Linha`) %>% 
  left_join(orcamento) -> orcamento_limpo

write.csv2(orcamento_limpo, "orcamento.csv")
