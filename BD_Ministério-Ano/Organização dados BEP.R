library(here)
library(tidyverse)

# Notas sobre os dados ----
# Quant_serv me retorna a FORÇA DE TRABALHO DO MINISTÉRIO:
# Força de trabalho = quant. de vínculo (-) cedido (-) exercício 
# Fonte dos dados: extraído das tabelas do BEP Jan 2017
# Sistema Integrado de Administração de Recursos Humanos-SIAPE (Data Warehouse).

# Arquivo apresenta as seguintes notas:
# Tabela inclui os servidores civis da Administração direta, Autarquias e Fundações do Poder Executivo (inclusive contratos temporários).																				
# Presidência - Inclui a Vice-Presidência, as Secretarias, ABIN, ANAC (Anteriormente pertencia ao Ministério da Defesa-MD/Comando da Aeronáutica-CAer, a partir de mar2012 passa-se ser vinculada a Secretaria de Aviação Civil-SAC subordinada a Presidência da República-PR), ANTAQ (Anteriormente pertencia ao Ministério dos Transportes-MT, a partir de mar2013 passa-se ser vinculada a Secretaria Especial de Porto-SEP e subordinada a Presidência da República-PR), CGU-Controladoria Geral da União (Anteriormente pertencia ao Ministério da Fazenda como Secretaria de Federal de Contrôle-SFC/MF, a partir de mai2002 passa-se chamado Corregedoria Geral da União subordinada a Presidência da República-PR e a partir de mai2003 passa-se ter alteração da sua denominação para CGU) e IPEA (Anteriormente pertencia ao Ministério do Planejamento-MP, a partir de mai2008 passa-se ser vinculada a Secretaria de Assuntos Estratégicos-SAE subordinda a Presidência da República-PR ).																				
# AGU - A Advocacia Geral da União-AGU está vinculada a Presidência da República																				
# Defesa - Inclui administração direta, Comando da Aeronáutica, Exército e Marinha.																				
# MDA - Inclui o INCRA																				
# MPOG -  Inclui ENAP, IBGE, IPEA (a partir de jan16 o IPEA deixa de ser vinculada a Secretaria de Assuntos Estratégicos-SAE subordinada a Presidência da República-PR e passa a ser vinculado ao Ministério do Planejamento, Orçamento e Gestão)																				
# Ano de 2016 - Posição de Dezembro/2016.																				
# De 1997 a 2015 foram considerados os quantitativos do mês de dezembro de cada ano.																				
# Nota 1: Nesta tabela estão sendo incluídos os servidores do Ministério da Saúde cedidos ao SUS (Lei 8270/91).																				
# Nota 2- A partir de janeiro de 2015, A Advocacia Geral da União-AGU está sendo contada dentro da Presidência da República e o Governo dos Ex-Territórios (AC, AP, RO, RR e e Antigo Estado da Guanabara) estão sendo contados dentro do Ministério do Planejamento, Orçamento e Gestão como Órgão responsável, e não mais separado. A partir de Janeiro de 2016 o IPEA deixa de ser vinculada a Secretaria de Assuntos Estratégicos-SAE subordinda a Presidência da República-PR e passa a ser vincula ao Ministério do Planejamento, Orçamento e Gestão.																				



# Leitura do arquivo ----
file_name <- "Força trabalho servidores.xlsx"
quant_serv <- readxl::read_xlsx(here("Dados originais", file_name))


# Organização dos dados ----
quant_serv <- pivot_longer(quant_serv, cols = 2:21, names_to = "ano") %>% 
  mutate(ano = as.numeric(ano)) %>% rename(forca_trabalho = value)

# Exportar# Exportar arquivo ---
write.csv2(quant_serv, "quant_serv_bep.csv")

# Adição do id-unico será manual?
