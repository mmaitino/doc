# Descrição geral
Repositório criado para manter o histórico e backup das alterações das bases de dados construídas para a pesquisa "Pluralização da política externa ambiental brasileira".



**Financiamento**: FAPESP (Processos 	2020/07387-1 e 2022/10926-7)

**Publicações resultantes**: 
- Maitino, Martin Egon. "Participação e política externa ambiental brasileira (1970-2018): atores domésticos e a definição da posição do Brasil", Tese (Doutorado em Ciência Política), Universidade de São Paulo, 2025. [Link](https://doi.org/10.11606/T.8.2025.tde-29052025-150405)

# Estrutura do repositório e dos dados
O repositório está centrado em dois subprojetos, referentes à organização e análise do banco de dados original *Atores e eventos da política externa ambiental brasileira (1970-2018)*: _BD_deleg-evento_ e _rede_parts_. 

- O primeiro projeto (_BD_deleg-evento_) inclui scripts usados na organização básica do banco de dados (incluindo sua atualização após a coleta de novos eventos e correção de erros) e a análise básica desses dados, realizada por meio do cálculo de estatísticas descritivas e gráficos simples. 
- O segundo projeto (_rede_parts_) dedica-se à análise desses dados como redes sociais. O projeto engloba a construção dos dados como rede bipartite, sua projeção em duas redes (rede de eventos e rede de participantes/organizações) e a análise descritiva dessas redes.

São incluídos, ainda, outros projetos paralelos que não constam da tese final. 

- O projeto _BD_Ministério-Ano_ foi construído após a coleta de dados históricos sobre a presença de estruturas dedicadas a relações internacionais (e.g., assessorias internacionais) nos ministérios brasileiros, combinando esses dados aos dados do banco de *Atores e Eventos*. 

- O projeto _ENB_, não finalizado, traz o webscraping de relatórios do Earth Negotiations Bulletin, publicado pelo IISD, tendo como objetivo expandir o banco de dados de Paula Castro sobre as interações de países na UNFCCC. 

- O projeto _webscraping_, também interrompido, trabalhava com a ideia de construir redes a partir da participação comum de atores em Side Events nas COPs. Por fim, o projeto _extradbs_ agrupa bancos de dados produzidos por outros pesquisadores que foram considerados úteis para a análise da tese - de particular interesse aqui são as análises construídas a partir do banco de dados de Paula Castro, como os scores de cooperação e o cálculo de pontos ideais nas interações da UNFCCC.

## Diagrama das relações entre os bancos de dados
![Diagrama](modelos_db.png?raw=true)

# Processo de construção dos bancos
Para mais informações, ver [Maitino (2025)](https://doi.org/10.11606/T.8.2025.tde-29052025-150405), Cap. 2
