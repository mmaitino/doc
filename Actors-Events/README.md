# Banco de dados: Atores e eventos da política externa ambiental brasileira

**Autor:** Martin Egon Maitino  
**Financiamento:** FAPESP (Processos 2020/07387-1 e 2022/10926-7)

## Publicações relacionadas
* Maitino, Martin Egon. **"Participação e política externa ambiental brasileira (1970-2018): atores domésticos e a definição da posição do Brasil"**, Tese (Doutorado em Ciência Política), Universidade de São Paulo, 2025. [Link para a tese]

---

## Descrição geral do banco de dados
O banco de dados de Atores e Eventos da Política Externa Ambiental brasileira compila os indivíduos e organizações participantes das delegações oficiais do Brasil a eventos multilaterais ambientais. Oferece instrumentos para a mensuração dos padrões de participação e interação de atores domésticos nesses fóruns.

O banco consolidado é composto por dois grupos de arquivos:
1. **Listas de referência:** Eventos (`eventos.csv`) e participantes (`delegs.csv`).
2. **Redes de atores:** Projetadas a partir das listas anteriores em nível de indivíduos e organizações.

### Metodologia de Tratamento e Limpeza
Devido à variação na grafia dos nomes originais, os dados passaram por fases de limpeza:
* **Organizações:** Padronização realizada manualmente.
* **Indivíduos:** Padronização realizada com auxílio do software *Google Refine*, aplicando métodos de clusterização e algoritmos de limpeza de forma supervisionada.
* **Tipologias:** As organizações foram classificadas seguindo a documentação de Farias e Carmo (2020) para viabilizar comparações futuras.

#### Quadro 1. Tipos de organização identificados

| Categoria detalhada | Categoria reduzida | Categorias Farias e Carmo |
| :--- | :--- | :--- |
| Administração federal direta (sem MRE) e autarquias | Governo federal não-MRE | Administração federal direta (sem MRE) e autarquias |
| Empresas públicas, estatais e bancos federais | Governo federal não-MRE | Estatais e bancos federais (inclusive BNDES) |
| Forças Armadas | Governo federal não-MRE | - |
| Ministério das Relações Exteriores | Governo federal MRE | Ministério das Relações Exteriores |
| Governos estaduais e municipais | Governos subnacionais | Governos estaduais e municipais |
| Empresas, associações setoriais e patronais | Setor empresarial | Empresários, federações e confederações |
| ONGs e Movimentos Sociais | Sociedade civil/Sindicatos | - |
| Universidades, escolas e sociedades científicas | Órgãos de ensino e pesquisa | - |
| Sistema S | Órgãos de ensino e pesquisa | - |
| Organismos internacionais | Outro | Organismos internacionais |
| Judiciário, AGU e Ministério Público | Outro | Judiciário |

---

## Arquivos e Dicionário de Variáveis

### 1. Lista de participantes (`delegs.csv`)
*   **id_indevento:** ID numérico da participação.
*   **titulo:** Título do indivíduo (Mr., Ms., Dr., H.E.).
*   **nome:** Nome (não padronizado) do participante.
*   **desc:** Descrição conforme fonte original.
*   **cargo_deleg:** Cargo na delegação (ex: Líder de delegação).
*   **org / org_detalhe:** Organização e subdivisão vinculada.
*   **cargo_org:** Cargo na instituição de origem.
*   **conf / pais / fonte:** Sigla do evento, país da delegação e fonte do dado.
*   **id_org_dupla:** ID para harmonização institucional.

### 2. Lista de eventos (`eventos.csv`)
*   **Nome do evento:** ID textual (ex: "CBD, COP06").
*   **Conference:** Nome por extenso da Convenção.
*   **Tema:** Classificação temática.
*   **Data / Local / Locale:** Detalhes geográficos e temporais.
*   **Tipo evento:** Categoria (COP, Encontro preparatório, etc.).
*   **Coleta:** Resultado da obtenção das listas de participantes.

### 3. Redes de Indivíduos (`indnet9018_nodes.csv` / `_edges.csv`)
*   **Nodes:** Contém `id_individuo`, `nome_padrao`, `org_limpo` e métricas de participação.
*   **Edges:** Contém `from`, `to`, `weight` (co-participações) e dados contextuais do evento (`tema`, `ano`).

### 4. Redes de Organizações (`orgnet9018_nodes.csv` / `_edges.csv`)
*   **Nodes:** Contém `org_limpo`, `id_org_unica` e classificações de tipo de organização.
*   **Edges:** Representa a co-presença institucional em eventos, com pesos baseados na frequência de interações.

---
**Nota:** As planilhas "dicionário" de tratamento não foram incluídas na base pública por questões de volume, mas podem ser solicitadas diretamente ao autor.