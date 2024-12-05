# Técnicas de Análise de Dados

Este repositório contém exemplos e implementações de técnicas de análise de dados utilizando **R**. As análises foram realizadas com base em dois conjuntos de dados principais:  

- **Resultados do ENEM 2019 (pré-pandemia)**: explorados para análises estatísticas e de modelagem preditiva.  
- **Questionário de satisfação de uma companhia aérea dos Estados Unidos**: utilizado para análise fatorial.

## Técnicas Utilizadas  

### 1. ANOVA  
Análise de Variância (ANOVA) foi empregada para investigar se as médias das notas em diferentes grupos (por exemplo, região, tipo de escola, ou faixa de renda) apresentam diferenças estatisticamente significativas.  

### 2. Clusterização  
Métodos de agrupamento (K-means e Hierarchical Clustering) foram aplicados aos dados do ENEM 2019 para identificar padrões de similaridade entre os participantes com base em variáveis como desempenho nas provas, renda familiar e localização geográfica.

### 3. Regressão Simples  
A Regressão Linear Simples foi usada para prever as notas dos candidatos em uma prova específica (como Matemática) com base em variáveis como escolaridade dos pais, tipo de escola (pública ou privada), e desempenho em outras áreas.

### 4. Regressão Logística  
Utilizada para prever a probabilidade de um candidato ser aprovado (nota acima de um certo limiar) com base em variáveis categóricas e numéricas, como tipo de escola e renda familiar.

### 5. Análise Fatorial  
A Análise Fatorial foi aplicada ao questionário de satisfação de uma companhia aérea nos Estados Unidos. Essa técnica reduziu as várias perguntas do questionário a um conjunto menor de fatores principais que representam dimensões subjacentes de satisfação do cliente, como "qualidade do serviço" e "experiência com o voo".

## Dados  

### ENEM 2019  
Os dados divulgados pelo INEP sobre o ENEM de 2019 foram utilizados para a maioria das análises (ANOVA, clusterização, regressão múltipla e regressão logística). Esse conjunto de dados oferece informações detalhadas sobre os participantes, incluindo notas nas provas, características demográficas e socioeconômicas.  

### Questionário de Satisfação  
Os dados do questionário foram obtidos de um estudo sobre a satisfação dos passageiros de uma companhia aérea nos Estados Unidos. Ele inclui respostas a perguntas relacionadas ao atendimento, conforto e outros aspectos da experiência de voo.  

## Requisitos  

Para reproduzir as análises, você precisará ter o R instalado em seu sistema, juntamente com os pacotes listados nos scripts. 

## Estrutura do Repositório  

- `scripts/`  
  Contém os scripts para cada técnica de análise.  

  - `ANOVA.R`: Script para análise de variância.  
  - `CLUSTERIZACAO.R`: Script para clusterização.  
  - `REGRESSAO_SIMPLES.R`: Script para regressão simples.  
  - `REGRESSAO_LOGISTICA.R`: Script para regressão logística.  
  - `ANALISE_FATORIAL.R`: Script para análise fatorial.  

- `Dados/`  
  Contém os conjuntos de dados utilizados nas análises.  

- `Dicionário_Microdados_Enem_2019/`\
  Apresenta uma tradução do que significa as questões que possuem apenas código e o significado das siglas.

- `Link do Arquivo do ENEM oficial do Site do INEP`  
  Não foi possível subir o arquivo oficial do INEP devido ao seu tamanho, mas está nesse link, só é necessário para rodar a técnica ANOVA
  ````
  https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/enem
  ````
  
## Como Executar

1. Clone este repositório:  
   ```bash
   git clone https://github.com/kaique321123/Tecnicas_Analise_de_Dados.git
   cd Tecnicas_Analise_de_Dados
