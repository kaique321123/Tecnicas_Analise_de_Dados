library(data.table)
library(nortest)
library(car)

arqMicrodados <- "Dados\\MICRODADOS_ENEM_2019.csv"
arqLimpo <- "Dados\\ENTRADA_REGRESSAO.csv"

main <- function() {
  #filtrarEntrada()
  #amostrar()
  #emitirDataset(amostras, arqLimpo)
  init()
  amostrar()
}

# Leitura e inicialização do dataset inicial. Essas funções são pesadas, mas podem ser executadas
# apenas uma vez antes de trabalhar com o dataset.
init <- function() {
  lerEntradaLimpa()
}

amostrar <- function() {
  # Obtém N amostras
  set.seed(1)
  n <- 10000
  amostras <<- tabLimpa[sample(1:nrow(tabLimpa), n), ]
  
  cat("Amostragem completa. Extraídas", n, "amostras.");
}

emitirDataset <- function(dataset, arquivo) {
  # Cria uma nova tabela com os dados limpos
  cat("Criando novo arquivo", arqLimpo, "\n")
  write.csv(dataset, arqLimpo, row.names = FALSE)
}

lerEntradaLimpa <- function() {
  cat("Lendo arquivo '", arqLimpo, "' de entrada...\n")
  
  # Lendo o arquivo CSV separado por vírgula explicitamente
  tabLimpa <<- read.csv(arqLimpo, header = TRUE, sep = ",")
  tabLimpa$EscolaPublica <- factor(tabLimpa$EscolaPublica, levels = c('N', 'S'))
  #View(tabLimpa)
  
  cat("Leitura completa!", nrow(tabLimpa), "entradas. \n");
}

# Lê o arquivo original de microdados e filtra a tabela para uma nova tabela. 
# A nova tabela terá apenas as colunas de interesse e estará livre de valores nulos
filtrarEntrada <- function() {
  cat("--- Filtrando Dataset ---\n")
  
  # Lê o arquivo com apenas as colunas desejadas
  colunas <- c("NU_NOTA_CN",
               "NU_NOTA_CH",
               "NU_NOTA_LC",
               "NU_NOTA_MT",
               "NU_NOTA_REDACAO",
               "TP_SEXO",
               "TP_ESCOLA",
               "TP_COR_RACA"
               )
  df <- fread(arqMicrodados, select=colunas)
  cat("Fonte:", nrow(df), "\n")
  
  # Filtra as linhas com qualquer valor faltante
  df = na.omit(df)
  cat("SEM NA:", nrow(df), "\n")
  
  # Omite valores zerados
  df = df[apply(df!=0, 1, all),]
  cat("SEM 0:", nrow(df), "\n")
  
  # Deixar apenas linhas de participantes com escolaridade declarada
  df <- df[df$TP_ESCOLA != 1]
  cat("COM ESCOLARIDADE: ", nrow(df), "\n")
  
  # Renomeia colunas
  names(df)[names(df) == 'TP_SEXO'] = 'Sexo'
  names(df)[names(df) == 'TP_COR_RACA'] = 'Cor'
  names(df)[names(df) == 'TP_ESCOLA'] = 'EscolaPublica'
  names(df)[names(df) == 'NU_NOTA_CN'] = 'NotaCN'
  names(df)[names(df) == 'NU_NOTA_CH'] = 'NotaCH'
  names(df)[names(df) == 'NU_NOTA_LC'] = 'NotaLC'
  names(df)[names(df) == 'NU_NOTA_MT'] = 'NotaMT'
  names(df)[names(df) == 'NU_NOTA_REDACAO'] = 'NotaRE'
  
  # Deixar apenas linhas de participantes com escolaridade declarada
  df <- df[df$Cor >= 1 & df$Cor <= 4]
  cat("APENAS CORES DECLARADAS: ", nrow(df), "\n")
  
  # Troca os valores numéricos por letras
  df$EscolaPublica <- ifelse(df$EscolaPublica == 2, "S", "N")
  
  df$Cor <- replace(df$Cor, df$Cor == 1, "B")
  df$Cor <- replace(df$Cor, df$Cor == 2, "Pa")
  df$Cor <- replace(df$Cor, df$Cor == 3, "Pr")
  df$Cor <- replace(df$Cor, df$Cor == 4, "A")
  
  cat("Concluído!\n")
  tabLimpa <<- df
}

main()