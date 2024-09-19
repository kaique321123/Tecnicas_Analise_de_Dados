library(data.table)

arqMicrodados <- "MICRODADOS_ENEM_2019.csv"
#arqLimpo <- "ENTRADA_LIMPA.csv"
arqLimpo <- "INPUT_FILTRADO.csv"

main <- function() {
  # Cria uma coluna com a média das notas
  colunasNotas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")
  tabLimpa$MediaNotas <- rowMeans(tabLimpa[, colunasNotas])
  
  mediaGeral <- mean(tabLimpa$MediaNotas)
  varianciaGeral <- var(tabLimpa$MediaNotas)
  print(mediaGeral)
  print(varianciaGeral)
  
  # Visualizando as primeiras linhas do arquivo no RStudio
  View(tabLimpa)
}

lerEntradaLimpa <- function() {
  # Lendo o arquivo CSV separado por vírgula explicitamente
  tabLimpa <<- read.csv(arqLimpo, header = TRUE, sep = ",")
  tabLimpa <<- na.omit(tabLimpa)
}

filtrarEntrada <- function() {
  colunas <- c("Q006", "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")
  
  dadosFiltrados <- fread(microdados, select=colunas)
  dadosLimpos = na.omit(dados)
  write.csv(dadosLimpos, arqLimpo, row.names = FALSE)
}

lerEntradaLimpa()


#main()


