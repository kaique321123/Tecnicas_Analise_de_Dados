library(data.table)

arqMicrodados <- "Dados\\MICRODADOS_ENEM_2019.csv"
arqLimpo <- "Dados\\ENTRADA_LIMPA.csv"

main <- function() {
  # Cria uma coluna com a média das notas
  colunasNotas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")
  tabLimpa$MediaNotas <<- rowMeans(tabLimpa[, colunasNotas])
  
  #criarColRenda()
  
  # Estabelece que haverão 3 plots, 3x1
  par(mfrow = c(3,1))
  
  # Cria uma distribuição de notas de todos os indivíduos
  hist(tabLimpa$MediaNotas, main="Notas", xlab="Nota", ylab="Pessoas")
  
  # Cria dois plots para as classes de renda de todos e dos indivíduos de melhor performance.
  compararPerfRenda()
  
  # Calcula média e variância geral
  mediaGeral <- mean(tabLimpa$MediaNotas)
  varianciaGeral <- var(tabLimpa$MediaNotas)
  print(mediaGeral)
  print(varianciaGeral)
}

compararPerfRenda <- function() {
  # Faz um plot de classes de renda 
  freqRenda <- table(tabLimpa$Q006)
  barplot(freqRenda, main="Classes de Renda Geral", xlab="Classes", ylab="Indivíduos", col="blue")
  
  # Faz um plot de classes de renda para aqueles que tiraram notas apenas maiores que x
  corte <- 700
  tabTop = tabLimpa[tabLimpa$MediaNotas > corte, ]
  freqTop = table(tabToppers$Q006)
  barplot(freqTop, main="Renda dos Melhores", xlab="Classes", ylab="Indivíduos", col="red")
  
}

# Função que transforma a categoria da renda de volta ao ponto mediano da categoria
criarColRenda <- function() {
  catRendaValor = c(0, 998, 1497, 1996, 2495, 2994, 3992, 4999, 5988, 6986, 7984, 8982, 9980, 11976, 14970, 19960, -1)

  renda <- function(categoria) {
    if (categoria == "A") return(0)
    if (categoria == "Q") return(-1)

    indice <- alphaIndex(categoria)
    val1 = catRendaValor[indice - 1]
    val2 = catRendaValor[indice]
    return((val1 + val2) / 2)
  }

  # Cria uma coluna renda
  tabLimpa$renda <- sapply(tabLimpa$Q006, renda)
}

alphaIndex <- function(ch) {
  ch <- tolower(ch)
  alphabet <- letters
  index <- match(ch, alphabet)
  return (index)
}

lerEntradaLimpa <- function() {
  cat("Lendo arquivo '", arqLimpo, "' de entrada...\n")
  # Lendo o arquivo CSV separado por vírgula explicitamente
  tabLimpa <<- read.csv(arqLimpo, header = TRUE, sep = ",")
  tabLimpa <<- na.omit(tabLimpa)
  cat("Leitura completa!\n");
  return(tabLimpa)
}

# Lê o arquivo original de microdados e filtra a tabela para uma nova tabela. 
# A nova tabela terá apenas as colunas de interesse e estará limpo de valores faltantes.
filtrarEntrada <- function() {
  # Lê o arquivo com apenas as colunas desejadas
  colunas <- c("Q006",
               "NU_NOTA_CN",
               "NU_NOTA_CH",
               "NU_NOTA_LC",
               "NU_NOTA_MT",
               "NU_NOTA_REDACAO")
  dadosFiltrados <- fread(microdados, select=colunas)
  
  # Filtra as linhas com qualquer valor faltante
  dadosLimpos = na.omit(dados)
  
  # Cria uma nova tabela com os dados limpos
  write.csv(dadosLimpos, arqLimpo, row.names = FALSE)
  
  return(dadosLimpos)
}

lerEntradaLimpa()
main()


