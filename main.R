library(data.table)

arqMicrodados <- "Dados\\MICRODADOS_ENEM_2019.csv"
arqLimpo <- "Dados\\ENTRADA_LIMPA.csv"
classesRenda <- c('nula', 'baixa', 'media', 'media_alta', 'alta')

main <- function() {
	#calcularMediaFinal()
	#categorizarRenda()
  calcTendenciasCentrais()
  criarIntervalosDeNotas()
  criarBoxPlots()
  
  calcMedidasClasses()  
  calcQuadrados()
  testeNormalidade()
  calcTesteF()
}

# Calcula medidas de tendência central
calcTendenciasCentrais <- function() {
  geralN <<- nrow(tabLimpa)
  geralMedia <<- mean(tabLimpa$NotaFinal)
  geralVariancia <<- var(tabLimpa$NotaFinal)
  geralMediana = median(tabLimpa$NotaFinal)
  
  cat("Total:", geralN, "\n")
  cat("Média:", geralMedia, "\n")
  cat("Variância:", geralVariancia, "\n")
  cat("Desvio Padrão:", sqrt(geralVariancia), "\n")
  cat("Mediana: ", geralMediana, "\n")
}

# Executa o teste da estatística F da ANOVA
calcTesteF <- function() {
  # Calcula a estatística F e o valor crítico
  alfa <- 0.05
  fCalc = qmTrat / qmErro
  fCrit <- qf(1 - alfa, glTrat, glErro)
  
  cat("FCalc:", fCalc, "\n")
  cat("FCrit:", fCrit, "\n")
  
  if (fCalc < fCrit) {
    print("Aceitamos H0!")
  } else {
    print("Rejeitamos H0!")
  }
}

testeLevene <- function () {}

# Teste de aderência a distribuição Normal
testeNormalidade <- function() {
  cat("\n--- Teste de Normalidade ---\n")
  
  # Executa o teste de Anderson-Darling nos dados para verificar se seguem uma distribuição normal.
  alfa = 0.05
  res = ad.test(tabLimpa$NotaFinal)
  pValor = res$p.value
  
  cat("Estatística", res$statistic, "\n")
  cat("P-Valor", pValor, "\n")
  
  if (pValor > alfa) {
    cat("As notas seguem uma distribuição normal!")
  } else {
    cat("As notas NÃO seguem uma distribuição normal!")
  }
}

# Calcula todas as somas de quadrados das classes de renda
calcQuadrados <- function() {
  nGrupos = length(classesRenda)
  
  # Calcula as somas de quadrados nos três níveis
  sqTotal = sum(tabClasses$C_SQTotal)
  glTotal = geralN - 1
  
  sqTrat = sum(tabClasses$C_SQTrat)
  glTrat <<- nrow(tabClasses) - 1
  
  sqErro = sum(tabClasses$C_SQErro)
  glErro <<- geralN - nGrupos
  
  cat("SQTot:", sqTotal, "\n")
  cat("SQTrat:", sqTrat, "\n")
  cat("SQErro:", sqErro, "\n")
  
  cat("glTrat: ", glTrat, "\n")
  cat("glErro: ", glErro, "\n")
  cat("glTotal: ", glTotal, "\n")
  
  # Calcula os Quadrados Médios
  qmTrat <<- sqTrat / glTotal
  qmErro <<- sqErro / glErro
  
  cat("QM_Trat:", qmTrat, "\n")
  cat("QM_Erro:", qmErro, "\n")
}

criarIntervalosDeNotas <- function() {
  cat('\n--- Intervalos de Classe ---')
  intervalos = cut(tabLimpa$NotaFinal, breaks = c(0, 200, 400, 600, 800, 1000), right = TRUE, include.lowest = TRUE)
  tabela = table(intervalos)
  print(tabela)
}

calcMedidasClasses <- function() {
  # Cria tabela de classes
  tabClasses <<- data.frame(
    nome = classesRenda,
    stringsAsFactors = FALSE
  )
  
  # Para cada classe de renda, execute a função abaixo
  dados = lapply(classesRenda, function (classe) {
    # Extrai todas as notas finais dos alunos com a determinada classe de renda
    df = tabLimpa[tabLimpa$ClasseRenda == classe, "NotaFinal", drop=FALSE]
    
    # Calcula o tamanho, a média e variância de todas as notas
    N = nrow(df)
    media = mean(df$NotaFinal)
    variancia = var(df$NotaFinal)
    
    # Calcula os componentes das somas de quadrados
    C_SQTotal = sum((df$NotaFinal - geralMedia)^2)
    C_SQTrat = N * (media - geralMedia) ^ 2
    C_SQErro = sum((df$NotaFinal - media)^2)
    
    return( c(N, media, variancia, C_SQTotal, C_SQTrat, C_SQErro) )
  })
  
  # Insere como colunas todos os valoes
  tabClasses$N <<- sapply(dados, function(v) v[1])
  tabClasses$Media <<- sapply(dados, function(v) v[2])
  tabClasses$Variancia <<-sapply(dados, function(v) v[3])
  tabClasses$C_SQTotal <<- sapply(dados, function(v) v[4])
  tabClasses$C_SQTrat <<- sapply(dados, function(v) v[5])
  tabClasses$C_SQErro <<- sapply(dados, function(v) v[6])
}

criarBoxPlots <- function() {
  par(mfrow = c(1, 1))
  histogramaGlobal = TRUE
  plotsClasses = FALSE
  
  if (histogramaGlobal) {
    
    # Create the histogram with counts on the y-axis
    hist(tabLimpa$NotaFinal, breaks = 30, plot = TRUE, col = "lightblue", border = "blue",
                      main = "",
                      xlab = "Notas", ylab = "Pessoas")
    axis(1, at = seq(0, 1000, by = 50))  # Adds ticks at every 2 units
    axis(2, at = seq(0, 10e+05, by = 1e+05))  # Adds ticks at every 2 units
    
    hist_data <- hist(tabLimpa$NotaFinal, breaks = 300, plot = FALSE)
    
    # Add a frequency polygon using the counts
    lines(hist_data$mids, hist_data$counts * 10, type = "l", col = "red", lwd = 2)  # Frequency p
  }
  
  if (plotsClasses) {
    plots <- list(
      list("nula", "Renda Nula", "#F00"),
      list("baixa", "Renda Baixa", "#F80"),
      list("media", " Renda Média", "#FF0"),
      list("media_alta", "Renda Média-Alta", "#AF0"),
      list("alta", "Renda Alta", "#0F0")
    )
    
    labels = c("Nula", "Baixa", "Média", "Média-Alta", "Alta");
    colors = c("#F00", "#F80", "#FF0", "#AF0", "#0F0");
    
    tabLimpa$ClasseRenda <- factor(tabLimpa$ClasseRenda, levels = c('nula', 'baixa', 'media', 'media_alta', 'alta'))
    boxplot(tabLimpa$NotaFinal ~ tabLimpa$ClasseRenda, xlab="Classe de Renda", ylab="Notas", col = colors, names = labels)  
  }

  # for (plot in plots) {
  #   cat = plot[[1]]
  #   titulo = plot[[2]]
  #   cor = plot[[3]]
  #   
  #   tabRendaCat <- tabLimpa[tabLimpa$ClasseRenda == cat, "NotaFinal", drop = FALSE]
  #   boxplot(tabRendaCat$NotaFinal, main=titulo, ylab="Notas", col=cor)
  # }
}

# Cria uma nova coluna com a categoria de renda de cada pessoa, baseada na resposta do questionário
# socioeconômico (Q006)
categorizarRenda <- function() {
  print("Categorizando as rendas...")
  
  categorias <- list(
    list("nula",       c("A")),
    list("baixa",      c("B", "C", "D", "E")),
    list("media",      c("F", "G", "H", "I", "J")),
    list("media_alta", c("K", "L", "M", "N", "O")),
    list("alta",       c("P", "Q"))
  )
  
  catRenda <- function(quest) {
    for (catArr in categorias) {
      if (quest %in% catArr[[2]]) {
        return(catArr[[1]])
      }
    }
    
    stop("Resposta inválida ao questionário.")
  }
  
  tabLimpa$ClasseRenda <<- sapply(tabLimpa$Q006, catRenda)  
  
  print("Renda categorizada!")
}

calcularMediaFinal <- function() {
  # Cria uma coluna com a média final da nota de cada aluno
  colunasNotas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")
  tabLimpa$NotaFinal <<- rowMeans(tabLimpa[, colunasNotas])
}

# Lê o arquivo original de microdados e filtra a tabela para uma nova tabela. 
# A nova tabela terá apenas as colunas de interesse e estará livre de valores nulos
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

lerEntradaLimpa <- function() {
  cat("Lendo arquivo '", arqLimpo, "' de entrada...\n")
  
  # Lendo o arquivo CSV separado por vírgula explicitamente
  tabLimpa <<- read.csv(arqLimpo, header = TRUE, sep = ",")
  tabLimpa <<- na.omit(tabLimpa)
  
  cat("Leitura completa!\n");
  return(tabLimpa)
}

#lerEntradaLimpa()
main()


