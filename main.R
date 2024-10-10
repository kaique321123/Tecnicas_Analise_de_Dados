library(data.table) # para fread
library(nortest) # para ad.test
library(car) # para leveneTest


#arqMicrodados <- "Dados\\MICRODADOS_ENEM_2019.csv"
arqLimpo <- "Dados\\ENTRADA_LIMPA.csv"

main <- function() {
  # Cria uma coluna com a média das notas
  colunasNotas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")
  tabLimpa$MediaNotas <<- rowMeans(tabLimpa[, colunasNotas])
  
  
  # Estabelece que haverão 3 plots, 3x1
 # par(mfrow = c(3,1))
  
  # Calcula média e variância geral
  mediaGeral <- mean(tabLimpa$MediaNotas)
  varianciaGeral <- var(tabLimpa$MediaNotas)
  
  print(paste("mediaGeral", mediaGeral))
  print(paste("varianciaGeral", varianciaGeral))
  
  print(mediaGeral)
  print(varianciaGeral)
  criaDF()
  calculaSST()
  CalculaSQE()
  CalculaQuadradosMedios()
  CalculaRazaoF()
  CalculaSQT()
  CalculaGraus()
  CalculaNormalidade()
  CalculaLevene()
}

#compararPerfRenda <- function() {
  # Faz um plot de classes de renda 
 # freqRenda <- table(tabLimpa$Q006)
#  barplot(freqRenda, main="Classes de Renda Geral", xlab="Classes", ylab="Indivíduos", col="blue")
  
  # Faz um plot de classes de renda para aqueles que tiraram notas apenas maiores que x
  #corte <- 700
  #tabTop = tabLimpa[tabLimpa$MediaNotas > corte, ]
  #freqTop = table(tabToppers$Q006)
 # barplot(freqTop, main="Renda dos Melhores", xlab="Classes", ylab="Indivíduos", col="red")
  
#}

CalculaNormalidade <- function(){
  teste_normalidade <- ad.test(tabLimpa$MediaNotas)
  
  #DADOS NÃO NORMALIZADOS
  # Exibindo o resultado do teste
  cat("Estatística do teste de Anderson-Darling:", teste_normalidade$statistic, "\n")
  cat("Valor p do teste:", teste_normalidade$p.value, "\n")
}

CalculaLevene <- function(){
  #Teste de homocedasticidade (Levene)
  # Converter Classe_Social para um fator
  tabLimpa$Classe_Social <- as.factor(tabLimpa$Classe_Social)
  
  # Realizar o teste de Levene novamente
  resultado_levene <- leveneTest(MediaNotas ~ Classe_Social, data = tabLimpa)
  
  # Exibir o resultado do teste de Levene
  print(resultado_levene)
  
  
}

criaDF <- function(){
  
  DfSemRenda <<- tabLimpa[tabLimpa$Classe_Social ==  "Sem Renda", ]
  varianciaGeralSemRenda <- var(DfSemRenda$MediaNotas)
  print(paste("varianciaGeralSemRenda", varianciaGeralSemRenda))
  
  DfBaixa<<-tabLimpa[tabLimpa$Classe_Social ==  "Baixa", ]
  varianciaGeralBaixa <- var(DfBaixa$MediaNotas)
  print(paste("varianciaGeralBaixa", varianciaGeralBaixa))
  
  DfMédia<<-tabLimpa[tabLimpa$Classe_Social ==  "Média", ]  
  varianciaGeralDfMedia <- var(DfMédia$MediaNotas)
  print(paste("varianciaGeralDfMedia", varianciaGeralDfMedia))
  
  DfMédia_Alta<<-tabLimpa[tabLimpa$Classe_Social ==  "Média Alta", ]
  varianciaGeralMédia_Alta<- var(DfMédia_Alta$MediaNotas)
  print(paste("varianciaGeralMédia_Alta", varianciaGeralMédia_Alta))
  
  DfAlta<<-tabLimpa[tabLimpa$Classe_Social ==  "Alta", ]
  varianciaGeralAlta <- var(DfAlta$MediaNotas)
  print(paste("varianciaGeralAlta", varianciaGeralAlta))
  
}


calculaSST <- function(){
  media_geral <- mean(tabLimpa$MediaNotas)
  
}
calculaSST <- function(){
  
  # Calcular a média geral da coluna 'MediaNotas'
  media_geral <- mean(tabLimpa$MediaNotas)
  
  # Calcular a individual da coluna 'MediaNotas'
  media_geralSR <- mean(DfSemRenda$MediaNotas)
  media_geralB  <- mean(DfBaixa$MediaNotas)
  media_geralM  <- mean(DfMédia$MediaNotas)
  media_geralMA  <- mean(DfMédia_Alta$MediaNotas)
  media_geralA  <- mean(DfAlta$MediaNotas)
  
  print(paste("media_geralSR", media_geralSR))
  
  
  # Calcular a Soma de Quadrados Tratamento (SQT)
  #SQTSemRenda <- 158088*(sum((DfSemRenda$MediaNotas - media_geral)^2))
  #SQTBaixa <- 2500145*(sum((DfBaixa$MediaNotas - media_geral)^2))
  #SQTMédia <- 745958*(sum((DfMédia$MediaNotas - media_geral)^2))
  #SQTMédia_Alta <- 214973*(sum((DfMédia_Alta$MediaNotas - media_geral)^2))
  #SQTAlta <- 82745*(sum((DfAlta$MediaNotas - media_geral)^2))
  
  SQTSemRenda <- 158088*(((media_geralSR - media_geral)^2))
  SQTBaixa <- 2500145*(((media_geralB - media_geral)^2))
  SQTMédia <- 745958*(((media_geralM - media_geral)^2))
  SQTMédia_Alta <- 214973*(((media_geralMA - media_geral)^2))
  SQTAlta <- 82745*(((media_geralA - media_geral)^2))
  
  
  SQTotalTratamento <<- (SQTSemRenda+ SQTBaixa + SQTMédia + SQTMédia_Alta + SQTAlta)
  #print(SQTotal)
  print(paste("Soma de Quadrados Total Entre Tratamentos(SQT):", SQTotalTratamento))
  glT <<- (5 -1)
  print(paste("(glT)):", glT))
  
}

CalculaSQE <- function(){
  
  # Calcular a individual da coluna 'MediaNotas'
  media_geralSR <- mean(DfSemRenda$MediaNotas)
  media_geralB  <- mean(DfBaixa$MediaNotas)
  media_geralM  <- mean(DfMédia$MediaNotas)
  media_geralMA  <- mean(DfMédia_Alta$MediaNotas)
  media_geralA  <- mean(DfAlta$MediaNotas)
  
  # Calcular a Soma de Quadrados do Erro (SQE)
  SQESemRenda <- (sum((DfSemRenda$MediaNotas - media_geralSR)^2))
  SQEBaixa <- (sum((DfBaixa$MediaNotas - media_geralB)^2))
  SQEMédia <- (sum((DfMédia$MediaNotas - media_geralM)^2))
  SQEMédia_Alta <- (sum((DfMédia_Alta$MediaNotas - media_geralMA)^2))
  SQEAlta <- (sum((DfAlta$MediaNotas - media_geralA)^2))
  
  SQTotalErro <<- (SQESemRenda+ SQEBaixa + SQEMédia + SQEMédia_Alta + SQEAlta)
  #print(SQTotalErro)
  print(paste("Soma de Quadrados Erro Total (SQE):", SQTotalErro))
  glE <<- ((nrow(tabLimpa)) - 5)
  print(paste("(glE)):", glE))
  
}

CalculaQuadradosMedios <- function(){
  
  QMTratamento <<- SQTotalTratamento/glT
  print(paste("(QMT):", QMTratamento))
  
  QMErro <<- SQTotalErro/glE
  print(paste("(QME):", QMErro))
  
}

CalculaRazaoF <- function(){
  
  RazaoF <<- QMTratamento/QMErro
  print(paste("(Razão F):", RazaoF))
  
}

CalculaSQT <- function(){
 
  media_geral <- mean(tabLimpa$MediaNotas)
  
  # Calcular a Soma de Quadrados Total (SQT)
  SQTSemRenda <<- (sum((DfSemRenda$MediaNotas - media_geral)^2))
  SQTBaixa <<- (sum((DfBaixa$MediaNotas - media_geral)^2))
  SQTMédia <<- (sum((DfMédia$MediaNotas - media_geral)^2))
  SQTMédia_Alta <<- (sum((DfMédia_Alta$MediaNotas - media_geral)^2))
  SQTAlta <<- (sum((DfAlta$MediaNotas - media_geral)^2))
  
  SQTotal <<- SQTSemRenda + SQTBaixa + SQTMédia + SQTMédia_Alta + SQTAlta
  print(paste("Soma de Quadrados Totais (SQT):", SQTotal))
  
  glTotal <<- ((nrow(tabLimpa)) - 1)
  print(paste("(glTotal):", glTotal))
  
}

CalculaGraus <- function(){
  alfa <- 0.05
  glT <- 4
  glE <- 3701904
  f_critico <- qf(1 - alfa, glT, glE)
  print(paste("(f_critico):", f_critico))
  
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

#lerEntradaLimpa()
main()


