library(data.table)
library(nortest)
library(car)

varDependente <<- 'NotaRE'
#varIndependentes <<- list('NotaMT', 'NotaLC', 'NotaCH', 'NotaCN', 'Sexo', 'EscolaPublica', 'Cor')
varIndependentes <<- list('NotaMT', 'NotaLC', 'NotaCH', 'NotaCN')
plotVars <<- list(
  list('NotaMT', 'Matemática e Tecnologias', '#38e'),
  list('NotaLC', 'Linguagens e Códigos',     '#f44'),
  list('NotaCH', 'Ciências e Humanidades',   '#ea0'),
  list('NotaCN', 'Ciências da Natureza',     '#1d1')
)

# Remove todas as linhas no dataframe onde a coluna com o dado nome possui outliers
removerOutliersDe <- function(df, variavel) {
  vetor = df[[variavel]]
  q1 <- quantile(vetor, 0.25)
  q3 <- quantile(vetor, 0.75)
  amplitude = q3 - q1
  inferior <- q1 - 1.5 * amplitude
  superior <- q3 + 1.5 * amplitude
  
  result = df[df[[variavel]] >= inferior & df[[variavel]] <= superior, ]
  return(result)
}

# Remove os outliers do dataframe para as variáveis independentes contínuas
filtrarOutliers <- function() {
  df = amostras
  df = removerOutliersDe(df, 'NotaMT')
  df = removerOutliersDe(df, 'NotaLC')
  df = removerOutliersDe(df, 'NotaCH')
  df = removerOutliersDe(df, 'NotaCN')
  amostrasLimpas <<- df
}

# Executa uma regressão simples para cada variável independente
regressaoSimples <- function() {
  df = amostrasLimpas
  regressoes <<- list()
  
  for (i in seq_along(varIndependentes)) {
    # Invoca o modelo de regressão simples Y ~ X, onde Y é a variável dependente escolhida, e
    # X uma das variáveis independentes
    vaIndependente = varIndependentes[[i]]
    formula = as.formula(paste(varDependente, '~', vaIndependente))
    modelo = lm(formula, data = df)
    
    # Salva o modelo de regressão globalmente
    regressoes[[i]] <<- modelo
    
    # Obtém informações sobre o modelo
    sumario = summary(modelo)
    
    # Imprime informações da regressão
    message("\n::", varDependente, '~', vaIndependente)
    
    coeffs = sumario$coefficients
    f_estat = sumario$fstatistic
    p_valor = pf(f_estat[1], f_estat[2], f_estat[3], lower.tail = FALSE)
    
    print(coeffs)
    cat("R²:",sumario$r.squared)
    cat(" --- F-estat:", f_estat[1])
    cat("\nσ:", sumario$sigma)
    cat(' --- p-valor:', p_valor)
    cat('\n')
  }
}

# Gera N gráficos de dispersão com a linha de regressão superimposta em cada gráfico
plotRegressaoSimples <- function() {
  df = amostrasLimpas
  
  par(mfrow = c(2, 2))
  
  for (struc in plotVars) {
    pVar = struc[[1]]
    nome = struc[[2]]
    cor  = struc[[3]]
    index = which(varIndependentes == pVar)
    
    regr = regressoes[[index]]
    
    # Plotar a regressão
    plot(df[[pVar]], df$NotaRE, main=nome, xlab=pVar, ylab='Nota em Redação', pch=20, col=cor)
    abline(regr, col='black', lwd = 3)
  }
}

# Gera N gráficos de dispersão de resíduo para cada regressão de variável contínua
plotResiduos <- function() {
  df = amostrasLimpas
  
  par(mfrow = c(2, 2))
  
  for (struc in plotVars) {
    pVar = struc[[1]]
    nome = struc[[2]]
    cor  = struc[[3]]
    index = which(varIndependentes == pVar)
    
    regr = regressoes[[index]]
    fits = fitted(regr)
    residuos = residuals(regr)
    
    # Plotar os resíduos da regressão
    plot(fits, residuos, main=nome, pch=20, col=cor)
    abline(h = 0, col='black', lwd=2, lty=2)
  }
}

# Executa uma regressão múltipla com todas as variáveis independentes
regressaoMultipla <- function() {
  df = amostrasLimpas
  
  vars = paste(varIndependentes, collapse=' + ')
  formula = paste(varDependente, '~', vars)
  modelo = lm(formula, data = df)
  sumario = summary(modelo)
  
  print(sumario)
}

#filtrarOutliers()
regressaoSimples()
#plotRegressaoSimples()
#plotResiduos()
#regressaoMultipla()