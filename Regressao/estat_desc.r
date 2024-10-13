estatTabelasEMedidas <- function() {
  df = tabLimpa
  
  message("--- Tabelas de Distribuição de Frequência / Tendência Central / Dispersão")
  cat(":: Nota de Redação")
  cat('\nMédia:', mean(amostras$NotaRE))
  cat('\nD.P.:', sqrt(var(amostras$NotaRE)))
  print(table(cut(amostras$NotaRE, breaks = c(0, 200, 400, 600, 800, 1000), right = TRUE, include.lowest = TRUE)))
  
  cat("\n:: Nota de Linguagens e Códigos")
  cat('\nMédia:', mean(amostras$NotaLC))
  cat('\nD.P.:', sqrt(var(amostras$NotaLC)))
  print(table(cut(amostras$NotaLC,      breaks = c(200, 400, 600, 800), right = TRUE, include.lowest = TRUE)))
  
  cat("\n:: Sexo")
  print(table(amostras$Sexo))
  
  cat("\n:: Escola Publica (S/N)")
  print(table(amostras$EscolaPublica))
  
  # Desenho dos plots
  par(mfrow = c(2, 2))
  
  labels = c("Pública", "Privada");
  colors = c("#3F3", "#FF4");
  df$EscolaPublica <- factor(df$EscolaPublica, levels = c('S', 'N'))
  boxplot(NotaRE ~ EscolaPublica, data = df, xlab="Tipo de Escola", ylab="Nota de Redação", col = colors, names = labels)  
  
  labels = c("Feminino", "Masculino");
  colors = c("#28F", "#8F3");
  df$Sexo <- factor(df$Sexo, levels = c('F', 'M'))
  boxplot(NotaRE ~ Sexo, data = df, xlab="Sexo", ylab="Nota de Redação", col = colors, names = labels)  
  
  labels = c("Branca", "Preta", "Parda", "Amarela");
  colors = c("#DDD", "#AAA", "#888", "#666");
  df$Cor <- factor(df$Cor, levels = c('B', 'Pr', 'Pa', 'A'))
  boxplot(NotaRE ~ Cor, data = df, xlab="Cor/Raça", ylab="Notas", col = colors, names = labels)  
}

estatTabelasEMedidas()
