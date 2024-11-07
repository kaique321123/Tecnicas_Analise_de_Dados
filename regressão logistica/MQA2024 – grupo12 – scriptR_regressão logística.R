# Carregar pacotes necessários
library(dplyr)
library(car)
library(ggplot2)
library(pscl)
library(stats)

while (!is.null(dev.list())) dev.off()


# Carregar os dados
dados <- read.csv("C://Users//KaiquedeJesusPessoaS//Desktop//analise-mqa//regressão logistica//MQA2024–grupo12–dataset_regressão_logística.csv")

# Manipulação de dados: cálculo da média e criando variáveis dummy de Cor
dados <- dados %>%
  mutate(
    media = rowMeans(select(., NotaCN, NotaCH, NotaLC, NotaMT, NotaRE), na.rm = TRUE),
    Aprovado = ifelse(media >= 525, 1, 0),
    EscolaPublica = ifelse(is.na(EscolaPublica), 0, ifelse(EscolaPublica == "S", 1, 0)),
    media_norm = scale(media),
    media_quad = media_norm^2,
    CorBranco = ifelse(Cor == "B", 1, 0),
    CorPreto = ifelse(Cor == "Pr", 1, 0),
    CorPardo = ifelse(Cor == "Pa", 1, 0)
  ) %>%
  filter(!is.na(media))

# Atribuir o dataset a outro objeto, pois aquele estava com problema para calular as estatísticas descritivas  
dt <- dados

# Ajustar o modelo de regressão logística
modelo_categ <- glm(Aprovado ~ media_quad + EscolaPublica + CorBranco + CorPreto + CorPardo, data = dados, family = binomial(link = "logit"))

# Exibir a equação do modelo com três casas decimais
print("Equação do modelo de regressão logística com variáveis indicadoras para Cor:")
print(format(summary(modelo_categ)$coefficients, digits = 3, nsmall = 3))

# Análise de Multicolinearidade - Calcular o VIF
vif_values <- vif(modelo_categ)
print("Valores de VIF para verificar a multicolinearidade:")
print(vif_values)

# Verificação da Tendência dos Resíduos
residuos <- residuals(modelo_categ, type = "deviance")
valores_ajustados <- fitted(modelo_categ)

# Calcular Odds Ratio e Intervalos de Confiança
odds_ratios_categ <- exp(coef(modelo_categ))
conf_int_categ <- exp(confint.default(modelo_categ))
print("Odds Ratios e Intervalos de Confiança:")
print(data.frame(
  OddsRatio = format(odds_ratios_categ, digits = 3, nsmall = 3),
  IC_2.5 = format(conf_int_categ[, 1], digits = 3, nsmall = 3),
  IC_97.5 = format(conf_int_categ[, 2], digits = 3, nsmall = 3)
))

# Calcular log-likelihood com três casas decimais
log_likelihood <- logLik(modelo_categ)
print(format(log_likelihood, digits = 3, nsmall = 3))

# Calcular pseudo-R² com três casas decimais
pseudo_R2 <- pR2(modelo_categ)
print("Pseudo R² de McFadden:")
print(format(pseudo_R2, digits = 3, nsmall = 3))

# Previsões e tabela de confusão
predicoes_categ <- ifelse(predict(modelo_categ, type = "response") > 0.5, 1, 0)
confusao_categ <- table(dados$Aprovado, predicoes_categ)
print("Tabela de Confusão (modelo com variáveis indicadoras para Cor):")
print(confusao_categ)

# Calcular métricas de desempenho com três casas decimais
sensibilidade_categ <- format(confusao_categ[2, 2] / sum(confusao_categ[2, ]), digits = 3, nsmall = 3)
especificidade_categ <- format(confusao_categ[1, 1] / sum(confusao_categ[1, ]), digits = 3, nsmall = 3)
acuracia_categ <- format(sum(diag(confusao_categ)) / sum(confusao_categ), digits = 3, nsmall = 3)

print(paste("Sensibilidade:", sensibilidade_categ))
print(paste("Especificidade:", especificidade_categ))
print(paste("Acurácia:", acuracia_categ))

# Tabela de Odds Ratios e Intervalos de Confiança com três casas decimais
odds_ratios_tabela <- data.frame(
  Variável = names(odds_ratios_categ),
  OddsRatio = format(odds_ratios_categ, digits = 3, nsmall = 3),
  IC_2.5 = format(conf_int_categ[, 1], digits = 3, nsmall = 3),
  IC_97.5 = format(conf_int_categ[, 2], digits = 3, nsmall = 3)
)
print("Odds Ratios e Intervalos de Confiança:")
print(odds_ratios_tabela)

# Tabela de métricas de desempenho com três casas decimais
metricas <- data.frame(
  Métrica = c("Sensibilidade", "Especificidade", "Acurácia"),
  Valor = c(sensibilidade_categ, especificidade_categ, acuracia_categ)
)
print("Métricas de Desempenho:")
print(metricas)

# Calcular os Resíduos e exibir os gráficos 
residuos <- residuals(modelo_categ, type = "deviance")
ggplot(dados, aes(x = media_quad, y = residuos)) + geom_point() + geom_smooth(se = FALSE) +
  labs(title = "Tendência dos Resíduos")

# Autocorrelação dos Resíduos com as Variáveis Independentes
acf(residuos, main = "Autocorrelação dos Resíduos")

# Gráfico de Regressão Logística
print(ggplot(dados, aes(x = media_quad, y = Aprovado)) +
        geom_point(aes(color = factor(Aprovado)), alpha = 0.5) + 
        geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue", formula = y ~ x) + 
        labs(title = "Curva de Regressão Logística",
             x = "Média Quadrática Normalizada (media_quad)",
             y = "Probabilidade de Aprovação",
             color = "Aprovado") +
        theme_minimal())

# Calcular os resíduos de deviance e os valores ajustados
residuos <- residuals(modelo_categ, type = "deviance")
valores_ajustados <- fitted(modelo_categ)

# Gráfico de Resíduos vs. Valores Ajustados 
print(ggplot(data = data.frame(ValoresAjustados = valores_ajustados, Residuos = residuos), 
             aes(x = ValoresAjustados, y = Residuos)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", color = "blue", se = FALSE) +
        labs(title = "Gráfico de Resíduos vs. Valores Ajustados",
             x = "Valores Ajustados",
             y = "Resíduos de Deviance") +
        theme_minimal()
)

# Autocorrelação dos Resíduos
ggplot(data = data.frame(residuos, valores_ajustados), aes(x = valores_ajustados, y = residuos)) +
  geom_point() +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Gráfico de Resíduos vs. Valores Ajustados", x = "Valores Ajustados", y = "Resíduos") +
  theme_minimal()

# Estatísticas descritivas após normalizar
mean_value <- mean(dt$media, na.rm = TRUE)
median_value <- median(dt$media, na.rm = TRUE)
sd_value <- sd(dt$media, na.rm = TRUE)
var_value <- var(dt$media, na.rm = TRUE)
min_value <- min(dt$media, na.rm = TRUE)
max_value <- max(dt$media, na.rm = TRUE)
quartis <- quantile(dt$media, probs = c(0.25, 0.50, 0.75, 1), na.rm = TRUE)

cat("Média:", mean_value, "\n")
cat("Mediana:", median_value, "\n")
cat("Desvio Padrão:", sd_value, "\n")
cat("Variância:", var_value, "\n")
cat("Mínimo:", min_value, "\n")
cat("Máximo:", max_value, "\n")
cat("Quartis:", quartis, "\n")