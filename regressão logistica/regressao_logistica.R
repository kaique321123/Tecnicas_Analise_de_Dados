# Carregar pacotes necessários
library(dplyr)
library(car)
library(ggplot2)
library(pscl)

# Carregar os dados
dados <- read.csv("C:\\Users\\KaiquedeJesusPessoaS\\Desktop\\analise-mqa\\regressão logistica\\ENTRADA_REGRESSAO.csv")


# Verificar os valores únicos na coluna EscolaPublica
unique(dados$EscolaPublica)


# Manipulação de dados: cálculo da média e codificação das variáveis de Cor
dados <- dados %>%
  mutate(
    media = rowMeans(select(., NotaCN, NotaCH, NotaLC, NotaMT, NotaRE), na.rm = TRUE),
    Aprovado = ifelse(media >= 525, 1, 0),
    EscolaPublica = ifelse(is.na(EscolaPublica), 0, ifelse(EscolaPublica == "S", 1, 0)),
    media_norm = scale(media),           # Normaliza a média
    media_quad = media_norm^2,           # Adiciona transformação quadrática
    CorBranco = ifelse(Cor == "B", 1, 0),
    CorPreto = ifelse(Cor == "Pr", 1, 0),
    CorPardo = ifelse(Cor == "Pa", 1, 0)
  ) %>%
  filter(!is.na(media))

# Ajustar o modelo de regressão logística com variáveis indicadoras para Cor
modelo_categ <- glm(Aprovado ~ media_quad + EscolaPublica + CorBranco + CorPreto + CorPardo, data = dados, family = binomial(link = "logit"))

# Exibir a equação do modelo
print("Equação do modelo de regressão logística com variáveis indicadoras para Cor:")
print(summary(modelo_categ)$coefficients)

# Calcular Odds Ratio e Intervalos de Confiança
odds_ratios_categ <- exp(coef(modelo_categ))
conf_int_categ <- exp(confint.default(modelo_categ))
print("Odds Ratios e Intervalos de Confiança:")
print(data.frame(OddsRatio = odds_ratios_categ, IC_2.5 = conf_int_categ[, 1], IC_97.5 = conf_int_categ[, 2]))

# Calcular log-likelihood
log_likelihood <- logLik(modelo_categ)
print(log_likelihood)

# Calcular pseudo-R²
pseudo_R2 <- pR2(modelo_categ)
print(pseudo_R2)

# Previsões e tabela de confusão
predicoes_categ <- ifelse(predict(modelo_categ, type = "response") > 0.5, 1, 0)
confusao_categ <- table(dados$Aprovado, predicoes_categ)
print("Tabela de Confusão (modelo com variáveis indicadoras para Cor):")
print(confusao_categ)

# Calcular sensibilidade, especificidade e acurácia
sensibilidade_categ <- confusao_categ[2, 2] / sum(confusao_categ[2, ])
especificidade_categ <- confusao_categ[1, 1] / sum(confusao_categ[1, ])
acuracia_categ <- sum(diag(confusao_categ)) / sum(confusao_categ)
print("Sensibilidade (modelo com variáveis indicadoras para Cor):")
print(sensibilidade_categ)
print("Especificidade (modelo com variáveis indicadoras para Cor):")
print(especificidade_categ)
print("Acurácia (modelo com variáveis indicadoras para Cor):")
print(acuracia_categ)

# Gerar gráfico da regressão logística
dados$probabilidade <- predict(modelo_categ, type = "response")

ggplot(dados, aes(x = media_quad, y = probabilidade)) +
  geom_point(aes(color = factor(Aprovado)), alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(
    title = "Curva de Regressão Logística",
    x = "Media Quad (Transformada Quadrática de Média Normalizada)",
    y = "Probabilidade de Aprovação",
    color = "Aprovado"
  ) +
  theme_minimal()


# Tabela de coeficientes
coeficientes <- data.frame(
  Variável = rownames(summary(modelo_categ)$coefficients),
  Estimate = summary(modelo_categ)$coefficients[, "Estimate"],
  Std_Error = summary(modelo_categ)$coefficients[, "Std. Error"],
  z_value = summary(modelo_categ)$coefficients[, "z value"],
  Pr_z = summary(modelo_categ)$coefficients[, "Pr(>|z|)"]
)

#print("summary modelo categoria")
#print(summary(modelo_categ))

#odds_ratios_categ <- exp(coef(modelo_categ))
#conf_int_categ <- exp(confint.default(modelo_categ))
#print("comprimento e largura de odd")
#print(length(odds_ratios_categ))
#print(dim(conf_int_categ))

print("Coeficientes do modelo de regressão logística com variáveis indicadoras para Cor:")
print(coeficientes)

# Tabela de Odds Ratios e Intervalos de Confiança
odds_ratios_tabela <- data.frame(
  Variável = names(odds_ratios_categ),
  OddsRatio = odds_ratios_categ,
  IC_2.5 = conf_int_categ[, 1],
  IC_97.5 = conf_int_categ[, 2]
)
print("Odds Ratios e Intervalos de Confiança:")
print(odds_ratios_tabela)

# Tabela de Confusão
confusao_tabela <- as.data.frame.matrix(confusao_categ)
colnames(confusao_tabela) <- c("Previsto_0", "Previsto_1")
rownames(confusao_tabela) <- c("Real_0", "Real_1")
print("Tabela de Confusão:")
print(confusao_tabela)

# Tabela de métricas de desempenho
metricas <- data.frame(
  Métrica = c("Sensibilidade", "Especificidade", "Acurácia"),
  Valor = c(sensibilidade_categ, especificidade_categ, acuracia_categ)
)
print("Métricas de Desempenho:")
print(metricas)

# Calcular os Resíduos 
residuos <- residuals(modelo_categ, type = "deviance")
ggplot(dados, aes(x = media_quad, y = residuos)) + geom_point() + geom_smooth(se = FALSE) +
  labs(title = "Tendência dos Resíduos")


# Autocorrelação dos Resíduos com as Variáveis Independentes
acf(residuos, main = "Autocorrelação dos Resíduos")



