# Carregar pacotes necessários
library(dplyr)
library(car)
library(ggplot2)
library(pscl)
library(stats)


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
modelo <- glm(Aprovado ~ media_quad + EscolaPublica + CorBranco + CorPreto + CorPardo, data = dados, family = binomial(link = "logit"))

# Criar um data frame com os valores preditos
dados$predito <- predict(modelo, type = "response")

print(
  ggplot(dados, aes(x = media_quad, y = Aprovado)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
    labs(title = "Gráfico de Regressão Logística",
         x = "Média Normalizada",
         y = "Probabilidade de Aprovação") +
    theme_minimal()
)

print(
  ggplot(dados, aes(x = media_norm, y = Aprovado)) +
    geom_point(alpha = 0.5) +
    geom_line(aes(y = predito), color = "blue", alpha = 0.1) +
    labs(title = "Gráfico de Regressão Logística",
         x = "Média Normalizada",
         y = "Probabilidade de Aprovação") +
    theme_minimal()
)

print(dados_agrupados <- dados %>%
        group_by(bin = cut(media_norm, breaks = 30)) %>%
        summarize(media_media_norm = mean(media_norm),
                  probabilidade_media = mean(predito))
      
      ggplot(dados, aes(x = media_norm, y = Aprovado)) +
        geom_point(alpha = 0.5) +
        geom_line(data = dados_agrupados, aes(x = media_media_norm, y = probabilidade_media), color = "blue") +
        labs(title = "Gráfico de Regressão Logística",
             x = "Média Normalizada",
             y = "Probabilidade de Aprovação") +
        theme_minimal()
)
