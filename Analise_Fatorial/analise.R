library(psych)
library(nFactors)
library(ggplot2)
library(cluster)

#data <- read.csv("Passageiros_Satisfacao_menor.csv")
data <- read.csv("Analise_Fatorial//Passageiros_Satisfacao_menor.csv")
#View(data)
#print(colnames(data))

data$Class <- ifelse(data$Class == "Eco", 1, 0)
#data$Gender <- ifelse(data$Gender == "Male", 1, 0)
data$satisfaction <- ifelse(data$satisfaction == "satisfied", 1, 0)

# "Gender" e "Gate.location" tem KMO muito baixo, então retirei essas variáveis
selected_columns <- data[, c( "Class", "Age","Flight.Distance","satisfaction",
                              "Food.and.drink","Online.boarding","Seat.comfort",
                              "Baggage.handling","Cleanliness","Inflight.wifi.service")] 
View(selected_columns)

# KMO
kmo_result <- KMO(selected_columns)
print(kmo_result)

# Matriz de Correlação
correlation_matrix <- cor(selected_columns)
# Arredondando a matriz para duas casas decimais
correlation_matrix <- round(correlation_matrix, 2)
# Exibindo a matriz de correlação com duas casas decimais
print(correlation_matrix)

# Teste de Bartlett
bartlett_test <- cortest.bartlett(correlation_matrix, n = 10000)
print(bartlett_test)

# MSA para cada variável
msa_per_variable <- kmo_result$MSA
print(msa_per_variable)

# ESCOLHA DO NÚMERO DE FATORES
# 1. Autovalores (eigenvalues)
fa_eigenvalues <- eigen(correlation_matrix)$values
print(fa_eigenvalues)

# 2. Scree Plot
fa.parallel(correlation_matrix, n.obs = 10000, fa = "pc", main = "Scree Plot")

# 3. Variância explicada
print("Rotação")
fa_var <- fa(correlation_matrix, nfactors = length(fa_eigenvalues), rotate = "none")
print(fa_var)

# 4. Determinação automática de fatores
n_factors <- nScree(x = fa_eigenvalues)
print(n_factors)


# Função para calcular a moda
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}



# Calcular as medidas apenas para variáveis numéricas
medidas_numericas <- data.frame(
  "Variável" = c("Age", "Flight.Distance", "Food.and.drink", 
                 "Online.boarding", "Seat.comfort", "Baggage.handling", 
                 "Cleanliness", "Inflight.wifi.service"),
  "Média" = c(mean(selected_columns$Age, na.rm = TRUE),
              mean(selected_columns$Flight.Distance, na.rm = TRUE),
              mean(selected_columns$Food.and.drink, na.rm = TRUE),
              mean(selected_columns$Online.boarding, na.rm = TRUE),
              mean(selected_columns$Seat.comfort, na.rm = TRUE),
              mean(selected_columns$Baggage.handling, na.rm = TRUE),
              mean(selected_columns$Cleanliness, na.rm = TRUE),
              mean(selected_columns$Inflight.wifi.service, na.rm = TRUE)),
  "Desvio Padrão" = c(sd(selected_columns$Age, na.rm = TRUE),
                      sd(selected_columns$Flight.Distance, na.rm = TRUE),
                      sd(selected_columns$Food.and.drink, na.rm = TRUE),
                      sd(selected_columns$Online.boarding, na.rm = TRUE),
                      sd(selected_columns$Seat.comfort, na.rm = TRUE),
                      sd(selected_columns$Baggage.handling, na.rm = TRUE),
                      sd(selected_columns$Cleanliness, na.rm = TRUE),
                      sd(selected_columns$Inflight.wifi.service, na.rm = TRUE)),
  "Mediana" = c(median(selected_columns$Age, na.rm = TRUE),
                median(selected_columns$Flight.Distance, na.rm = TRUE),
                median(selected_columns$Food.and.drink, na.rm = TRUE),
                median(selected_columns$Online.boarding, na.rm = TRUE),
                median(selected_columns$Seat.comfort, na.rm = TRUE),
                median(selected_columns$Baggage.handling, na.rm = TRUE),
                median(selected_columns$Cleanliness, na.rm = TRUE),
                median(selected_columns$Inflight.wifi.service, na.rm = TRUE)),
  "Mínimo" = c(min(selected_columns$Age, na.rm = TRUE),
               min(selected_columns$Flight.Distance, na.rm = TRUE),
               min(selected_columns$Food.and.drink, na.rm = TRUE),
               min(selected_columns$Online.boarding, na.rm = TRUE),
               min(selected_columns$Seat.comfort, na.rm = TRUE),
               min(selected_columns$Baggage.handling, na.rm = TRUE),
               min(selected_columns$Cleanliness, na.rm = TRUE),
               min(selected_columns$Inflight.wifi.service, na.rm = TRUE)),
  "Máximo" = c(max(selected_columns$Age, na.rm = TRUE),
               max(selected_columns$Flight.Distance, na.rm = TRUE),
               max(selected_columns$Food.and.drink, na.rm = TRUE),
               max(selected_columns$Online.boarding, na.rm = TRUE),
               max(selected_columns$Seat.comfort, na.rm = TRUE),
               max(selected_columns$Baggage.handling, na.rm = TRUE),
               max(selected_columns$Cleanliness, na.rm = TRUE),
               max(selected_columns$Inflight.wifi.service, na.rm = TRUE))
)

# Calcular a moda apenas para variáveis categóricas
medidas_categoricas <- data.frame(
  "Variável" = c("Class", "satisfaction"),
  "Moda" = c(get_mode(selected_columns$Class),
             get_mode(selected_columns$satisfaction))
)

# Exibir os resultados
print("Medidas Numéricas:")
print(medidas_numericas)

print("Medidas Categóricas:")
print(medidas_categoricas)




# Criar a matriz de correlação com os dados selecionados
numeric_columns <- sapply(selected_columns, is.numeric)
cor_matrix <- cor(selected_columns[, numeric_columns])

# Realizar a análise fatorial
fa_result <- fa(cor_matrix, nfactors = 4, rotate = "none")

# Criar o diagrama de fatores
fa.diagram(fa_result)

library(ggplot2)
library(reshape2)

# Transformar os dados para o formato longo
data_long <- melt(selected_columns)

# Criar boxplots para cada variável
ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  theme_minimal() +
  labs(title = "Boxplots das Variáveis Quantitativas",
       x = "Variáveis",
       y = "Valores") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



