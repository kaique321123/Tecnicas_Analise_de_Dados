library(psych)


data <- read.csv("Analise_Fatorial\\Passageiros_Satisfacao_menor.csv")
#View(data)

data$Class <- ifelse(data$Class == "Eco", 1, 0)
#data$Gender <- ifelse(data$Gender == "Male", 1, 0)
data$satisfaction <- ifelse(data$satisfaction == "satisfied", 1, 0)

# "Gender" e "Gate.location" tem KMO muito baixo, então retirei essas variáveis
selected_columns <- data[, c( "Class", "Age","Flight.Distance","satisfaction")] 
#View(selected_columns)


# KMO
kmo_result <- KMO(selected_columns)
print(kmo_result)

# Matriz de Correlação
correlation_matrix <- cor(selected_columns)
print(correlation_matrix)

# Teste de Bartlett
bartlett_test <- cortest.bartlett(correlation_matrix, n = 10000)
print(bartlett_test)