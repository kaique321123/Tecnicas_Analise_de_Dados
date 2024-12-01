data <- read.csv("Analise_Fatorial\\Passageiros_Satisfacao_menor.csv"
)


#View(data)
data$Class <- ifelse(data$Class == "Eco", 1, 0)
data$Gender <- ifelse(data$Gender == "Male", 1, 0)
data$satisfaction <- ifelse(data$satisfaction == "satisfied", 1, 0)
selected_columns <- data[, c("Gender", "Class", "Age","Flight.Distance","Gate.location","satisfaction")] 
#View(selected_columns)

#correlation_matrix <- cor(selected_columns)
#print(correlation_matrix)

library(psych)

selected_columns <- selected_columns[, -which(names(selected_columns) %in% c("Gender", "Gate.location"))]
correlation_matrix <- cor(selected_columns)
kmo_result <- KMO(correlation_matrix)
print(kmo_result)


