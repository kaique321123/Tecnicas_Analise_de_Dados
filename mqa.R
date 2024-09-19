# Lendo o arquivo CSV
dados <- read.csv("C:\\Users\\KaiquedeJesusPessoaS\\Desktop\\analise-mqa\\ENTRADA_LIMPA.csv", header = TRUE, sep = ",")

#View(dados)
str(dados)

# Convertendo colunas para numérico, se necessário
dados$Alta <- as.numeric(as.character(dados$Alta))
dados$Baixa <- as.numeric(as.character(dados$Baixa))
dados$Média <- as.numeric(as.character(dados$Média))
dados$`Média Alta` <- as.numeric(as.character(dados$`Média Alta`))
dados$`Sem Renda` <- as.numeric(as.character(dados$`Sem Renda`))

# Cálculo das médias, ignorando valores NA
media_geral_Alta <- mean(dados$Alta, na.rm = TRUE)
media_geral_Baixa <- mean(dados$Baixa, na.rm = TRUE)
media_geral_Média <- mean(dados$Média, na.rm = TRUE)
media_geral_Média_Alta <- mean(dados$`Média Alta`, na.rm = TRUE)
media_geral_Sem_Renda <- mean(dados$`Sem Renda`, na.rm = TRUE)

# Cálculo da variância para a coluna Media_de_Notas, ignorando valores NA
#variancia_geral <- var(dados$Media_de_Notas, na.rm = TRUE)

# Exibir os resultados
print(paste("Média Geral Alta:", media_geral_Alta))
print(paste("Média Geral Baixa:", media_geral_Baixa))
print(paste("Média Geral Média:", media_geral_Média))
print(paste("Média Geral Média Alta:", media_geral_Média_Alta))
print(paste("Média Geral Sem Renda:", media_geral_Sem_Renda))
#print(paste("Variância Geral:", variancia_geral))