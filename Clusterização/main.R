library(ggplot2)
library(ggdendro)
library(gplots)
library(dplyr)

# Definição do caminho para o arquivo CSV
arqAmostrasCruas = "C://Users//KaiquedeJesusPessoaS//Desktop//analise-mqa//Clusterização//Dados//50k_Amostras.csv"
arqAmostras = arqAmostrasCruas


clusterH = function() {
	cat("Realizando clusterização (Agrupamento)...\n")
	
	tabSelect = tabCluster[sample(nrow(tabCluster), 40), ]
	
	cat("Criando matriz de distâncias...\n")
	matDistancia = dist(tabSelect)
	
	cat("Gerando clusters...\n")
	hc = hclust(matDistancia)
	plot(hc)
	
	hcDentro = ggdendro::dendro_data(hc)
	
	# Plot with ggplot2
	ggplot() +
		geom_segment(data = hcDentro$segments, aes(x = x, y = y, xend = xend, yend = yend)) +
		geom_text(data = hcDentro$labels, aes(x = x, y = y, label = label), hjust = 1, angle = 90) +
		theme_minimal() +
		labs(title = "Dendrogram of Hierarchical Clustering", x = "", y = "Height")
	
	cat("Clusterização completa.\n")
}

clusterK = function() {
	cat("Realizando clusterização (K-Means)...\n")
  
  
	# Executa a clusterização
	kmeans_result <<- kmeans(tabCluster, centers = 6, nstart = 25, iter.max = 50)
	
	# Copia as colunas de cluster para as tabelas
	tabCluster$cluster <<- as.factor(kmeans_result$cluster)
	tabLimpa$cluster <<- as.factor(kmeans_result$cluster)
	
	# Exibe o Plot
	show(ggplot(tabCluster, aes(x = zLC, y = zRE, color = cluster)) +
			 	geom_point(size = 1) +
			 	scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
			 	scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
			 	labs() +
			 	theme_minimal()
	)
	
	cat("Pronto.\n");
}

normalizar = function() {
	# Realiza normalização
	# + runif(nrow(tabLimpa), min = -10, max = 10)
	tabLimpa$zRE <<- scale(tabLimpa$NU_NOTA_RE)
	tabLimpa$zLC <<- scale(tabLimpa$NU_NOTA_LC)
	tabLimpa$zCH <<- scale(tabLimpa$NU_NOTA_CH)
	tabLimpa$zMT <<- scale(tabLimpa$NU_NOTA_MT)
	tabLimpa$zCN <<- scale(tabLimpa$NU_NOTA_CN)
	
	# Variáveis de critério para a clusterização
	tabCluster <<- tabLimpa[, c("zLC", "zCH", "zCN", "zMT", "zRE")]
}

iniFiltrar = function() {
	tabLimpa <<- tabAmostras
	tabLimpa <<- tabAmostras[, c(
		"NU_NOTA_LC",
		"NU_NOTA_MT",
		"NU_NOTA_CN",
		"NU_NOTA_CH",
		"NU_NOTA_REDACAO",
		"CO_UF_PROVA",
		"TP_COR_RACA",
		"TP_ESCOLA",
		"Q006"
	)]
	
	cat("Valores Totais: ", nrow(tabLimpa), "\n")
	
	# Removendo NAs
	tabLimpa <<- na.omit(tabLimpa)
	cat("Sem NA: ", nrow(tabLimpa), "\n")
	
	# Removendo 0s
	tabLimpa <<- tabLimpa[!apply(tabLimpa == 0, 1, any), ]
	cat("Sem 0s: ", nrow(tabLimpa), "\n")

	tabLimpa$Regiao <<- sapply(tabLimpa$CO_UF_PROVA, transRegiao)
	tabLimpa$Renda <<- sapply(tabLimpa$Q006, transRenda)
	tabLimpa$Cor <<- sapply(tabLimpa$TP_COR_RACA, transCor)
	tabLimpa$Escola <<- sapply(tabLimpa$TP_ESCOLA, transEscola)
}

transRegiao = function(uf) {
	cod = (uf - uf %% 10) / 10
	return(switch(cod, "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) 
}

transRenda = function(cat) {
	# [A, B]: Sem Renda
	# [C, H]: Baixa Renda
	# [I, K]: Média
	# [L, P]: Alta
	# [Q, -]: Muito Alto
	
	num <- as.numeric(charToRaw(cat)) - as.numeric(charToRaw('A'))
	if (num <= 1) return ("Sem")
	if (num <= 7) return ("Baixa")
	if (num <= 10) return ("Média")
	if (num <= 15) return ("Alta")
	return ("Muito Alta")
}

transCor = function(cor) {
	return(switch(cor, "Branca", "Preta", "Parda", "Amarela", "Indígena"))
}

transEscola = function(esc) {
	return(switch(esc, NA, "Pública", "Privada"))
}

iniLerAmostras = function() {
	cat("Lendo amostras de '", arqAmostras, "'...\n")
  
	# Lendo o arquivo CSV separado por vírgula explicitamente
	tabAmostras <<- read.csv(arqAmostras, header = TRUE, sep = ";")
	cat("Leitura completa!", nrow(tabAmostras), "entradas. \n");
}


#iniLerAmostras()
#iniFiltrar()
#normalizar()
clusterH()
clusterK()

# Carregar pacotes necessários
library(dplyr)

# Função para calcular a moda
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calcular medidas de tendência central e dispersão para as notas
notas <- data.frame(
  "Nota" = c("Linguagens e Códigos (LC)", "Matemática (MT)", "Redação (RE)", "Ciências Humanas (CH)", "Ciências da Natureza (CN)"),
  "Média" = c(mean(tabLimpa$NU_NOTA_LC, na.rm = TRUE), 
              mean(tabLimpa$NU_NOTA_MT, na.rm = TRUE), 
              mean(tabLimpa$NU_NOTA_RE, na.rm = TRUE),
              mean(tabLimpa$NU_NOTA_CH, na.rm = TRUE),
              mean(tabLimpa$NU_NOTA_CN, na.rm = TRUE)),
  "Desvio Padrão" = c(sd(tabLimpa$NU_NOTA_LC, na.rm = TRUE),
                      sd(tabLimpa$NU_NOTA_MT, na.rm = TRUE),
                      sd(tabLimpa$NU_NOTA_RE, na.rm = TRUE),
                      sd(tabLimpa$NU_NOTA_CH, na.rm = TRUE),
                      sd(tabLimpa$NU_NOTA_CN, na.rm = TRUE)),
  "Mediana" = c(median(tabLimpa$NU_NOTA_LC, na.rm = TRUE),
                median(tabLimpa$NU_NOTA_MT, na.rm = TRUE),
                median(tabLimpa$NU_NOTA_RE, na.rm = TRUE),
                median(tabLimpa$NU_NOTA_CH, na.rm = TRUE),
                median(tabLimpa$NU_NOTA_CN, na.rm = TRUE)),
  "Mínimo" = c(min(tabLimpa$NU_NOTA_LC, na.rm = TRUE),
               min(tabLimpa$NU_NOTA_MT, na.rm = TRUE),
               min(tabLimpa$NU_NOTA_RE, na.rm = TRUE),
               min(tabLimpa$NU_NOTA_CH, na.rm = TRUE),
               min(tabLimpa$NU_NOTA_CN, na.rm = TRUE)),
  "Máximo" = c(max(tabLimpa$NU_NOTA_LC, na.rm = TRUE),
               max(tabLimpa$NU_NOTA_MT, na.rm = TRUE),
               max(tabLimpa$NU_NOTA_RE, na.rm = TRUE),
               max(tabLimpa$NU_NOTA_CH, na.rm = TRUE),
               max(tabLimpa$NU_NOTA_CN, na.rm = TRUE))
)

# Calcular moda para variáveis categóricas (Cor, Renda, UF)
modaCor <- get_mode(tabLimpa$Cor)
modaRenda <- get_mode(tabLimpa$Renda)
modaUF <- get_mode(tabLimpa$Regiao)

# Criar uma tabela para as modas das variáveis categóricas
modas <- data.frame(
  "Variável" = c("Cor", "Renda", "UF (Estado)"),
  "Moda" = c(modaCor, modaRenda, modaUF)
)

output_file <- "C://Users//KaiquedeJesusPessoaS//Desktop//tabLimpa.csv"
write.csv(tabLimpa, file = output_file, row.names = FALSE)

cat("tabLimpa foi salva no arquivo:", output_file, "\n")


get_mode <- function(v) {
  v <- na.omit(v)  # Remove valores NA
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calcular moda para cada variável categórica por cluster
modaPorCluster <- tabLimpa %>%
  group_by(cluster) %>%
  summarise(
    ModaCor = get_mode(Cor),
    ModaRenda = get_mode(Renda),
    ModaEscola = get_mode(Escola),  # Não conta NA aqui
    ModaRegiao = get_mode(Regiao)
  )

# Exibir as modas por cluster
print(modaPorCluster)

# Salvar a tabela de modas por cluster
#output_moda_file <- "C://Users//KaiquedeJesusPessoaS//Desktop//modasPorCluster.csv"
#write.csv(modaPorCluster, file = output_moda_file, row.names = FALSE)

#cat("Tabela de modas por cluster foi salva no arquivo:", output_moda_file, "\n")

cat("Cálculo de modas por cluster completo.\n")


cat("Script completo.\n")