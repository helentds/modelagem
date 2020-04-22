
# Definir diretório de trabalho
setwd("~/Dropbox/Experimento_MaquiLavar")


# Treinando o modelo
install.packages("neuralnet")
install.packages("caTools")
library(neuralnet)
library(caTools)

df_lav <- read.csv("Lavando.csv")

df_lav <- na.omit(df_lav[,-1])
df_lav <- df_lav[,-4]
df_lav$opel<- 1
View(df_lav)

df_Enx <- read.csv("Enxaguando.csv")

df_Enx <- na.omit(df_Enx[,-1])
df_Enx <- df_Enx[,c(-4,-5)]
df_Enx$opel<- 2

#View(df_Enx)

df_Cent <- read.csv("Centrifugando.csv")



df_Cent <- na.omit(df_Cent[,-1])
df_Cent <- df_Cent[,c(-4,-5)]
df_Cent$opel<- 3
df_Cent <- df_Cent[c(1:200),]

#View(df_Cent)


df <- rbind(df_lav, df_Enx, df_Cent)
View(df)

summary(df)


# Função de Normalização
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Aplicando a função de normalização a todo o dataset
lav_norm <- as.data.frame(lapply(df, normalize))

# Confirmando que o range está entre 0 e 1
summary(lav_norm$opel)
summary(lav_norm)
# Comparando com o original
summary(df$opel)
summary(df)

# Dividindo os dados
split = sample.split(lav_norm$opel, SplitRatio = 0.70)

treino = subset(df, split == TRUE)
teste = subset(df, split == FALSE)

# Obtendo o nome das colunas
coluna_nomes <- names(treino)
coluna_nomes

# Agregando
formula <- as.formula(paste("opel ~", paste(coluna_nomes[!coluna_nomes %in% "opel"], collapse = " + ")))
formula

# Treinando o Modelo
rede_neural <- neuralnet(formula, data = treino, hidden = 1)

# Plot
plot(rede_neural)
?neuralnet


# Avaliando a performance
model_results <- compute(rede_neural, teste[1:3])

# Obter os valores previstos
predicted_opel <- model_results$net.result

# Examinando a correlação dos valores previstos

cor(predicted_opel, teste$opel)


# Otimizando o Modelo
rede_neural1 <- neuralnet(formula, data = treino, hidden = 5, threshold = 0.01,
                          stepmax = 1e+15)

# Plot
plot(rede_neural1)

#?neuralnet


# Avaliando a performance
model_results1 <- compute(rede_neural1, teste[1:3])

# Obter os valores previstos
predicted_opel1 <- model_results1$net.result

# Examinando a correlação dos valores previstos

cor(predicted_opel1, teste$opel)


df_maquina <- write.csv(df, file = "MaquinaLavar.csv")





