# Cluster
install.packages("quantmod")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("factoextra")
install.packages("cluster")

library(quantmod)
library(dplyr)

# Descargar datos de Copec desde Yahoo Finance
getSymbols("COPEC.SN", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# Mostrar los primeros datos
head(COPEC.SN)

# Seleccionar columnas relevantes (precio ajustado y volumen)
data_copec <- data.frame(Date = index(COPEC.SN), coredata(COPEC.SN)[,c("COPEC.SN.Adjusted", "COPEC.SN.Volume")])

# Normalizar los datos para el análisis de clusters
data_normalized <- scale(data_copec[, -1])

# Ver los primeros datos normalizados
head(data_normalized)

set.seed(20200)

# Crear un dataframe simulado de clientes
clientes <- data.frame(
  ClienteID = 1:1000,
  TipoCliente = sample(c("Particular", "Empresa", "Energía Limpia"), 1000, replace = TRUE),
  FrecuenciaCompra = round(runif(1000, 1, 10), 0), # Frecuencia de compras al mes
  VolumenCompra = round(runif(1000, 20, 1000), 0), # Volumen de combustible o energía
  PreferenciaAmbiental = sample(c(0, 1), 1000, replace = TRUE) # 1: Interesado en energía limpia, 0: No
)

# Ver primeros datos
head(clientes)


# Normalización de variables numéricas
clientes_normalized <- scale(clientes[, c("FrecuenciaCompra", "VolumenCompra")])


# Definir el número de clusters
set.seed(20200)
kmeans_result <- kmeans(clientes_normalized, centers = 3, nstart = 20)

# Agregar los clusters a los datos originales
clientes$Cluster <- as.factor(kmeans_result$cluster)

# Ver los primeros datos con cluster asignado
head(clientes)


# Asignar nombres a los clusters
clientes$Cluster_Name <- case_when(
  clientes$Cluster == 1 ~ "Clientes de Alto Volumen",
  clientes$Cluster == 2 ~ "Clientes Particulares Frecuentes",
  clientes$Cluster == 3 ~ "Clientes Interesados en Energía Limpia"
)

# Ver los resultados con nombres
head(clientes[, c("ClienteID", "TipoCliente", "FrecuenciaCompra", "VolumenCompra", "Cluster_Name")])



library(ggplot2)

# Visualizar clusters con nombres descriptivos
ggplot(clientes, aes(x = FrecuenciaCompra, y = VolumenCompra, color = Cluster_Name)) +
  geom_point() +
  labs(title = "Clusters de Clientes de Copec", 
       x = "Frecuencia de Compra", 
       y = "Volumen de Compra", 
       color = "Nombre del Cluster") +
  theme_minimal()
