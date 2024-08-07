# Instalación de paquetes necesarios
install.packages("sf")
install.packages("terra")
install.packages("readxl")
install.packages("R0")
install.packages("MASS")
install.packages("tidyverse")
install.packages("aTSA")
install.packages("zoo")
install.packages("ggplot2")
install.packages("rstatix")
install.packages("ggspatial")
install.packages("prettymapr")
install.packages("tmap")
install.packages("spData")
install.packages("spdep")
install.packages("classInt")

# Cargar librerías
library(sf)
library(terra)
library(R0)
library(readxl)
library(tidyverse)
library(aTSA)
library(zoo)
library(ggplot2)
library(rstatix)
library(ggspatial)
library(prettymapr)
library(tmap)
library(spData)
library(spdep)
library(dplyr)
library(classInt)

# Limpiar el entorno
rm(list=ls())

# Leer datos de Excel
df <- read_csv("C:/Users/conta/Downloads/dengue_data_all_municipalities.csv/dengue_data_all_municipalities.csv")
df = data.frame(df)

library(dplyr)

#################################################################
# TASAS DE INCIDENCIA
#################################################################

# Calcular tasas por semana epidemiológica
tasa_se <- df %>%
  mutate(across(342:1017, ~ . / df[[names(df)[3]]] * 100000, .names = "t.{col}"))

# Agregar nombres de municipios y códigos
#tasa_se <- tasa_se %>%
#  select(Municipio = Municipality, Municipio_code = Municipality.code, starts_with("t."))

# Verificar resultados
head(tasa_se)


# Cargar shapefile de Colombia usando sf
setwd("C:/Users/conta/Downloads/Piloto_Desigualdades")
Colombia <- st_read("Piloto_Desigualdades.shp")
head(Colombia)
plot(Colombia["geometry"])

colombia = cbind(Colombia[,6], Colombia[,9])

# Fusionar datos espaciales con datos de tasas
Colombiac <- merge(colombia, tasa_se, by.x="Cod_Mun", by.y="Municipality.code")
head(Colombiac)
class(Colombiac)

Colombiac = Colombiac[Colombiac$Cod_Mun != 88001 & Colombiac$Cod_Mun != 88564,]
# Guardar el dataframe Colombiac en un archivo CSV
tasas = tasa_se[,c(1,2,1018:1693)]

write.csv(df, file = "C:/Users/conta/Downloads/df_completa.csv", row.names = FALSE)
write.csv(tasas, file = "C:/Users/conta/Downloads/tasas.csv", row.names = FALSE)

# Análisis espacial

# Calcular vecinos usando sf y spdep
neighbours <- poly2nb(tasas)
plot(tasas[,169:179], border = 'lightgrey')
plot(neighbours, coordinates(tasas, add=TRUE, col='red')

# Calcular vecinos del caso de Rook
neighbours2 <- poly2nb(Colombiac, queen = FALSE)
plot(Colombiac, border = 'lightgrey')
plot(neighbours, coordinates(Colombiac), add=TRUE, col='blue')
plot(neighbours2, coordinates(Colombiac), add=TRUE, col='red')

# Limpiar conjuntos de vecinos vacíos
valid_indices <- which(card(neighbours) > 0)
neighbours_clean <- neighbours[valid_indices]

# Reconstruir nb objeto asegurando que el formato sea correcto
neighbours_clean <- lapply(neighbours_clean, function(neigh) neigh[neigh %in% valid_indices])

# Convertir los datos de vecinos a un objeto listw
listw <- nb2listw(neighbours)
listw

# Autocorrelación espacial global
moran.test(na.omit(Colombiac$tasa_se_X2010.w16), listw)
moran.test(na.omit(Colombiac$tasa_se_X2010.w18), listw)


# Crear un gráfico de Moran
moran <- moran.plot(na.omit(Colombiac$tasa_se_X2010.w16), listw = nb2listw(neighbours, style = "W"), labels=as.character(Colombiac$MpNombre))

# Crear salida local de Moran
local <- localmoran(x = na.omit(Colombiac$tasa_se_X2010.w16), listw = nb2listw(neighbours, style = "W"))

# Adjuntar resultados a nuestro shapefile de polígonos
moran.map <- cbind(Colombiac, local)

# Mapear los resultados
tm_shape(moran.map) + 
  tm_fill(col = "Ii", style = "quantile", title = "Local Moran statistic") + 
  tm_borders(alpha=.4)

# Crear un mapa LISA
quadrant <- vector(mode="numeric", length=nrow(local))
m.qualification <- Colombiac$X202052 - mean(Colombiac$X202052)
m.local <- local[,1] - mean(local[,1])
signif <- 0.1
quadrant[m.qualification > 0 & m.local > 0] <- 4
quadrant[m.qualification < 0 & m.local < 0] <- 1
quadrant[m.qualification < 0 & m.local > 0] <- 2
quadrant[m.qualification > 0 & m.local < 0] <- 3
quadrant[local[,5] > signif] <- 0
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(Colombiac, border="lightgray", col=colors[findInterval(quadrant, brks, all.inside=FALSE)])
box()
text(Colombiac, labels = "Comuna", cex=0.5)

# Guardar el dataframe Colombiac en un archivo CSV
write.csv(Colombiac, file = "C:/DAIRA/INVESTIGACION/Covid_19/data/Colombiac.csv", row.names = FALSE)
