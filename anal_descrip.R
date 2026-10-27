library(readxl)
datos <- read_excel("C:/Users/diler/OneDrive/Escritorio/Unal2024/UNAL/Estadística/Semestre VII/Regresión/articulo de aplicacion/datos.xlsx")
View(datos)     
str(datos)
dim(datos)
paquetes <- c("dplyr", "tidyr", "ggplot2")

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(dplyr)
library(tidyr)
library(ggplot2)
datos <- as.data.frame(datos)
datos
