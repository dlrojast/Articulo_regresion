library(readxl)eD
datos <- read_excel("datos.xlsx")
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
