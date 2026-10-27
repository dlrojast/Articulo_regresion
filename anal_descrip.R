# ============================================================
# ESTADÍSTICA DESCRIPTIVA DE IPCF, GINI Y THEIL
# Departamentos de Colombia: 2002, 2005, 2008 y 2016
# ============================================================

# Instalar paquetes si es necesario
paquetes <- c("dplyr", "tidyr", "ggplot2")

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# 1. INGRESAR LOS DATOS
# Los valores faltantes se representan con NA.
# Se utilizan puntos decimales, como requiere R.
# ============================================================

datos <- data.frame(
  DEPARTAMENTO = c(
    "Antioquia", "Atlántico", "Bogotá", "Bolívar", "Boyacá",
    "Caldas", "Caquetá", "Cauca", "Cesar", "Chocó",
    "Córdoba", "Cundinamarca", "Huila", "La Guajira",
    "Magdalena", "Meta", "Nariño", "Norte de Santander",
    "Quindío", "Risaralda", "Santander", "Sucre", "Tolima",
    "Valle del Cauca"
  ),
  
  IPCF_2002 = c(
    110.20, 102.36, 176.68, 64.15, NA, 87.30, NA, NA, NA, NA,
    58.60, NA, NA, NA, NA, 85.10, 59.23, 71.01, NA, 104.91,
    100.99, NA, 75.40, 119.39
  ),
  
  GINI_2002 = c(
    0.569, 0.534, 0.544, 0.529, NA, 0.504, NA, NA, NA, NA,
    0.545, NA, NA, NA, NA, 0.476, 0.518, 0.483, NA, 0.495,
    0.531, NA, 0.517, 0.528
  ),
  
  THEIL_2002 = c(
    0.727, 0.572, 0.569, 0.551, NA, 0.500, NA, NA, NA, NA,
    0.592, NA, NA, NA, NA, 0.445, 0.539, 0.470, NA, 0.496,
    0.539, NA, 0.513, 0.572
  ),
  
  IPCF_2005 = c(
    110.35, 89.54, 179.54, 71.47, NA, 83.99, NA, NA, NA, NA,
    64.21, NA, NA, NA, NA, 91.76, 61.24, 62.96, NA, 97.86,
    100.73, NA, 71.06, 120.67
  ),
  
  GINI_2005 = c(
    0.551, 0.512, 0.556, 0.482, NA, 0.502, NA, NA, NA, NA,
    0.534, NA, NA, NA, NA, 0.491, 0.506, 0.487, NA, 0.505,
    0.520, NA, 0.493, 0.538
  ),
  
  THEIL_2005 = c(
    0.624, 0.550, 0.615, 0.447, NA, 0.498, NA, NA, NA, NA,
    0.568, NA, NA, NA, NA, 0.484, 0.493, 0.472, NA, 0.508,
    0.523, NA, 0.481, 0.599
  ),
  
  IPCF_2008 = c(
    116.02, 83.25, 180.85, 65.08, 67.47, 86.72, 64.48,
    53.08, 52.82, 46.76, 58.85, 83.89, 64.13, 48.94,
    55.02, 98.13, 61.90, 73.13, 95.45, 94.91, 108.73,
    50.76, 75.27, 109.26
  ),
  
  GINI_2008 = c(
    0.578, 0.502, 0.529, 0.550, 0.556, 0.544, 0.476,
    0.575, 0.541, 0.597, 0.523, 0.457, 0.573, 0.558,
    0.559, 0.496, 0.532, 0.498, 0.560, 0.510, 0.476,
    0.527, 0.509, 0.517
  ),
  
  THEIL_2008 = c(
    0.674, 0.514, 0.555, 0.606, 0.578, 0.602, 0.443,
    0.640, 0.583, 0.744, 0.554, 0.393, 0.671, 0.611,
    0.645, 0.483, 0.582, 0.484, 0.670, 0.512, 0.417,
    0.563, 0.497, 0.543
  ),
  
  IPCF_2016 = c(
    116.85, 90.62, 168.75, 70.49, 84.00, 89.18, 62.25,
    56.21, 63.00, 43.15, 60.91, 97.80, 67.73, 61.00,
    56.66, 89.86, 63.81, 65.99, 83.50, 91.22, 104.79,
    56.68, 80.68, 108.59
  ),
  
  GINI_2016 = c(
    0.505, 0.437, 0.497, 0.480, 0.530, 0.507, 0.460,
    0.499, 0.471, 0.569, 0.468, 0.431, 0.520, 0.532,
    0.486, 0.458, 0.522, 0.474, 0.464, 0.437, 0.445,
    0.466, 0.482, 0.485
  ),
  
  THEIL_2016 = c(
    0.500, 0.374, 0.496, 0.445, 0.560, 0.523, 0.432,
    0.478, 0.422, 0.644, 0.443, 0.352, 0.502, 0.536,
    0.446, 0.415, 0.536, 0.424, 0.436, 0.367, 0.374,
    0.430, 0.457, 0.484
  )
)

# ============================================================
# 2. TRANSFORMAR LOS DATOS A FORMATO LARGO
# ============================================================

datos_largos <- datos %>%
  pivot_longer(
    cols = -DEPARTAMENTO,
    names_to = c("Indicador", "Anio"),
    names_pattern = "(IPCF|GINI|THEIL)_(\\d+)",
    values_to = "Valor"
  ) %>%
  mutate(Anio = as.integer(Anio))

# ============================================================
# 3. FUNCIÓN DE ESTADÍSTICA DESCRIPTIVA
# ============================================================

estadisticas <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(data.frame(
      N = 0,
      Media = NA,
      Mediana = NA,
      Desviacion_Estandar = NA,
      Varianza = NA,
      Minimo = NA,
      Q1 = NA,
      Q3 = NA,
      Maximo = NA,
      Rango = NA
    ))
  }
  
  data.frame(
    N = length(x),
    Media = mean(x),
    Mediana = median(x),
    Desviacion_Estandar = sd(x),
    Varianza = var(x),
    Minimo = min(x),
    Q1 = unname(quantile(x, 0.25)),
    Q3 = unname(quantile(x, 0.75)),
    Maximo = max(x),
    Rango = max(x) - min(x)
  )
}

# ============================================================
# 4. CALCULAR ESTADÍSTICAS POR INDICADOR Y AÑO
# ============================================================

tabla_descriptiva <- datos_largos %>%
  group_by(Indicador, Anio) %>%
  group_modify(~ estadisticas(.x$Valor)) %>%
  ungroup() %>%
  arrange(Indicador, Anio)

# Mostrar resultados
print(tabla_descriptiva, n = Inf)

# ============================================================
# 5. CALCULAR EL NÚMERO DE DATOS DISPONIBLES POR AÑO
# ============================================================

datos_disponibles <- datos_largos %>%
  group_by(Indicador, Anio) %>%
  summarise(
    Observaciones = sum(!is.na(Valor)),
    Datos_faltantes = sum(is.na(Valor)),
    .groups = "drop"
  )

print(datos_disponibles, n = Inf)

# ============================================================
# 6. RESUMEN POR DEPARTAMENTO PARA EL AÑO 2016
# ============================================================


# 1. Corregir la opción de impresión de valores faltantes
options(na.print = "NA")

# 2. Crear el resumen de 2016
resumen_2016 <- datos %>%
  select(
    DEPARTAMENTO,
    IPCF_2016,
    GINI_2016,
    THEIL_2016
  )

# 3. Mostrar los resultados
print(resumen_2016)

# ============================================================
# 7. GRÁFICOS DE CAJA POR INDICADOR Y AÑO
# ============================================================

ggplot(
  datos_largos %>% filter(!is.na(Valor)),
  aes(x = factor(Anio), y = Valor)
) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  facet_wrap(~ Indicador, scales = "free_y") +
  labs(
    title = "Distribución de los indicadores por año",
    x = "Año",
    y = "Valor"
  ) +
  theme_minimal()

# ============================================================
# 8. GRÁFICOS DE EVOLUCIÓN DE LAS MEDIAS
# ============================================================

medias <- datos_largos %>%
  group_by(Indicador, Anio) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  medias,
  aes(x = Anio, y = Media, group = Indicador)
) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ Indicador, scales = "free_y") +
  scale_x_continuous(breaks = c(2002, 2005, 2008, 2016)) +
  labs(
    title = "Evolución de la media de los indicadores",
    x = "Año",
    y = "Media"
  ) +
  theme_minimal()


