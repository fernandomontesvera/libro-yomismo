# 1. Cargar las librerías necesarias
library(tidyverse)

# 2. Cargar los datos directamente desde la URL del archivo CSV
url_datos <- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"
provincias_df <- read_csv(url_datos)

# 3. Análisis de datos con Tidyverse
provincias_top10_superficie <- provincias_df %>%
  arrange(desc(`Superficie en km2`)) %>%
  slice_head(n = 10)

# 4. Crear el gráfico con ggplot2
grafico_superficie <- ggplot(provincias_top10_superficie, 
                             aes(x = fct_reorder(`Nombre de provincia`, `Superficie en km2`), 
                                 y = `Superficie en km2`)) +
  geom_col(fill = "#0072B2", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 de Provincias Argentinas por Superficie",
    subtitle = "Datos expresados en kilómetros cuadrados (km²)",
    x = "Provincia",
    y = "Superficie (km²)"
  ) +
  geom_text(aes(label = scales::comma(`Superficie en km2`, big.mark = ".")), 
            hjust = -0.1, size = 3, color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold")
  )

# 5. Mostrar el gráfico generado
print(grafico_superficie)
