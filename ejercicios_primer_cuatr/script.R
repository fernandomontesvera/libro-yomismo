# Entorno limpio
rm(list = ls())

# Directorio
carpeta <- getwd()
setwd(carpeta)

# Instalar paquetes si es necesario
paquetes <- c("tidyverse", "tidytext", "ggplot2", "tm", "wordcloud", "RColorBrewer")
paquetes_a_instalar <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(paquetes_a_instalar)) install.packages(paquetes_a_instalar)

# Cargar librerías
library(tidyverse)
library(tidytext)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Descargar texto de Frankenstein desde Gutenberg
url <- "https://www.gutenberg.org/cache/epub/84/pg84.txt"
destino <- "./libro-gutengerg.txt"
download.file(url, destino, mode="wb")

# Leer texto
text_data <- readLines(destino, encoding = "UTF-8")
text_df <- data.frame(line = 1:length(text_data), text = text_data)

# Tokenizar texto
text_df <- text_df %>%
  unnest_tokens(word, text)

# Frecuencias
word_counts <- text_df %>%
  count(word, sort = TRUE)

# Densidad de vocabulario
total_words <- nrow(text_df)
unique_words <- n_distinct(text_df$word)
vocab_density <- unique_words / total_words

# Frecuencia relativa
word_counts <- word_counts %>%
  mutate(relative_frequency = n / total_words)

# Stopwords en inglés
stopwords1 <- get_stopwords("en")

# Leer stopwords personalizadas (una por línea)
stopwords2 <- readLines("stopwords_Frankenstein.txt", encoding = "UTF-8")
stopwords2 <- trimws(stopwords2)
stopwords2 <- stopwords2[stopwords2 != ""]

# Convertir a tibble para que funcione con anti_join
stopwords2_tbl <- tibble(word = stopwords2, lexicon = "custom")

# Unir ambas listas
stopwords_total <- bind_rows(stopwords1, stopwords2_tbl)

# Eliminar stopwords del conteo
word_counts <- word_counts %>% 
  anti_join(stopwords_total, by = "word")

# Filtrado final (mínimo 4 letras, sin NA ni palabras vacías, y algunas personalizadas)
word_counts1 <- word_counts %>%
  filter(nchar(word) > 3) %>%
  filter(!is.na(word) & word != "") %>%
  filter(!(word %in% c("dijo", "hacia", "entonces", "modo", "this", "luego", "aquí", "toda", "gran", "the", "gutenberg", "project", "después", "pues")))

# Gráfico de barras (top 25)
top_words <- word_counts1 %>%
  top_n(25, n)

ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Frecuencia de Palabras", x = "Palabras", y = "Frecuencia")

# Nube de palabras
wordcloud(words = word_counts1$word, 
          freq = word_counts1$n, 
          min.freq = 1, 
          max.words = 300, 
          random.order = FALSE, 
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# Mostrar densidad
print(paste("Densidad de Vocabulario:", vocab_density))

# Guardar resultados (opcional)
# write.csv(word_counts1, "word_counts.csv", row.names = FALSE)
