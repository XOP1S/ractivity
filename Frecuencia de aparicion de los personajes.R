library(tokenizers)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)


#Mirar cual es el folder de trabajo actual y asegurarse que en el est? el documento de la obra
getwd()

#Lee el texto desde el archivo "melville.txt" y lo coloca en un vector
text.v <- scan("C:/Users/oscar/Desktop/Rtarea/melville.txt", what="character", sep="\n")

# Guarda lal?nea del texto en donde inicia la obra en start.v
start.v <- which(text.v == "--1--")
start.v

# Guarda lal?nea del texto en donde finaliza la obra en start.v
end.v <- which(text.v == "FIN DE LA NOVELA")
end.v

# Las siguientes l?neas guardan en variables el texto del metadato de la obra
# El metadato tiene dos partes, una al principio de la obra y una al final
# El metadato del principio de la obra inicia en la l?nea 1 y finaliza en la l?nea anterior a que comience la obra (start.v -1)

start.metadata.v <- text.v[1:start.v -1]
start.metadata.v

# El metadato del final de la obra inicia en la l?nea (end.v+1) y finaliza en la ?ltima de la obra
end.metadata.v <- text.v[(end.v+1):length(text.v)]
end.metadata.v

# Colocamos todo el metadato de la obra en la variable (metadata.v)
metadata.v <- c(start.metadata.v, end.metadata.v)
metadata.v

# Ahora, las lineas de la novela son las que se encuentra entre la l?nea de inicio (start.v) y la de fin de la obra (end.v)
# Colocaremos est?s l?neas en la variable (novel.lines.v)
novel.lines.v <-  text.v[start.v:end.v]

class(novel.lines.v)

# La variable novel.lines.v almacena en l?neas la novela, con la funci?n paste se uniran todas las l?neas de la novela, separandolas con un espacio " "
novel.v <- paste(novel.lines.v, collapse=" ")

class(novel.v)

# La funcion tolower transforma el texto en min?scula
novel.lower.v <- tolower(novel.v)

#Frecuencia de aparicion de los personajes principales----------------------------------------------------
indices_tomos <- c(gregexpr("--[0-9]+--", novel.lower.v)[[1]], nchar(novel.lower.v) + 1)
indices_tomos

# Dividir el texto en tomos utilizando los índices encontrados
tomos <- lapply(1:(length(indices_tomos) - 1), function(i) {
  tomo_texto <- substr(novel.lower.v, indices_tomos[i], indices_tomos[i + 1] - 1)
  return(tomo_texto)
})

# Mostrar los tomos por separado
for (i in 1:length(tomos)) {
  cat("Capitulo ", i, ":\n")
  cat(tomos[[i]], "\n\n")
}

lista_personajes <- c("ozores", "fermín", "magistral","álvaro","víctor","regenta")

# Crear una lista para almacenar las frecuencias de menciones de cada personaje en cada tomo
frecuencia_por_capitulo <- lapply(tomos, function(tomo) {
  sapply(lista_personajes, function(personaje) {
    count <- length(grep(personaje, tomo, ignore.case = TRUE))
    return(count)
  })
})

# Convertir la lista en un data frame con los resultados
tabla_frecuencia_por_capitulo <- data.frame(do.call(rbind, frecuencia_por_capitulo))
colnames(tabla_frecuencia_por_capitulo) <- lista_personajes

# Mostrar la tabla de frecuencia por capítulo
print(tabla_frecuencia_por_capitulo)

# Agregar la columna de números de capítulo al data frame original
tabla_frecuencia_por_capitulo$Capitulo <- seq_along(frecuencia_por_capitulo)

# Convertir el data frame a formato largo (long format) para el gráfico
tabla_frecuencia_por_capitulo_long <- tidyr::gather(tabla_frecuencia_por_capitulo, Personaje, Frecuencia, -Capitulo)

# Cargar la librería ggplot2 si aún no lo has hecho

library(ggplot2)

# Crear el gráfico de puntos con líneas de tendencia
ggplot(data = tabla_frecuencia_por_capitulo_long, aes(x = Capitulo, y = Frecuencia, color = Personaje)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Frecuencia de menciones de personajes principales en La Regenta",
       x = "Capítulo",
       y = "Frecuencia de menciones",
       color = "Personaje") +
  theme_minimal()

