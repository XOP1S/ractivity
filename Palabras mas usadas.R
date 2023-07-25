library(tokenizers)
library(tidyverse)
library(dplyr)
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
novel.lines.v

class(novel.lines.v)

# La variable novel.lines.v almacena en l?neas la novela, con la funci?n paste se uniran todas las l?neas de la novela, separandolas con un espacio " "
novel.v <- paste(novel.lines.v, collapse=" ")
novel.v

class(novel.v)

oraciones <- tokenize_sentences(novel.v)
oraciones

#Palabras mas usadas con longitud mayor a 4--------------------------------------------------------------
palabras <- tokenize_words(novel.v)
palabras

tabla <- table(palabras[[1]])
tabla <- data_frame(word = names(tabla), count = as.numeric(tabla))
tabla_filtrada <- tabla %>%
  filter(nchar(word) > 4)
tabla <- arrange(tabla_filtrada, desc(count))
tabla

