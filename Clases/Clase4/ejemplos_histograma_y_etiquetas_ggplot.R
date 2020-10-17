library(ggplot2)
library(dplyr)
library(nycflights13)

set.seed(15)
data<-data.frame(x = rnorm(1000))



data %>%
  ggplot(aes(x)) +
  geom_histogram(
    bins = 10, # número de barras
    fill = "#849463",#color del relleno de la barra (notar que tb se puede asignar como RGB)
    col = "green" # color del contorno de la barra (también se podría asignar como RGB)
    ) +
  stat_bin(
    bins = 10, # stat_bin permite generar cortes en la variable del eje X
    geom ='label', # tipo de geometría a indicar en la etiqueta (puede ser text, label, point, y cualquiera de las capas que están disponibles) )
    color='blue', # color de la etiqueta
    mapping = aes(label=..count..), # mapeo estético que queremos visualizar en cada bin. En este caso es el conteo dentro de cada bin
    position = position_stack(vjust = 1) # posición relativa dentro de cada barra
    )


# Adicionalmente, si se quiere mostrar el histograma de frecuencia --------
#  es lo mismo pero se añade y = ..density.. en el maepo estético del histograma

data %>%
  ggplot(aes(x,y = ..density..)) + # se añade y = ..density.. para visualizar las frecuencias
  geom_histogram(
    bins = 10, # número de barras
    fill = "#849463",#color del relleno de la barra (notar que tb se puede asignar como RGB)
    col = "green" # color del contorno de la barra (también se podría asignar como RGB)
  ) +
  stat_bin(
    bins = 10, # stat_bin permite generar cortes en la variable del eje X
    geom ='label', # tipo de geometría a indicar en la etiqueta (puede ser text, label, point, y cualquiera de las capas que están disponibles) )
    color='blue', # color de la etiqueta
    mapping = aes(label=..count..), # mapeo estético que queremos visualizar en cada bin. En este caso es el conteo dentro de cada bin
    position = position_stack(vjust = 1) # posición relativa dentro de cada barra
  )


# Orientación etiquetas ejes ----------------------------------------------


data(flights)

# A modo de ejemplo podemos visulizar el total de vuelos por día .

flights %>%
  group_by(year,month,day) %>%
  summarise(total_vuelos = n()) %>%
  mutate(fecha = as.Date(paste0(year,
                                ifelse(nchar(month) == 1, paste0("0",month),month),
                                ifelse(nchar(day) == 1, paste0("0",day),day)
                                ),
                         format = "%Y%m%d"
                         )
         ) %>%
  ggplot() +
  aes(x = fecha, y = total_vuelos) + 
  geom_line() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .5,hjust = 1)
    ) 

# La función element_text() permite modificar orientación, y ajste vertical y hoizontal



