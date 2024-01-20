# INSTALAR LIBRERIAS
install.packages("readxl")
install.packages("ragg")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
options(repos = c(CRAN = "https://cloud.r-project.org"))

# LLAMAR LIBRERIAS
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

#leer los archivos
file.choose()
octubre_2023 <- read.csv("https://raw.githubusercontent.com/kalliche/ACA_III/main/Octubre%202023%20exportaciones%20CoL1.csv")
septiembre_2023 <- read.csv("https://raw.githubusercontent.com/kalliche/ACA_III/main/Septiembre%202023%20exportaciones%20Col%201.csv")

summary(octubre_2023)

# Obtener nombres de columnas y tipos de datos en octubre_2023
nombres_octubre <- names(octubre_2023)
tipos_octubre <- sapply(octubre_2023, class)

# Imprimir nombres y tipos de datos en octubre_2023
cat("\nColumnas y tipos de datos en octubre_2023:\n")
for (i in seq_along(nombres_octubre)) {
  cat(sprintf("%s: %s\n", nombres_octubre[i], tipos_octubre[i]))
}

# Obtener nombres de columnas y tipos de datos en septiembre_2023
nombres_septiembre <- names(septiembre_2023)
tipos_septiembre <- sapply(septiembre_2023, class)

# Imprimir nombres y tipos de datos en septiembre_2023
cat("\nColumnas y tipos de datos en septiembre_2023:\n")
for (i in seq_along(nombres_septiembre)) {
  cat(sprintf("%s: %s\n", nombres_septiembre[i], tipos_septiembre[i]))
}

# explorar y limpiar datos
summary(octubre_2023)
head(octubre_2023)
str(octubre_2023)

summary(septiembre_2023)
head(septiembre_2023)
str(septiembre_2023)

# realizar analisis
summary(septiembre_2023$FOBDOL)
summary(octubre_2023$FOBDOL)

# Comparación de medias de FOBDOL
t.test(septiembre_2023$FOBDOL, octubre_2023$FOBDOL)

# Top 5 y 10 por COD_PAI4 y FOBDOL
top_5_fobdol <- datos_combinados %>%
  group_by(COD_PAI4) %>%
  arrange(desc(FOBDOL)) %>%
  slice_head(n = 5)

top_10_fobdol <- datos_combinados %>%
  group_by(COD_PAI4) %>%
  arrange(desc(FOBDOL)) %>%
  slice_head(n = 10)

# Gráfico para el Top 5 por COD_PAI4 y FOBDOL
ggplot(filter(datos_combinados, COD_PAI4 %in% top_5_fobdol$COD_PAI4), 
       aes(x = COD_PAI4, y = FOBDOL, fill = Mes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 5 de FOBDOL por COD_PAI4 entre Octubre y Septiembre 2023", 
       x = "COD_PAI4", y = "FOBDOL")

# Gráfico para el Top 10 por COD_PAI4 y FOBDOL
ggplot(filter(datos_combinados, COD_PAI4 %in% top_10_fobdol$COD_PAI4), 
       aes(x = COD_PAI4, y = FOBDOL, fill = Mes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 de FOBDOL por COD_PAI4 entre Octubre y Septiembre 2023", 
       x = "COD_PAI4", y = "FOBDOL")

# Combinar todos los registros de COD_PAI4 y sumar los valores numéricos
summarized_data <- datos_combinados %>%
  group_by(COD_PAI4) %>%
  summarise(
    Total_Registros = n(),
    Suma_FOBDOL = sum(FOBDOL),
    Suma_FOBPES = sum(FOBPES),
    Suma_CANTI = sum(CANTI),
    Suma_AGRENA = sum(AGRENA),
    Suma_FLETES = sum(FLETES),
    Suma_SEGURO = sum(SEGURO),
    Suma_OTROSG = sum(OTROSG)
  )

# Mostrar la tabla resumen
print(summarized_data)

# Gráfico de líneas para el resumen de variables
ggplot(summarized_data, aes(x = COD_PAI4, group = 1)) +
  geom_line(aes(y = Suma_FOBDOL, color = "FOBDOL"), size = 1) +
  geom_line(aes(y = Suma_FOBPES, color = "FOBPES"), size = 1) +
  geom_line(aes(y = Suma_CANTI, color = "CANTI"), size = 1) +
  geom_line(aes(y = Suma_AGRENA, color = "AGRENA"), size = 1) +
  geom_line(aes(y = Suma_FLETES, color = "FLETES"), size = 1) +
  geom_line(aes(y = Suma_SEGURO, color = "SEGURO"), size = 1) +
  geom_line(aes(y = Suma_OTROSG, color = "OTROSG"), size = 1) +
  labs(title = "Resumen de Variables por COD_PAI4", x = "COD_PAI4", y = "Suma") +
  scale_color_manual(values = c("FOBDOL" = "skyblue", "FOBPES" = "salmon", "CANTI" = "lightgreen", 
                                "AGRENA" = "purple", "FLETES" = "orange", "SEGURO" = "pink", "OTROSG" = "lightblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crear un nuevo dataframe con la suma de valores por COD_PAI4
nuevo_dataframe <- datos_combinados %>%
  group_by(COD_PAI4) %>%
  summarise(
    Total_FOBDOL = sum(FOBDOL),
    Total_FOBPES = sum(FOBPES),
    Total_CANTIDAD = sum(CANTI),
    Total_FLETES = sum(FLETES),
    Total_SEGURO = sum(SEGURO),
    Total_OTROSG = sum(OTROSG)
  )

# Mostrar el nuevo dataframe
print(nuevo_dataframe)

############################

# Combinar y sumar por COD_PAI4 (solo para columnas numéricas)
datos_combinados_suma <- datos_combinados %>%
  group_by(COD_PAI4) %>%
  summarise_if(is.numeric, sum)

##########################
# 5 paises con mayor exportaciones
########################

# Filtrar datos para los países especificados
paises_seleccionados <- c("IND", "ECU", "CHN", "PAN", "USA")

datos_paises <- datos_combinados %>%
  filter(COD_PAI4 %in% paises_seleccionados)

# Sumar los valores de FOBDOL y FOBPES por país
totales_paises <- datos_paises %>%
  group_by(COD_PAI4) %>%
  summarise(
    Total_FOBDOL = sum(FOBDOL),
    Total_FOBPES = sum(FOBPES)
  )

# Ver los resultados
print(totales_paises)

# Gráfica de barras
ggplot(totales_paises, aes(x = COD_PAI4, y = Total_FOBDOL + Total_FOBPES, fill = COD_PAI4)) +
  geom_bar(stat = "identity") +
  labs(title = "Totales de FOBDOL + FOBPES por país",
       x = "País",
       y = "Total FOBDOL + FOBPES")





