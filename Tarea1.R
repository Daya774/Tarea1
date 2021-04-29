# 1. Creación del proyecto e importación de datos.

library(readxl)
Tarea1 <- read_excel("Tarea1.xlsx")
View(Tarea1)

#2 Cuantas columnas tiene la tabla? Que tipo de dato contiene cada columna?

head(Tarea1) # Tiene 7 columnas
dim(Tarea1)
str(Tarea1)
class(Tarea1) # Es un data frame
summary(Tarea1)

#3 Cambie el nombre de columnas para eliminar la tilde de la palabra “Ubicación”
# y dejar solo el nombre de los hongos (eliminar el “UFC”)

colnames(Tarea1) = c("Ubicacion","Tratamiento","Bloque","Fusarium","Rhizoctonia",
                     "Pythium",
                     "Phytophthora")
head(Tarea1)

#4 Modifique la tabla para obtenerla en formato largo agrupando en una columna
#llamada “Hongo” las columnas "Fus", "Rhizoc", "Pythium" y "Phyto" y en otra 
# columna los valores de UFC. [Pista:
# puede utilizar pivot_longer() ]

data_longer <- Tarea1%>% 

pivot_longer(cols = c(Fusarium, Rhizoctonia, Pythium, Phytophthora),
             names_to = "Hongo",
             values_to = "UFC")
data_longer

#5 Agrupe y sumarise por ubicación, tratamiento, hongo; y calcule el promedio, 
# min, max y desviación estándar para UFC.

data_longer %>%
  group_by(Ubicacion, Tratamiento, Hongo) %>%
  summarise(UFC_prom = mean(UFC),
            UFC_max = max(UFC),
            UFC_desvest = sd(UFC))

data_longer %>%
  group_by(Ubicacion) %>%
  summarise(UFC_prom = mean(UFC),
            UFC_max = max(UFC),
            UFC_desvest = sd(UFC))

data_longer %>%
  group_by(Tratamiento) %>%
  summarise(UFC_prom = mean(UFC),
            UFC_max = max(UFC),
            UFC_desvest = sd(UFC))

data_longer %>%
  group_by(Hongo) %>%
  summarise(UFC_prom = mean(UFC),
            UFC_max = max(UFC),
            UFC_desvest = sd(UFC))

#6 Utilize ggplot para obtener boxplots con Tratamiento en “eje x” y UFC en 
# “eje y”. Agregue otra capa con geom_jitter().

library(ggplot2)

ggplot(data_longer, aes(x= Tratamiento, y= UFC)) +
  geom_boxplot(color = "red", outlier.alpha = 0) +
  geom_jitter(width = 0.3)

#7 Cambie el color de los boxplot a azul. Cambie el ancho de jitter a 
# width=0.2 y agregue diferentes colores de jitter dependiendo del tratamiento.  

ggplot(data_longer, aes(x= Tratamiento, y= UFC)) +
  geom_boxplot(color = "blue", outlier.alpha = 0) +
  geom_jitter(aes(color = Tratamiento), width = 0.2, size = 4)

#8 - Observe que los outliers del boxplot aparecen en azul y que están 
# duplicados con algunos puntos de la capa geom_jitter

# Haga transparentes los outliers del boxplot. [Pista: utilice outlier.alpha]

ggplot(data_longer, aes(x= Tratamiento, y= UFC)) +
  geom_boxplot(color = "blue", outlier.alpha = 2) +
  geom_jitter(aes(color = Tratamiento), width = 0.2, size = 4)

#9 Agregue una faceta para cada Hongo y cambie el boxplot por una capa de 
# violin color verde.

ggplot(data_longer, aes(x= Tratamiento, y= UFC)) +
  geom_violin(color = "green") + 
  facet_wrap(~Hongo)

#10 Ajuste el rango del eje "y" para que se observen mejor los datos agregando
# una escala libre.

ggplot(data_longer, aes(x= Tratamiento, y= UFC)) +
  geom_violin(color = "green") + 
  facet_wrap(~Hongo,
             scales = "free_y")

# Cambie las etiquetas de cada panel para que tenga el nombre completo del 
#hongo: Fusarium, Phytophthora, Pythium, Rhizoctonia. Ya tiene los nombres 
# completos.

#11 Agregue el tema theme_economist() del paquete ggthemes

library(ggthemes)

ggplot(data_longer, aes(x= Tratamiento, y= UFC)) +
  geom_violin(color = "green") + 
  facet_wrap(~Hongo,
             scales = "free_y") +
  
  theme_economist()

#12 Ahora, en vez de usar facetas, filtre los datos para cada hongo. 
# Genere 4 figuras llamadas f1, f2, f3 y f4.

library(ggplot2)

ggplot(tarea1 = data_longer, aes(x = Tratamiento, y = UFC)) +
  geom_boxplot(color = "blue", outlier.alpha = 0) +
  geom_jitter(aes(alpha(Fusarium), width = 0.2, size = 4) +
  
  


#13 Agregue etiquetas A, B, C y D para cada panel, y asígnelo a un objeto 
# llamado “f”



#14 Salve la figura con el nombre "Figura1", dimensiones de 178mm de ancho, 
# 185mm de alto, y resolución 450dpi.


