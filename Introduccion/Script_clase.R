# COMPLEMENTARIA 1
# Fecha: 19/06/2026
# Autor: David Florez



# Ejecuta esto UNA sola vez en tu computador
install.packages("pacman")
library(pacman)

p_load(tidyverse,  # manipulación y visualización de datos
       haven,      # leer archivos .dta (Stata), .sav (SPSS)
       readxl,     # leer archivos Excel
       knitr,      # tablas y compilación de documentos
       stargazer,  # tablas de regresión
       lmtest,     # pruebas sobre modelos lineales
       sandwich,   # errores estándar robustos
       ggplot2     # errores estándar robustos
       )   # errores estándar robustos


# Ver dónde está trabajando R ahora mismo
getwd()

# Cambiar el directorio de trabajo (ajusta la ruta a tu computador)
setwd("C:/Users/braya/OneDrive - Universidad de los Andes/Escritorio/U/TA/Econometrics 1 2026-19/Econometrics_1")



# Operaciones aritméticas básicas
2 + 3

15 / 4

sqrt(144)     # raíz cuadrada

log(exp(1))   # logaritmo natural



# Crear objetos
x <- 5
y <- 3
z <- x + y
z

# También puedes usar =, aunque <- es la convención para evitar confusiones
nombre <- "ML"
nombre 


# Numérico (numeric)
salario <- 3500000
class(salario)

# Texto / cadena (character)
ciudad <- "Bogota"
class(ciudad)


# Lógico (logical)
es_estudiante <- TRUE
class(es_estudiante)

# Entero (integer)
semestre <- 5L
class(semestre) 


# Crear vectores con c() (concatenar)
edades    <- c(20, 21, 19, 22, 20, 23)
nombres   <- c("Ana", "Luis", "Maria", "Carlos", "Juan", "Sofia")
en_beca   <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)


# Longitud del vector
length(edades)

# Acceder a elementos por posición (índice empieza en 1)
edades[3]       # primer elemento
edades[2:4]     # elementos del 2 al 4
edades[c(1,5)]  # elementos 1 y 5


# Las operaciones se aplican elemento a elemento
salarios <- c(2800000, 3500000, 4200000, 3100000, 2950000, 5000000)

# Transformaciones
salarios_millones <- salarios / 1e6
salarios_millones


# Estadísticas básicas
mean(salarios_millones)     # media

median(salarios_millones)   # mediana

sd(salarios_millones)     # desviación estándar 

var(salarios_millones)      # varianza

sum(salarios)      # suma


# ¿Cuáles edades son mayores a 20?
edades > 20

# Extraer solo los elementos que cumplen la condición
edades_sup_20 <- edades[edades > 20]


# Operadores lógicos:
# >  mayor que    |  <  menor que
# >= mayor o igual|  <= menor o igual
# == igual a      |  != diferente de
# &  Y (and)      |  |  O (or)
# !  NO (negación)

edades[edades >= 20 & edades <= 22]


# Crear una matriz
M <- matrix(1:9, nrow = 3, ncol = 3)
M

# Dimensiones
dim(M)

nrow(M)

ncol(M)

# Acceso a elementos: M[fila, columna]
M[2, 3]     # fila 2, columna 3

M[1, ]      # toda la fila 1

M[, 2]      # toda la columna 2




# Álgebra matricial (relevante para OLS)
A <- matrix(c(1, 2, 3, 4), nrow = 2)
B <- matrix(c(5, 6, 7, 8), nrow = 2)

A %*% B      # multiplicación matricial

t(A)         # transpuesta

solve(A)     # inversa (si existe)


# Crear un data frame
estudiantes <- data.frame(
  nombre    = c("Ana", "Luis", "Maria", "Carlos", "Juan", "Sofia"),
  semestre  = c(5, 5, 4, 5, 6, 4),
  promedio  = c(3.9, 3.5, 4.1, 3.2, 3.7, 4.3),
  beca      = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
)


# Ver el data frame
estudiantes

# Dimensiones
nrow(estudiantes)   # número de observaciones

# Nombres de las variables (columnas)
names(estudiantes)


# Primeras y últimas filas
head(estudiantes, 3)


# Resumen estadístico
summary(estudiantes)

# Con el operador $
estudiantes$promedio 


# Estudiantes con promedio mayor a 3.8
estudiantes_sub_3_8 <- estudiantes[estudiantes$promedio > 3.8, ]


# Con la función subset()
subset(estudiantes, beca == TRUE)


# Con tidyverse (filter) — esta es la forma que usaremos en el curso
filter(estudiantes, semestre == 5 & promedio > 3.5)


# Crear nueva columna
estudiantes$promedio_ponderado <- estudiantes$promedio * 1.1




# Con mutate (tidyverse) — esta es la forma que usaremos en el curso
estudiantes <- mutate(estudiantes,
                      es_excelente = promedio >= 4.0)
estudiantes


# Carpeta relativa al proyecto de RStudio — ajusten si es necesario
ruta_datos <- "Introduccion/datos"

ruta_datos <- "C:/Users/braya/OneDrive - Universidad de los Andes/Escritorio/U/TA/Econometrics 1 2026-19/Econometrics_1/Introduccion/datos"


sb11 <- read_dta(file.path(ruta_datos, "SB11_2019_Stata.dta"))

sb11_xl <- read_excel(file.path(ruta_datos, "SB11_2019_Excel.xlsx"))


dim(sb11)


nrow(sb11)   # observaciones


ncol(sb11)   # variables

names(sb11)  # nombres de columnas


head(sb11, 5)


summary(sb11)


sb11 <- sb11 |>
  mutate(
    punt_global          = as.numeric(punt_global),
    punt_matematicas     = as.numeric(punt_matematicas),
    punt_lectura_critica = as.numeric(punt_lectura_critica),
    EDAD_ESTUDIANTES     = as.numeric(EDAD_ESTUDIANTES),
    genero               = as.character(estu_genero),
    naturaleza           = as.character(cole_naturaleza),
    depto                = as.character(estu_depto_reside),
    internet             = as.character(fami_tieneinternet)
  )

sb11 <- sb11 |>
  mutate(
    # Variable dicotómica: 1 si es mujer
    mujer          = if_else(genero     == "F",       1L, 0L),
    # Variable dicotómica: 1 si el colegio es oficial
    oficial        = if_else(naturaleza == "OFICIAL", 1L, 0L),
    # Variable dicotómica: 1 si tiene internet en casa
    tiene_internet = if_else(internet   == "Si",      1L, 0L),
    # Diferencia respecto a la media nacional
    brecha_global  = punt_global - mean(punt_global, na.rm = TRUE)
  )


sb11 |>
  select(punt_global, punt_matematicas, punt_lectura_critica,
         EDAD_ESTUDIANTES, mujer, oficial, tiene_internet) |>
  summarise(across(everything(), ~ sum(is.na(.))))


n_antes <- nrow(sb11)

sb11<- sb11 |>
  filter(genero %in% c("F", "M"))

cat("Eliminados:", n_antes - nrow(sb11), "\n")


cat("Quedan:    ", nrow(sb11), "\n")


sb11 |>
  summarise(
    n       = n(),
    media   = mean(punt_global,   na.rm = TRUE),
    mediana = median(punt_global, na.rm = TRUE),
    sd      = sd(punt_global,     na.rm = TRUE),
    minimo  = min(punt_global,    na.rm = TRUE),
    maximo  = max(punt_global,    na.rm = TRUE)
  )



sb11 |>
  group_by(mujer, oficial) |>
  summarise(
    n            = n(),
    media_global = mean(punt_global, na.rm = TRUE),
    sd_global    = sd(punt_global,   na.rm = TRUE)
  )


resumen_depto <- sb11 |>
  group_by(depto) |>
  summarise(
    n            = n(),
    media_global = round(mean(punt_global, na.rm = TRUE), 1),
    sd_global    = round(sd(punt_global,   na.rm = TRUE), 1)
  ) |>
  arrange(desc(media_global)) |>
  head(10)

kable(
  resumen_depto,
  col.names = c("Departamento", "N", "Media puntaje global", "DE"),
  caption   = "Top 10 departamentos por puntaje global:  Saber 11 2019"
)


cor<- sb11 |>
  select(punt_matematicas, punt_lectura_critica,
         punt_c_naturales, punt_ingles, punt_global) |>
  cor(use = "complete.obs") |>
  round(3)






ggplot(sb11, aes(x = punt_global)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(
    title   = "Distribución del puntaje global — Saber 11 2019",
    x       = "Puntaje global",
    y       = "Frecuencia",
    caption = "Fuente: ICFES — Saber 11 2019. Elaboración propia."
  ) +
  theme_minimal()

sb11 |>
  mutate(Genero = if_else(mujer == 1, "Mujer", "Hombre")) |>
  ggplot(aes(x = punt_global, fill = Genero)) +
  geom_histogram(binwidth = 10, color = "white", alpha = 0.8) +
  facet_wrap(~ Genero) +
  labs(
    title   = "Distribución del puntaje global por género",
    x       = "Puntaje global",
    y       = "Frecuencia",
    caption = "Fuente: ICFES — Saber 11 2019. Elaboración propia."
  ) +
  theme_minimal() +
  theme(legend.position = "none")




sb11 |>
  mutate(Colegio = if_else(oficial == 1, "Oficial", "No oficial")) |>
  ggplot(aes(x = Colegio, y = punt_global, fill = Colegio)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title   = "Puntaje global por naturaleza del colegio",
    x       = NULL,
    y       = "Puntaje global",
    caption = "Fuente: ICFES — Saber 11 2019. Elaboración propia."
  ) +
  theme_minimal() +
  theme(legend.position = "none")




sb11 |>
  filter(!is.na(tiene_internet)) |>
  mutate(Internet = if_else(tiene_internet == 1, "Con internet", "Sin internet")) |>
  group_by(Internet) |>
  summarise(media_global = mean(punt_global, na.rm = TRUE)) |>
  ggplot(aes(x = Internet, y = media_global, fill = Internet)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(media_global, 1)), vjust = -0.5) +
  labs(
    title   = "Puntaje global promedio según acceso a internet en casa",
    x       = NULL,
    y       = "Puntaje global promedio",
    caption = "Fuente: ICFES — Saber 11 2019. Elaboración propia."
  ) +
  theme_minimal() +
  theme(legend.position = "none")




ggplot(sb11, aes(x = punt_matematicas, y = punt_lectura_critica)) +
  geom_point(alpha = 0.05, size = 0.6, color = "red") +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  labs(
    title    = "Matemáticas vs. Lectura crítica — Saber 11 2019",
    subtitle = "La línea azul es la regresión OLS (tema de las próximas semanas)",
    x        = "Puntaje matemáticas",
    y        = "Puntaje lectura crítica",
    caption  = "Fuente: ICFES — Saber 11 2019. Elaboración propia."
  ) +
  theme_minimal()


