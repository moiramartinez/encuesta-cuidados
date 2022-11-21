# Procesamiento bases de datos para el análisis del trabajo de cuidados en FACSO
# Moira Martínez, Catalina Navia y Ambar Santander
# Curso Encuestas Sociales, Noviembre 2022

#----1. LIBRERÍAS Y BASE DE DATOS ----

options(scipen=999) 

library(dplyr)
library(stargazer)

# Base de datos de caracterización

carac <- read.csv("input/caracterizacion.csv")

stargazer(carac, type = "text")

# Base de datos trabajo de cuidados

cuidados <- read.csv("input/grupo2.csv")

stargazer(cuidados, type="text")

#----2. SELECCIÓN DE VARIABLES ----

# Comprobar nombres de las variables en cada base de datos.

names(carac)
names(cuidados)


# Se deja fueran fuera las variables que no aportan al análisis o no son relevantes en esta oportunidad.

# Selección de variables de interés carac


proc_carac <- carac %>% select(session, satisfaccion_vida, familia_1, familia_2, familia_3, familia_3b, familia_4, 
                               e_civil, edad, carrera, anho_curs, educ_padre, educ_madre) 

# Selección de variables de interés cuidados

proc_cuidados <- cuidados %>% select(session, pregunta1, cuidado_tiempo, cuidado_1, cuidado_2, cuidado_3, cuidado_4, cuidado_5, 
                                     cuidado_6, cuidado_7, cuidado_8, cuidado_9, cuidado_10, cuidado_11, cuidado_12, cuidado_multiple)







