# Procesamiento bases de datos para el análisis del trabajo de cuidados en FACSO
# Moira Martínez, Catalina Navia y Ambar Santander
# Curso Encuestas Sociales, Noviembre 2022

#----1. LIBRERÍAS Y BASE DE DATOS ----

options(scipen=999) 

library(dplyr)
library(stargazer)
library(sjmisc)
library(car)
library(sjlabelled)

# Base de datos de caracterización

carac <- read.csv("input/caracterizacion.csv")

stargazer(carac, type = "text")

# Base de datos trabajo de cuidados

cuidados <- read.csv("input/grupo2.csv")

stargazer(cuidados, type="text")

#----2. SELECCIÓN DE VARIABLES Y UNION DE BASES DE DATOS  ----

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

# Unión de ambas bases

proc_encuesta <- merge(proc_cuidados, proc_carac, by='session')


#----3. PROCESAMIENTO DE VARIABLES----
#---- 3.1 pregunta1 ----

frq(proc_encuesta$pregunta1) # Revisar la variable

proc_encuesta$pregunta1 <- na_if(proc_encuesta$pregunta1, 3) # El valor 3 corresponde a No sabe/No responde, se agrega a las NA

proc_encuesta$pregunta1 <- car::recode(proc_encuesta$pregunta1, "c(1) = 0; c(2) = 1" ) # Se asigna el valor 0 a si no hizo trabajo de cuidado, 1 sí hizo.

proc_encuesta <- rename(proc_encuesta, "si_no_cuidado"= pregunta1) #Cambiar nombre a uno más intuitivo

get_label(proc_encuesta$si_no_cuidado) # La variable no tiene etiqueta, se le pondrá una.

proc_encuesta$si_no_cuidado <- set_label(x = proc_encuesta$si_no_cuidado, label = 'Tareas de cuidado última semana')


proc_encuesta$si_no_cuidado <- set_labels(proc_encuesta$si_no_cuidado,
                                          labels = c('No'=0,
                                                     'Sí'=1)) # Etiquetado categorías de respuesta
frq(proc_encuesta$si_no_cuidado) # Revisión final


