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
library(tidyr)
library(splitstackshape)


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

#---- 3.1 cuidado_tiempo----

frq(proc_encuesta$cuidado_tiempo)

get_label(proc_encuesta$cuidado_tiempo)

proc_encuesta$cuidado_tiempo <- set_label(x =proc_encuesta$cuidado_tiempo, label = 'Horas al día trabajo de cuidados')

proc_encuesta$cuidado_tiempo <- set_labels(proc_encuesta$cuidado_tiempo, 
                                           labels= c('Menos de 1 hr.'=1,
                                                     '1-3 hrs.'=2,
                                                     '3-5 hrs.'=3))
frq(proc_encuesta$cuidado_tiempo)


#---- 3.2 cuidado_1 ----

frq(proc_encuesta$cuidado_1)

proc_encuesta <- rename(proc_encuesta, "cuidado_comer" = cuidado_1)

proc_encuesta$cuidado_comer <- set_label(x = proc_encuesta$cuidado_comer,label = "Frecuencia dar de comer o amamantar")

proc_encuesta$cuidado_comer <- set_labels(proc_encuesta$cuidado_comer,
                                          labels= c('Nunca' = 1,
                                                    'Raramente' = 2,
                                                    'Ocasionalemente' = 3,
                                                    'Frecuentemente' = 4, 
                                                    'Muy frecuentemente' = 5))
frq(proc_encuesta$cuidado_comer)

#----3.3 cuidado_2 ----

frq(proc_encuesta$cuidado_2)

proc_encuesta <- rename(proc_encuesta, "cuidado_acostar" = cuidado_2)

proc_encuesta$cuidado_acostar <- set_label(x = proc_encuesta$cuidado_acostar,label = "Frecuencia acostar")

proc_encuesta$cuidado_acostar <- set_labels(proc_encuesta$cuidado_acostar,
                                          labels= c('Nunca' = 1,
                                                    'Raramente' = 2,
                                                    'Ocasionalemente' = 3,
                                                    'Frecuentemente' = 4, 
                                                    'Muy frecuentemente' = 5))
frq(proc_encuesta$cuidado_acostar)

#---- 3.4 cuidado_3 ----

frq(proc_encuesta$cuidado_3)

proc_encuesta <- rename(proc_encuesta, "cuidado_mudar" = cuidado_3)

proc_encuesta$cuidado_mudar <- set_label(x = proc_encuesta$cuidado_mudar,label = "Frecuencia mudar o llevar al baño")

proc_encuesta$cuidado_mudar <- set_labels(proc_encuesta$cuidado_mudar,
                                            labels= c('Nunca' = 1,
                                                      'Raramente' = 2,
                                                      'Ocasionalemente' = 3,
                                                      'Frecuentemente' = 4, 
                                                      'Muy frecuentemente' = 5))
frq(proc_encuesta$cuidado_mudar)

#---- 3.5 cuidado_4 ----

frq(proc_encuesta$cuidado_4)

proc_encuesta <- rename(proc_encuesta, "cuidado_asear" = cuidado_4)

proc_encuesta$cuidado_asear <- set_label(x = proc_encuesta$cuidado_asear,label = "Frecuencia bañar o asear")

proc_encuesta$cuidado_asear <- set_labels(proc_encuesta$cuidado_asear,
                                          labels= c('Nunca' = 1,
                                                    'Raramente' = 2,
                                                    'Ocasionalemente' = 3,
                                                    'Frecuentemente' = 4, 
                                                    'Muy frecuentemente' = 5))
frq(proc_encuesta$cuidado_asear)


#---- 3.6 cuidado_5 ----

frq(proc_encuesta$cuidado_5)

proc_encuesta <- rename(proc_encuesta, "cuidado_vestir" = cuidado_5)

proc_encuesta$cuidado_vestir <- set_label(x = proc_encuesta$cuidado_vestir,label = "Frecuencia vestir o arreglar")

proc_encuesta$cuidado_vestir <- set_labels(proc_encuesta$cuidado_vestir,
                                          labels= c('Nunca' = 1,
                                                    'Raramente' = 2,
                                                    'Ocasionalemente' = 3,
                                                    'Frecuentemente' = 4, 
                                                    'Muy frecuentemente' = 5))
frq(proc_encuesta$cuidado_vestir)

#---- 3.7 cuidado_6 ----

frq(proc_encuesta$cuidado_6)

proc_encuesta <- rename(proc_encuesta, "cuidado_aconsejar" = cuidado_6)

proc_encuesta$cuidado_aconsejar <- set_label(x = proc_encuesta$cuidado_aconsejar,label = "Frecuencia aconsejar")

proc_encuesta$cuidado_aconsejar <- set_labels(proc_encuesta$cuidado_aconsejar,
                                           labels= c('Nunca' = 1,
                                                     'Raramente' = 2,
                                                     'Ocasionalemente' = 3,
                                                     'Frecuentemente' = 4, 
                                                     'Muy frecuentemente' = 5))
frq(proc_encuesta$cuidado_aconsejar)


#---- 3.8 cuidado_7 ----

frq(proc_encuesta$cuidado_7)

proc_encuesta <- rename(proc_encuesta, "cuidado_salud" = cuidado_7)

proc_encuesta$cuidado_salud <- set_label(x = proc_encuesta$cuidado_salud,label = "Frecuencia dar medicamentos, algún tratamiento de salud o cuidar por alguna enfermedad")

proc_encuesta$cuidado_salud <- set_labels(proc_encuesta$cuidado_salud,
                                              labels= c('Nunca' = 1,
                                                        'Raramente' = 2,
                                                        'Ocasionalemente' = 3,
                                                        'Frecuentemente' = 4, 
                                                        'Muy frecuentemente' = 5))
frq(proc_encuesta$cuidado_salud)

#---- 3.9 cuidado_8 ----

frq(proc_encuesta$cuidado_8)

proc_encuesta <- rename(proc_encuesta, "cuidado_acompanar1" = cuidado_8)

proc_encuesta$cuidado_acompanar1 <- set_label(x = proc_encuesta$cuidado_acompanar1,label = "Frecuencia acompañar o llevar a algún centro de salud")

proc_encuesta$cuidado_acompanar1 <- set_labels(proc_encuesta$cuidado_acompanar1,
                                          labels= c('Nunca' = 1,
                                                    'Raramente' = 2,
                                                    'Ocasionalemente' = 3,
                                                    'Frecuentemente' = 4, 
                                                    'Muy frecuentemente' = 5))
frq(proc_encuesta$cuidado_acompanar1)

#---- 3.10 cuidado_9 ----

frq(proc_encuesta$cuidado_9)

proc_encuesta <- rename(proc_encuesta, "cuidado_acompanar2" = cuidado_9)

proc_encuesta$cuidado_acompanar2 <- set_label(x = proc_encuesta$cuidado_acompanar2,label = "Frecuencia acompañar o llevar a algún centro educacional")

proc_encuesta$cuidado_acompanar2 <- set_labels(proc_encuesta$cuidado_acompanar2,
                                               labels= c('Nunca' = 1,
                                                         'Raramente' = 2,
                                                         'Ocasionalemente' = 3,
                                                         'Frecuentemente' = 4, 
                                                         'Muy frecuentemente' = 5))

frq(proc_encuesta$cuidado_acompanar2)

#---- 3.11 cuidado_10 ----

frq(proc_encuesta$cuidado_10)

proc_encuesta <- rename(proc_encuesta, "cuidado_tareas" = cuidado_10)

proc_encuesta$cuidado_tareas <- set_label(x = proc_encuesta$cuidado_tareas,label = "Frecuencia ayudar con tareas escolares")

proc_encuesta$cuidado_tareas <- set_labels(proc_encuesta$cuidado_tareas,
                                               labels= c('Nunca' = 1,
                                                         'Raramente' = 2,
                                                         'Ocasionalemente' = 3,
                                                         'Frecuentemente' = 4, 
                                                         'Muy frecuentemente' = 5))

frq(proc_encuesta$cuidado_tareas)

#---- 3.12 cuidado_11 ----

frq(proc_encuesta$cuidado_11)

proc_encuesta <- rename(proc_encuesta, "cuidado_jugar" = cuidado_11)

proc_encuesta$cuidado_jugar <- set_label(x = proc_encuesta$cuidado_jugar,label = "Frecuencia jugar")

proc_encuesta$cuidado_jugar <- set_labels(proc_encuesta$cuidado_jugar,
                                           labels= c('Nunca' = 1,
                                                     'Raramente' = 2,
                                                     'Ocasionalemente' = 3,
                                                     'Frecuentemente' = 4, 
                                                     'Muy frecuentemente' = 5))

frq(proc_encuesta$cuidado_jugar)

#---- 3.13 cuidado_12 ----

frq(proc_encuesta$cuidado_12)

proc_encuesta <- rename(proc_encuesta, "cuidado_leer" = cuidado_12)

proc_encuesta$cuidado_leer <- set_label(x = proc_encuesta$cuidado_leer,label = "Frecuencia leer o contar cuentos")

proc_encuesta$cuidado_leer <- set_labels(proc_encuesta$cuidado_leer,
                                          labels= c('Nunca' = 1,
                                                    'Raramente' = 2,
                                                    'Ocasionalemente' = 3,
                                                    'Frecuentemente' = 4, 
                                                    'Muy frecuentemente' = 5))

frq(proc_encuesta$cuidado_leer)

#---- 3.14 cuidado_multiple_1 ----

frq(proc_encuesta$cuidado_multiple)

proc_encuesta <- cSplit(proc_encuesta,"cuidado_multiple",",") #Se transforman las respuestas a 3 variables distintas, pues son 3 respuestas.

frq(proc_encuesta$cuidado_multiple_1)

proc_encuesta$cuidado_multiple_1 <- set_label(x = proc_encuesta$cuidado_multiple_1,label = "Tarea de cuidado que toma más tiempo")

proc_encuesta$cuidado_multiple_1 <- set_labels(proc_encuesta$cuidado_multiple_1,
                                         labels= c('Dar de comer o amamantar' = 1,
                                                   'Acostar' = 2,
                                                   'Aconsejar' = 6))
frq(proc_encuesta$cuidado_multiple_1)

#---- 3.15 cuidado_multiple_2 ----

frq(proc_encuesta$cuidado_multiple_2)

proc_encuesta$cuidado_multiple_2 <- set_label(x = proc_encuesta$cuidado_multiple_2,label = "Segunda tarea de cuidado que toma más tiempo")

proc_encuesta$cuidado_multiple_2 <- set_labels(proc_encuesta$cuidado_multiple_2,
                                               labels= c('Acostar' = 2,
                                                         'Mudar o llevar al baño' = 3,
                                                         'Vestir o arreglar' = 5, 
                                                         'Aconsejar' = 6))

frq(proc_encuesta$cuidado_multiple_2)

#---- 3.16 cuidado_multiple_3 ----

frq(proc_encuesta$cuidado_multiple_3)

proc_encuesta$cuidado_multiple_3 <- set_label(x = proc_encuesta$cuidado_multiple_3,label = "Tercera tarea de cuidado que toma más tiempo")

proc_encuesta$cuidado_multiple_3 <- set_labels(proc_encuesta$cuidado_multiple_3,
                                               labels= c('Acompañar o llevar a algún centro de salud' = 8,
                                                         'Acompañar o llevar a algún centro educacional' = 9,
                                                         'Jugar' = 11))

frq(proc_encuesta$cuidado_multiple_3) 






