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
library(splitstackshape)
library(tidyr)


# Base de datos de caracterización

carac <- read.csv("input/caracterizacion.csv")

stargazer(carac, type = "text")

# Base de datos trabajo de cuidados

cuidados <- read.csv("input/grupo2.csv")

stargazer(cuidados, type="text")

# Base de datos sexualidad e identidad de género

genero <- read.csv("input/grupo4.csv")

stargazer(genero, type="text")

#----2. SELECCIÓN DE VARIABLES Y UNION DE BASES DE DATOS  ----

# Comprobar nombres de las variables en cada base de datos.

names(carac)
names(cuidados)
names(genero)

# Se deja fueran fuera las variables que no aportan al análisis o no son relevantes en esta oportunidad.

# Selección de variables de interés carac


proc_carac <- carac %>% select(session, satisfaccion_vida, familia_1, familia_2, familia_3, familia_3b, familia_4, 
                               e_civil, edad, carrera, anho_curs, educ_padre, educ_madre) 

# Selección de variables de interés cuidados

proc_cuidados <- cuidados %>% select(session, pregunta1, cuidado_tiempo, cuidado_1, cuidado_2, cuidado_3, cuidado_4, cuidado_5, 
                                     cuidado_6, cuidado_7, cuidado_8, cuidado_9, cuidado_10, cuidado_11, cuidado_12, cuidado_multiple)


# Selección variables de interés genero

proc_genero <- genero %>% select (session, id_genero, id_genero_otra, sex_asignado)

# Unión de ambas bases

proc_encuesta <- merge(proc_cuidados, proc_carac, by='session')

proc_encuesta <- merge(proc_encuesta, proc_genero, by='session')


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

#---- 3.17 satisfaccion_vida ----

frq(proc_encuesta$satisfaccion_vida)

proc_encuesta$satisfaccion_vida <- set_label(x = proc_encuesta$satisfaccion_vida, 
                                             label = "Satisfacción con la vida (1 a 10)")

proc_encuesta$satisfaccion_vida <- set_labels(proc_encuesta$satisfaccion_vida,
                                              labels = c('Extremadamente insatisfecho' = 1,
                                                         'Extremadamente satisfecho' = 10))

frq(proc_encuesta$satisfaccion_vida)

#---- 3.18 familia_1 ----

frq(proc_encuesta$familia_1)

proc_encuesta <- rename(proc_encuesta, "miembros_hogar" = familia_1)

proc_encuesta$miembros_hogar <- set_label(x = proc_encuesta$miembros_hogar, 
                                             label = "Número de miembros en el hogar")

frq(proc_encuesta$miembros_hogar)

#---- 3.19 familia_2 ----

frq(proc_encuesta$familia_2)

proc_encuesta <- rename(proc_encuesta, "miembros_cuidado" = familia_2)

proc_encuesta$miembros_cuidado <- car::recode(proc_encuesta$miembros_cuidado, "c(1) = 1; c(2) = 0" )

proc_encuesta$miembros_cuidado <- set_labels(proc_encuesta$miembros_cuidado,
                                          labels = c('No'=0,
                                                     'Sí'=1))

proc_encuesta$miembros_cuidado <- set_label(x = proc_encuesta$miembros_cuidado, 
                                          label = "Miembro del hogar requiere cuidados")

frq(proc_encuesta$miembros_cuidado)

#----3.20 familia_3 ----

frq(proc_encuesta$familia_3)

proc_encuesta <- rename(proc_encuesta, "hijos" = familia_3)

proc_encuesta$hijos <- car::recode(proc_encuesta$hijos, "c(1) = 1; c(2) = 0" )

proc_encuesta$hijos <- set_labels(proc_encuesta$hijos,
                                             labels = c('No'=0,
                                                        'Sí'=1))

proc_encuesta$hijos <- set_label(x = proc_encuesta$hijos, 
                                            label = "Tiene hijos")

frq(proc_encuesta$hijos)

#---- 3.21 familia_3b ----

frq(proc_encuesta$familia_3b)

proc_encuesta <- rename(proc_encuesta, "cantidad_hijos" = familia_3b)

proc_encuesta$cantidad_hijos <- set_label(x = proc_encuesta$cantidad_hijos, 
                                 label = "Número de hijos")

frq(proc_encuesta$cantidad_hijos)

#---- 3.22 familia_4 ----

frq(proc_encuesta$familia_4)

proc_encuesta <- rename(proc_encuesta, "vive_padres" = familia_4)

proc_encuesta$vive_padres <- car::recode(proc_encuesta$vive_padres, "c(1) = 1; c(2) = 0" )

proc_encuesta$vive_padres <- set_labels(proc_encuesta$vive_padres,
                                  labels = c('No'=0,
                                             'Sí'=1))

proc_encuesta$vive_padres <- set_label(x = proc_encuesta$vive_padres, 
                                          label = "Vive con sus padres")

frq(proc_encuesta$vive_padres)

#---- 3.23 e_civil ----

frq(proc_encuesta$e_civil)

proc_encuesta$e_civil <- set_label(x = proc_encuesta$e_civil, 
                                       label = "Estado civil")

proc_encuesta$e_civil <- set_labels(proc_encuesta$e_civil,
                                        labels = c('Soltero'=1,
                                                   'Casado'=2,
                                                   'Otro' =6))

frq(proc_encuesta$e_civil)

#---- 3.24 edad ----

frq(proc_encuesta$edad)

proc_encuesta$edad <- na_if(proc_encuesta$edad, 4)
proc_encuesta$edad <- na_if(proc_encuesta$edad, 99)

proc_encuesta$edad <- set_label(x = proc_encuesta$edad, 
                                   label = "Edad")

frq(proc_encuesta$edad)

#----3.25 carrera ----

frq(proc_encuesta$carrera)

proc_encuesta$carrera <- set_label(x = proc_encuesta$carrera, 
                                label = "Carrera")

proc_encuesta$carrera <- set_labels(proc_encuesta$carrera,
                                    labels = c('Antropologia'=1,
                                               'Educacion Parvularia'=2,
                                               'Psicologia' =3,
                                               'Sociologia'= 4,
                                               'Trabajo Social' = 5))

frq(proc_encuesta$carrera)

#---- 3.26 anho_curs ----

frq(proc_encuesta$anho_curs)

proc_encuesta <- rename(proc_encuesta, "ano_ingreso" = anho_curs)

proc_encuesta$ano_ingreso <- na_if(proc_encuesta$ano_ingreso, 3)

proc_encuesta$ano_ingreso <- set_label(x = proc_encuesta$ano_ingreso, 
                                   label = "Año ingreso carrera")

frq(proc_encuesta$ano_ingreso)

#---- 3.27 educ_padre ----

frq(proc_encuesta$educ_padre)

proc_encuesta$educ_padre <- set_label(x = proc_encuesta$educ_padre, 
                                      label = "Nivel educacional padre")

proc_encuesta$educ_padre <- set_labels(proc_encuesta$educ_padre,
                                    labels = c('Ed. Basica'=1,
                                               'Ed. Media'=2,
                                               'Ed. Tecnica' =3,
                                               'Ed. Universitaria'= 4,
                                               'Postgrado' = 5))

frq(proc_encuesta$educ_padre)

#---- 3.28 educ_madre ----

frq(proc_encuesta$educ_madre)

proc_encuesta$educ_madre <- set_label(x = proc_encuesta$educ_madre, 
                                      label = "Nivel educacional madre")

proc_encuesta$educ_madre <- set_labels(proc_encuesta$educ_madre,
                                       labels = c('Ed. Basica'=1,
                                                  'Ed. Media'=2,
                                                  'Ed. Tecnica' =3,
                                                  'Ed. Universitaria'= 4,
                                                  'Postgrado' = 5))

frq(proc_encuesta$educ_madre)

#---- 3.29 id_genero ----

frq(proc_encuesta$id_genero)

proc_encuesta$id_genero <- set_label(x = proc_encuesta$id_genero,
                                     label= "Identidad de género")

proc_encuesta$id_genero <- set_labels(proc_encuesta$id_genero,
                                      labels = c('Mujer cis' = 1,
                                                'Hombre cis' = 3,
                                                'No binarie' = 5,
                                                'Otra'= 6 ))

frq(proc_encuesta$id_genero)


#----3.30 id_genero_otra ----

frq(proc_encuesta$id_genero_otra)

proc_encuesta[proc_encuesta=="thank the formr monkey"] <- NA

proc_encuesta$id_genero_otra <- set_label(x = proc_encuesta$id_genero_otra,
                                     label= "Si respondió otra, ¿Cuál es su identidad de género?")

frq(proc_encuesta$id_genero_otra)

#---- 3.31 

frq(proc_encuesta$sex_asignado)

proc_encuesta$sex_asignado <- na_if(proc_encuesta$sex_asignado, 3)

proc_encuesta$sex_asignado <- car::recode(proc_encuesta$sex_asignado, "c(1) = 0; c(2) = 1" )

proc_encuesta$sex_asignado <- set_labels(proc_encuesta$sex_asignado,
                                        labels = c('Hombre'=0,
                                                   'Mujer'=1))

proc_encuesta$sex_asignado <- set_label(x = proc_encuesta$sex_asignado,
                                        label= 'Sexo asignado')

frq(proc_encuesta$sex_asignado)


#---- 4. CASOS PERDIDOS ----

# Se eliminarán los casos que no tienen un identificador de sesión ('session')

proc_encuesta <- proc_encuesta[-which(proc_encuesta$session == ""), ]

#---- 5. GENERACIÓN DE BASE DE DATOS PROCESADA -----

# Revisión

proc_encuesta <-as.data.frame(proc_encuesta)
stargazer(proc_encuesta, type="text")

# Guardar

save(proc_encuesta, file = "proc/encuesta_cuidados.RData")
