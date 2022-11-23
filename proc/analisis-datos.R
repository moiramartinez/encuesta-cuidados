# Análisis del trabajo de cuidados estudiantes de FACSO 2022
# Moira Martínez, Catalina Navia y Ambar Santander
# Curso Encuestas Sociales, Noviembre 2022

#----1. LIBRERÍAS Y BASE DE DATOS ----

library(sjPlot)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(sjlabelled)


load('proc/encuesta_cuidados.RData') # El nombre de la base es proc_encuesta

summary(proc_encuesta) # Revisión general

names(proc_encuesta)

# Variables:
# Session
# si_no_cuidado
# cuidado_tiempo
# cuidado_comer
# cuidado_acostar
# cuidado_mudar
# cuidado_asear
# cuidado_vestir
# cuidado_aconsejar
# cuidado_salud
# cuidado_acompanar1
# cuidado_acompanar2
# cuidado_tareas
# cuidado_jugar
# cuidado_leer
# cuidado_multiple_1
# cuidado_multiple_2
# cuidado_multiple_3
# satisfaccion_vida
# miembros_hogar
# miembros_cuidado
# hijos
# cantidad_hijos
# vive_padres
# e_civil
# edad
# carrera
# id_genero
# id_genero_otra
# Tiempo_estudio
# Tiempo_estudio2

#---- 2. ANALISIS DESCRIPTIVO ----

# Gráfico descriptivo

# Crear sub-base de datos que contenga solo las variables de cuidado

cuidados_only <- proc_encuesta %>% select(cuidado_acompanar1, cuidado_acompanar2, cuidado_aconsejar, 
                                          cuidado_acostar, cuidado_asear, cuidado_comer, cuidado_jugar,
                                          cuidado_mudar, cuidado_leer, cuidado_salud, cuidado_tareas, cuidado_vestir)

cuidados_only <-na.omit(cuidados_only)

cuidados_only <-sjlabelled::copy_labels(cuidados_only,proc_encuesta)

# Plotear

graph1 <- plot_likert(cuidados_only, sort.frq = 'pos.asc', geom.colors = 'Spectral', reverse.scale = TRUE)

save_plot('output/graph1.png', graph1, width = 35,
          height =20)

# Si/No

sino_only <- proc_encuesta %>% select(si_no_cuidado)

sino_only <-na.omit(sino_only)

graph2 <- ggplot(sino_only, aes(x = as_factor(si_no_cuidado))) +
  geom_bar(fill='#FFCC99') + 
  scale_x_discrete(labels=c('No', 'Sí'))

graph2 <- graph2 + ggtitle("Tareas de cuidado la última semana") +
  xlab("Respuesta")

ggsave("output/graph2.png", plot = graph2)

# cuidado_multiple_1

cm1_only <- proc_encuesta %>% select(cuidado_multiple_1)

cm1_only <- na.omit(cm1_only)

cm1_only<-sjlabelled::copy_labels(cm1_only,proc_encuesta)

get_labels(cm1_only$cuidado_multiple_1)

coul <- brewer.pal(3, "Set2")

graph3 <- ggplot(cm1_only, aes(x = as_factor(cuidado_multiple_1))) +
  geom_bar(fill=coul) + 
  scale_x_discrete(labels=c('Dar de comer o amamantar', 'Acostar', 'Aconsejar'))

graph3 <- graph3 + ggtitle("Tarea de cuidado que toma más tiempo") + 
  xlab('Respuesta')

ggsave("output/graph3.png", plot = graph3)


