# Análisis del trabajo de cuidados estudiantes de FACSO 2022
# Moira Martínez, Catalina Navia y Ambar Santander
# Curso Encuestas Sociales, Noviembre 2022

#----1. LIBRERÍAS Y BASE DE DATOS ----

library(sjPlot)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(sjlabelled)
library(sjmisc)
library(kableExtra)
library(webshot)
library(corrplot)
library(psy)
library(psych)
library(ggmosaic)
library(carData)
library(tidyr)

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

sum(is.na(proc_encuesta$cuidado_acompanar1)) #116 casos perdidos

sum(is.na(proc_encuesta$cuidados_acompanar2))

sum(is.na(proc_encuesta$cuidado_aconsejar))

sum(is.na(proc_encuesta$cuidado_acostar))

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

sum(is.na(proc_encuesta$si_no_cuidado)) #141 casos perdidos

sino_only <- proc_encuesta %>% select(si_no_cuidado)

sino_only <-na.omit(sino_only)


graph2 <- ggplot(sino_only, aes(x = as_factor(si_no_cuidado))) +
  geom_bar(fill='#FBBDEE') + 
  scale_x_discrete(labels=c('No', 'Sí'))

graph2 <- graph2 + ggtitle("Tareas de cuidado la última semana") +
  xlab("Respuesta")

ggsave("output/graph2.png", plot = graph2)

# cuidado_multiple_1

sum(is.na(proc_encuesta$cuidado_multiple_1)) # 174 casos perdidos

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

# Tabla general

names(proc_encuesta)

proc_encuesta %>% descr(si_no_cuidado, cuidado_tiempo, cuidado_comer, cuidado_acostar, cuidado_mudar, cuidado_asear, cuidado_vestir, cuidado_aconsejar,
                        cuidado_salud, cuidado_acompanar1, cuidado_acompanar2, cuidado_tareas, cuidado_jugar, cuidado_leer, cuidado_multiple_1,
                        cuidado_multiple_2, cuidado_multiple_3, show = c("label","range", "mean", "sd", "NA.prc", "n"), out = 'browser', file = 'output/tab1.html')

webshot("output/tab1.html","output/tab1.png")

# Horas
sum(is.na(proc_encuesta$cuidado_tiempo)) # 174 casos perdidos

tiempo_only <- proc_encuesta %>% select(cuidado_tiempo)

tiempo_only <- na.omit(tiempo_only)

get_labels(proc_encuesta$cuidado_tiempo)

coul2 <- brewer.pal(3, 'Pastel1')

graph4 <- ggplot(tiempo_only, aes(x = as_factor(cuidado_tiempo))) +
  geom_bar(fill=coul2) + 
  scale_x_discrete(labels=c('Menos de 1 hr.', '1-3 hrs.', '3-5 hrs.'))

graph4<- graph4 + ggtitle('Cantidad de horas dedicadas al trabajo de cuidado') + xlab('Respuesta')

ggsave('output/graph4.png', plot = graph4)

#----3. MATRIZ DE CORRELACIONES ----

cuidados_only <- sjlabelled::copy_labels(cuidados_only,proc_encuesta)

options(digits=2)

corMat  <- cor(cuidados_only) 

tab_corr(cuidados_only, triangle = "lower",  file = 'output/tab2.html')

webshot('output/tab2.html', 'output/tab2.png')

png("output/graph5.png", height=1800, width=1800, type = 'cairo')

graph5 <- corrplot(corMat, type="lower",
                   order="AOE", cl.pos="b", tl.pos="d")
dev.off()

#---- 4. ANÁLISIS FACTORIAL ----

scree.plot(cuidados_only) # Se ven claramente 3 factores

fac_ml <- fa(r = cuidados_only, nfactors = 2, fm= "ml")

summary(fac_ml)

png("output/graph6.png", height=700, width=900, type = 'cairo')

factor.plot(fac_ml, labels=rownames(fac_ml$loadings)) 

dev.off()

tab_fa(cuidados_only, rotation = "varimax",show.comm = TRUE,  nmbr.fctr = 2, title = "Analisis factorial tareas de cuidado", 
       file = 'output/tab3.html')

webshot('output/tab3.html', 'output/tab3.png')

# Puntajes factoriales

fac_ml2 <- fa(r = select(proc_encuesta,cuidado_comer, cuidado_acostar, cuidado_mudar, cuidado_asear, cuidado_vestir, cuidado_aconsejar,
                         cuidado_salud, cuidado_acompanar1, cuidado_acompanar2, cuidado_tareas, cuidado_jugar, cuidado_leer), nfactors = 2, fm= "ml", scores="regression")

proc_encuesta <- cbind(proc_encuesta, fac_ml2$scores)

head(proc_encuesta)

#----5. AJUSTES PARA ANALISIS BIVARIADO ----

proc_encuesta <- rename(proc_encuesta, "i_inflexible"= ML1) 

proc_encuesta <- rename(proc_encuesta, 'i_flexible'=ML2)

proc_encuesta$i_flexible<- set_label(x = proc_encuesta$i_flexible, label = 'Indice Trabajo de Cuidados Flexible')

proc_encuesta$i_inflexible<- set_label(x = proc_encuesta$i_inflexible, label = 'Indice Trabajo de Cuidados Inflexible')

frq(proc_encuesta$i_flexible)
frq(proc_encuesta$i_inflexible)

#----6. ANALISIS BIVARIADO ----

# Matriz de correlación satisfaccion_vida # BAJA CORRELACION -0.13, 55 casos

# A

prueba1<- proc_encuesta %>% select(i_flexible, satisfaccion_vida)

prueba1 <- drop_na(prueba1)

cor_1 <- cor(prueba1)

graph8 <- ggplot(prueba1, aes(x=satisfaccion_vida, y=i_flexible)) + 
  geom_point(colour = 'pink', size = 4)

graph8 <- graph8 + annotate("text", x = 2, y = 5, label = "r = - 0.14")

graph8 <- graph8 + ggtitle("Tareas de cuidado flexibles y satisfaccion con la vida") + 
  xlab('Satisfacción con la vida') + ylab ('Puntaje factorial tareas de cuidado flexibles')

ggsave('output/graph8.png', plot = graph8)

# B -0,13

prueba1.2<- proc_encuesta %>% select(i_inflexible, satisfaccion_vida)

prueba1.2 <- drop_na(prueba1.2)

cor_1.2 <- cor(prueba1.2)

graph9 <- ggplot(prueba1.2, aes(x=satisfaccion_vida, y=i_inflexible)) + 
  geom_point(colour = 'blue', size = 4)

graph9 <- graph9 + annotate("text", x = 2, y = 5, label = "r = - 0.13")

graph9 <- graph9 + ggtitle("Tareas de cuidado inflexibles y satisfaccion con la vida") + 
  xlab('Satisfacción con la vida') + ylab ('Puntaje factorial tareas de cuidado inflexibles')

ggsave('output/graph9.png', plot = graph9)

# Matriz de correlación miembros_hogar # BAJA CORRELACION -0.109, 55 casos CASO INTERESANTE IWAL

# A

prueba2 <- proc_encuesta %>% select(i_inflexible, miembros_hogar)

prueba2 <- drop_na(prueba2)

cor_2 <- cor(prueba2)

graph10 <- ggplot(prueba2, aes(x=miembros_hogar, y=i_inflexible)) + 
  geom_point(colour = 'blue', size = 4)

graph10 <- graph10 + annotate("text", x = 2, y = 5, label = "r = - 0.109")

graph10 <- graph10 + ggtitle("Tareas de cuidado inflexibles y numero de miembros del hogar") + 
  xlab('Numero de miembros del hogar') + ylab ('Puntaje factorial tareas de cuidado inflexibles')

ggsave('output/graph10.png', plot = graph10)

# B

prueba2.5 <- proc_encuesta %>% select(i_flexible, miembros_hogar)

prueba2.5 <- drop_na(prueba2.5)

cor_2.5 <- cor(prueba2.5)

graph11 <- ggplot(prueba2.5, aes(x=miembros_hogar, y=i_flexible)) + 
  geom_point(colour = 'pink', size = 4)

graph11 <- graph11 + annotate("text", x = 2, y = 5, label = "r = - 0.19")

graph11 <- graph11 + ggtitle("Tareas de cuidado flexibles y numero de miembros del hogar") + 
  xlab('Numero de miembros del hogar') + ylab ('Puntaje factorial tareas de cuidado flexibles')

ggsave('output/graph11.png', plot = graph11)


# Matriz de correlación cantidad_hijos # ALTA CORRELACION PERO MUY POCOS CASOS (2)

prueba3 <- proc_encuesta %>% select(i_inflexible, i_flexible, cantidad_hijos)

prueba3 <- drop_na(prueba3)

cor_3 <- cor(prueba3)

# Matriz correlacion edad #BAJA CORRELACION -0.095 y 0.069 para 54 casos

prueba4 <- proc_encuesta %>% select(i_inflexible, i_flexible, edad)

prueba4 <- drop_na(prueba4)

cor_4 <- cor(prueba4)

# Matriz correlacion tiempo_estudio # BAJA CORRELACION -0.16 Y 0.091 para 52 casos

prueba5 <- proc_encuesta %>% select(i_inflexible, i_flexible, Tiempo_estudio) 

prueba5 <- drop_na(prueba5)

cor_5 <- cor(prueba5)

# miembros_cuidado # CORRELACION MEDIA CON INFLEXIBLE BAJA CON FLEXIBLE 53 CASOS

prueba6 <- proc_encuesta %>% select(i_inflexible, i_flexible, miembros_cuidado) 

prueba6 <- drop_na(prueba6)

cor_6 <- cor(prueba6)

graph7 <- ggplot(prueba6, 
       aes(x = as.factor(miembros_cuidado), 
           y = i_inflexible)) +
  geom_boxplot(fill="orange", alpha=0.2) +
  scale_x_discrete(labels=c('No', 'Sí')) +
  labs(title = "Poseer o no un integrante familiar con enfermedad que requiera cuidados y trabajo de cuidados inflexible", 
       x = 'Poseer integrante a quien cuidar', y = 'Indice trabajo de cuidado inflexible') 

ggsave('output/graph7.png', plot = graph7)

# hijos

prueba7 <- proc_encuesta %>% select(i_inflexible, i_flexible, hijos) 

prueba7 <- drop_na(prueba7)

graph8 <- ggplot(prueba7, 
                 aes(x = as.factor(hijos), 
                     y = i_inflexible)) +
  geom_boxplot(fill="pink", alpha=0.2) +
  scale_x_discrete(labels=c('No', 'Sí')) +
  labs(title = "Tener hijos y trabajo de cuidados inflexible", 
       x = 'Tener', y = 'Indice trabajo de cuidado inflexible') 

ggsave('output/graph7.png', plot = graph8)




