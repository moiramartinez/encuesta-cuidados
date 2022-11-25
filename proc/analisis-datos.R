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
library(GPArotation)

load('output/encuesta_cuidados.RData') # El nombre de la base es proc_encuesta

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

sum(is.na(proc_encuesta$si_no_cuidado)) #42 casos perdidos

sino_only <- proc_encuesta %>% select(si_no_cuidado)

sino_only <-na.omit(sino_only)

graph2 <- ggplot(sino_only, aes(x = as_factor(si_no_cuidado))) +
  geom_bar(fill='#FBBDEE') + 
  scale_x_discrete(labels=c('No', 'Sí'))

graph2 <- graph2 + ggtitle("Tareas de cuidado la última semana") +
  xlab("Respuesta")

ggsave("output/graph2.png", plot = graph2)

# cuidado_multiple_1

sum(is.na(proc_encuesta$cuidado_multiple_1)) # 82 casos perdidos

cm1_only <- proc_encuesta %>% select(cuidado_multiple_1)

cm1_only <- na.omit(cm1_only)

cm1_only<-sjlabelled::copy_labels(cm1_only,proc_encuesta)

get_labels(cm1_only$cuidado_multiple_1)

coul <- brewer.pal(5, "Set2")

graph3 <- ggplot(cm1_only, aes(x = as_factor(cuidado_multiple_1))) +
  geom_bar(fill=coul) + scale_x_discrete(labels=c('Dar de comer o amamantar', 'Acostar', 'Vestir o arreglar', 'Aconsejar', 
                                                  'Ayudar con tareas escolares'))

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
sum(is.na(proc_encuesta$cuidado_tiempo)) # 83 casos perdidos

tiempo_only <- proc_encuesta %>% select(cuidado_tiempo)

tiempo_only <- na.omit(tiempo_only)

get_labels(proc_encuesta$cuidado_tiempo)

coul2 <- brewer.pal(3, 'Pastel1')

graph4 <- ggplot(tiempo_only, aes(x = as_factor(cuidado_tiempo))) +
  geom_bar(fill=coul2) + 
  scale_x_discrete(labels=c('Menos de 1 hr.', '1-3 hrs.', '3-5 hrs.'))

graph4 <- graph4 + ggtitle('Cantidad de horas dedicadas al trabajo de cuidado') + xlab('Respuesta')

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

#----6. ANALISIS BIVARIADO CON PUNTAJE FACTORIAL ----

# Matriz de correlación satisfaccion_vida # BAJA CORRELACION -0.13, 55 casos

# A

prueba1<- proc_encuesta %>% select(i_flexible, satisfaccion_vida)

prueba1 <- drop_na(prueba1)

cor_1 <- cor(prueba1)

graph8 <- ggplot(prueba1, aes(x=satisfaccion_vida, y=i_flexible)) + 
  geom_point(colour = 'pink', size = 4)

graph8 <- graph8 + annotate("text", x = 2, y = 5, label = "r = - 0.15")

graph8 <- graph8 + ggtitle("Tareas de cuidado flexibles y satisfaccion con la vida") + 
  xlab('Satisfacción con la vida') + ylab ('Puntaje factorial tareas de cuidado flexibles')

ggsave('output/graph8.png', plot = graph8)

# B -0,13

prueba1.2<- proc_encuesta %>% select(i_inflexible, satisfaccion_vida)

prueba1.2 <- drop_na(prueba1.2)

cor_1.2 <- cor(prueba1.2)

graph9 <- ggplot(prueba1.2, aes(x=satisfaccion_vida, y=i_inflexible)) + 
  geom_point(colour = 'blue', size = 4)

graph9 <- graph9 + annotate("text", x = 2, y = 5, label = "r = - 0.21")

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

graph10 <- graph10 + annotate("text", x = 2, y = 5, label = "r = - 0.066")

graph10 <- graph10 + ggtitle("Tareas de cuidado inflexibles y numero de miembros del hogar") + 
  xlab('Numero de miembros del hogar') + ylab ('Puntaje factorial tareas de cuidado inflexibles')

ggsave('output/graph10.png', plot = graph10)

# B

prueba2.5 <- proc_encuesta %>% select(i_flexible, miembros_hogar)

prueba2.5 <- drop_na(prueba2.5)

cor_2.5 <- cor(prueba2.5)

graph11 <- ggplot(prueba2.5, aes(x=miembros_hogar, y=i_flexible)) + 
  geom_point(colour = 'pink', size = 4)

graph11 <- graph11 + annotate("text", x = 2, y = 5, label = "r = - 0.1")

graph11 <- graph11 + ggtitle("Tareas de cuidado flexibles y numero de miembros del hogar") + 
  xlab('Numero de miembros del hogar') + ylab ('Puntaje factorial tareas de cuidado flexibles')

ggsave('output/graph11.png', plot = graph11)

# genero

# A

prueba6 <- proc_encuesta %>% select(i_inflexible, id_genero) 

prueba6 <- drop_na(prueba6)

graph12 <- ggplot(prueba6, aes(x= as.factor(id_genero), y= i_inflexible)) +
  geom_boxplot(fill = 'pink', alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#FF2F97", fill="#FF2F97") +
  theme(legend.position="none") + scale_x_discrete(labels=c('Mujer cis', 'Hombre cis', 'No binarie', 'Otra'))

graph12 <- graph12 + ggtitle("Genero y tareas de cuidado inflexibles") +
  xlab("Genero") + ylab('Puntaje factorial tareas de cuidado inflexibles')

ggsave("output/graph12.png", plot = graph12)

# B

prueba6.5 <- proc_encuesta %>% select(i_flexible, id_genero) 

prueba6.5 <- drop_na(prueba6.5)

graph13 <- ggplot(prueba6.5, aes(x= as.factor(id_genero), y= i_flexible)) +
  geom_boxplot(fill = '#BAFFA0', alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="green", fill="green") +
  theme(legend.position="none") + scale_x_discrete(labels=c('Mujer cis', 'Hombre cis', 'No binarie', 'Otra'))

graph13 <- graph13 + ggtitle("Genero y tareas de cuidado flexibles") +
  xlab("Genero") + ylab('Puntaje factorial tareas de cuidado flexibles')

ggsave("output/graph13.png", plot = graph13)

# Carrera

# A

prueba7 <- proc_encuesta %>% select(i_flexible, carrera) 

prueba7 <- drop_na(prueba7)

graph14 <- ggplot(prueba7, aes(x= as.factor(carrera), y= i_flexible)) +
  geom_boxplot(fill = '#BAFFA0', alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="green", fill="green") +
  theme(legend.position="none") + scale_x_discrete(labels=c('Antropologia', 'Ed. Parvularia', 'Psicologia', 'Sociologia', 'Trabajo Social'))

graph14 <- graph14 + ggtitle("Carrera y tareas de cuidado flexibles") +
  xlab("Carrera") + ylab('Puntaje factorial tareas de cuidado flexibles')

ggsave("output/graph14.png", plot = graph14)

#B

prueba7.2 <- proc_encuesta %>% select(i_inflexible, carrera)

prueba7.2 <- drop_na(prueba7.2)

graph15 <- ggplot(prueba7.2, aes(x= as.factor(carrera), y= i_inflexible)) +
  geom_boxplot(fill = 'pink', alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#FF2F97", fill="#FF2F97") +
  theme(legend.position="none") + scale_x_discrete(labels=c('Antropologia', 'Ed. Parvularia', 'Psicologia', 'Sociologia', 'Trabajo Social'))

graph15 <- graph15 + ggtitle("Carrera y tareas de cuidado inflexibles") +
  xlab("Carrera") + ylab('Puntaje factorial tareas de cuidado inflexibles')

ggsave("output/graph15.png", plot = graph15)

# Estado civil

# Inflexibles

prueba8 <- proc_encuesta %>% select(i_inflexible, e_civil)

prueba8 <- drop_na(prueba8)

graph16 <- ggplot(prueba8, aes(x= as.factor(e_civil), y= i_inflexible)) +
  geom_boxplot(fill = 'pink', alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#FF2F97", fill="#FF2F97") +
  theme(legend.position="none") + scale_x_discrete(labels=c('Soltero', 'Casado', 'Otro'))

graph16 <- graph16 + ggtitle("Estado civil y tareas de cuidado inflexibles") +
  xlab("Estado civil") + ylab('Puntaje factorial tareas de cuidado inflexibles')

ggsave("output/graph16.png", plot = graph16)

# Flexibles

prueba9 <- proc_encuesta %>% select(i_flexible, e_civil) 

prueba9 <- drop_na(prueba9)

graph17 <- ggplot(prueba9, aes(x= as.factor(e_civil), y= i_flexible)) +
  geom_boxplot(fill = '#BAFFA0', alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="green", fill="green") +
  theme(legend.position="none") + scale_x_discrete(labels=c('Soltero', 'Casado', 'Otro'))

graph17 <- graph17 + ggtitle("Estado civil y tareas de cuidado flexibles") +
  xlab("Estado civil") + ylab('Puntaje factorial tareas de cuidado flexibles')

ggsave("output/graph17.png", plot = graph17)


# Vive padres

# Inflexible

prueba10 <- proc_encuesta %>% select(i_inflexible, vive_padres)

prueba10 <- drop_na(prueba10)

graph18 <- ggplot(prueba10, aes(x= as.factor(vive_padres), y= i_inflexible)) +
  geom_boxplot(fill = 'pink', alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#FF2F97", fill="#FF2F97") +
  theme(legend.position="none") + scale_x_discrete(labels=c('No', 'Si'))

graph18 <- graph18 + ggtitle("Vivir con los padres y tareas de cuidado inflexibles") +
  xlab("Vive con los padres") + ylab('Puntaje factorial tareas de cuidado inflexibles')

ggsave("output/graph18.png", plot = graph18)

# Flexible

prueba11 <- proc_encuesta %>% select(i_flexible, vive_padres) 

prueba11 <- drop_na(prueba11)

graph19 <- ggplot(prueba11, aes(x= as.factor(vive_padres), y= i_flexible)) +
  geom_boxplot(fill = '#BAFFA0', alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="green", fill="green") +
  theme(legend.position="none") + scale_x_discrete(labels=c('No', 'Sí'))

graph19 <- graph19 + ggtitle("Vivir con los padres y tareas de cuidado flexibles") +
  xlab("Vive con los padres") + ylab('Puntaje factorial tareas de cuidado flexibles')

ggsave("output/graph19.png", plot = graph19)


# ---- 7. ANALISIS BIVARIADO SI_NO_CUIDADOS ----

# Genero 

sino_genero <- proc_encuesta %>% select(si_no_cuidado, id_genero)

sino_genero <- drop_na(sino_genero)

get_labels(sino_genero)

col3 <- brewer.pal(4, 'Pastel2')

graph20 <- ggplot(sino_genero, 
       aes(x = as.factor(si_no_cuidado), 
           fill = factor(id_genero,
                         labels = c("Mujer cis", 
                                    "Hombre cis", 
                                    "No binarie", 
                                    "Otra")))) + 
  geom_bar(position = "dodge") +  scale_fill_brewer(palette = "Pastel2") + scale_x_discrete(labels=c('No', 'Sí'))

graph20 <- graph20 + ggtitle("Realizar tareas de cuidados e identidad de género") + guides(fill=guide_legend(title="Identidad de género")) +
  xlab("Realiza tarea de cuidados")

ggsave('output/graph20.png',plot = graph20)

# Carrera

sino_carrera <- proc_encuesta %>% select(si_no_cuidado, carrera)

sino_carrera <- drop_na(sino_carrera)

get_labels(sino_carrera)

graph21 <- ggplot(sino_carrera, 
                  aes(x = as.factor(si_no_cuidado), 
                      fill = factor(carrera,
                                    labels = c("Antropología", 
                                               "Ed. Parvularia", 
                                               "Psicología", 
                                               "Sociología",
                                               'Trabajo Social')))) + 
  geom_bar(position = "dodge") +  scale_fill_brewer(palette = "YlGnBu") + scale_x_discrete(labels=c('No', 'Sí'))

graph21 <- graph21 + ggtitle("Realizar tareas de cuidados y carrera") + guides(fill=guide_legend(title="Carrera")) +
xlab("Realiza tarea de cuidados")

ggsave('output/graph21.png',plot = graph21)

# Estado civil

sino_ecivil <- proc_encuesta %>% select(si_no_cuidado, e_civil)

sino_ecivil <- drop_na(sino_ecivil)

get_labels(sino_ecivil)

graph22 <- ggplot(sino_ecivil, 
                  aes(x = as.factor(si_no_cuidado), 
                      fill = factor(e_civil,
                                    labels = c("Soltero", 
                                               "Casado", 
                                               "Otro")))) + 
  geom_bar(position = "dodge") +  scale_fill_brewer(palette = "Accent") + scale_x_discrete(labels=c('No', 'Sí'))

graph22 <- graph22 + ggtitle("Realizar tareas de cuidados y estado civil") + guides(fill=guide_legend(title="Estado civil")) +
  xlab("Realiza tarea de cuidados")

ggsave('output/graph22.png',plot = graph22)

# Vive padres

sino_padres <- proc_encuesta %>% select(si_no_cuidado, vive_padres)

sino_padres <- drop_na(sino_padres)

get_labels(sino_padres)

graph23 <- ggplot(sino_padres, 
                  aes(x = as.factor(si_no_cuidado), 
                      fill = factor(vive_padres,
                                    labels = c("No vive con sus padres", 
                                               "Vive con sus padres")))) + 
  geom_bar(position = "dodge") +  scale_fill_brewer(palette = "YlOrRd") + scale_x_discrete(labels=c('No', 'Sí'))

graph23 <- graph23 + ggtitle("Realizar tareas de cuidados y vivir con los padres") + guides(fill=guide_legend(title="")) +
  xlab("Realiza tarea de cuidados")

ggsave('output/graph23.png',plot = graph23)

# Satisfaccion con la vida

sino_vida <- proc_encuesta %>% select(si_no_cuidado, satisfaccion_vida)

sino_vida <- drop_na(sino_vida)

graph24 <- ggplot(sino_vida, 
       aes(x = as.factor(si_no_cuidado), 
           y = satisfaccion_vida)) + 
  geom_boxplot(fill = 'red', alpha = 0.4) + scale_x_discrete(labels=c('No', 'Sí')) + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#D84040", fill="#D84040")

graph24 <- graph24 + ggtitle("Realizar tareas de cuidados y satisfacción con la vida") +
  xlab("Realiza tarea de cuidados") + ylab('Satisfacción con la vida (1-10)')

ggsave('output/graph23.png',plot = graph23)

# Miembros en el hogar

sino_miembros <- proc_encuesta %>% select (si_no_cuidado,miembros_hogar)

sino_miembros <- drop_na(sino_miembros)

graph25 <- ggplot(sino_miembros, 
                  aes(x = as.factor(si_no_cuidado), 
                      y = miembros_hogar)) + 
  geom_boxplot(fill = 'yellow', alpha = 0.3) + scale_x_discrete(labels=c('No', 'Sí')) + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#F2D741", fill="#F2D741")

graph25 <- graph25 + ggtitle("Realizar tareas de cuidados y cantidad de miembros en el hogar") +
  xlab("Realiza tarea de cuidados") + ylab('Miembros en el hogar')

ggsave('output/graph25.png',plot = graph25)




