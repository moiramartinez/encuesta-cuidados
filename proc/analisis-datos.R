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

fac_ml2 <- fa(r = cuidados_only, nfactors = 2, fm= "ml", scores="regression")

cuidados_only2=cuidados_only

cuidados_only3 <- cbind(cuidados_only2, fac_ml2$scores)

head(cuidados_only3)





