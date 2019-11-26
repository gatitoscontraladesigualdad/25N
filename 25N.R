########################################################
#Datos sobre violencia en contra de las mujeres
#Elaborado por: Gatitos Contra la Desigualdad
#Fecha: 25/10/2019
########################################################
rm(list = ls())

### Instalar una paquetería que facilita apertura de paqueterías: pacman
#install.packages("pacman") 
library(pacman)

# Abrimos las paqueterías con un sólo comando:
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, DescTools, lmtest, viridis, foreign)
library("wesanderson")
names(wes_palettes)
#install.packages('ghibli')
library("ghibli")
# display palettes w/ names
#par(mfrow=c(9,3))
#for(i in names(ghibli_palettes)) print(ghibli_palette(i))

### Setup ----
options(scipen=999) # Prevenir notación científica
#-----------------

###Carpeta de trabajo y carga de datos
setwd("~/Documents/Data Science/Repos/2019B/Gatitos Contra La Desigualdad/25N")
#Fuentes: INEGI, https://www.inegi.org.mx/programas/mortalidad/default.html#Tabulados

#Datos
defun<- read.dbf(file = "~/Documents/Encuestas/Violencia/INEGI/defunciones_base_datos_2018_dbf/DEFUN18.DBF") 
catalogo<- read.dbf(file = "~/Documents/Encuestas/Violencia/INEGI/defunciones_base_datos_2018_dbf/CATMINDE.DBF") 

table(defun$PRESUNTO)

#Dejar sólo homicidios en 2018
homicidios <- defun %>%
  filter(PRESUNTO==2 & ANIO_OCUR==2018)

#Dar nombre a variables categóricas
homicidios$SEXO=factor(homicidios$SEXO,
                             levels = c(1,2,9),
                             labels = c("Hombre", "Mujer", "No Especificado"))
                                        

homicidios$LUGAR_OCUR=factor(homicidios$LUGAR_OCUR,
                 levels = c(0,1,2,3,4,5,6,7,8,9,88),
                 labels = c("Vivienda particular", "Vivienda colectiva", "Escuela u oficina pública",
                            "Áreas deportivas",
                            "Calle o carretera (vía pública)",
                            "Área comercial o de servicios",
                            "Área industrial (taller, fabrica u obra)",
                            "Granja (rancho o parcela)",
                            "Otro",
                            "Se ignora",
                            "No aplica para muerte natural"
                            ))


homicidios$LUGAR_OCUR <-  fct_collapse(homicidios$LUGAR_OCUR,
               vivienda    = c("Vivienda particular", "Vivienda colectiva"),
               vía_pública = c("Áreas deportivas", "Calle o carretera (vía pública)"),             
               otros = c("Escuela u oficina pública",
                         "Área comercial o de servicios",
                         "Área industrial (taller, fabrica u obra)",
                         "Granja (rancho o parcela)",
                         "Otro",
                         "Se ignora")
                         )

homicidios$OCUPACION <-  as.factor(homicidios$OCUPACION)
homicidios$OCUPACION <-  fct_collapse(homicidios$OCUPACION,
                                       funcionarios_o_profesionista    = c("1", "2"),
                                       agri_e_indust = c("6","7","8","9"),
                                       servicios = c("3","4","5"),
                                       otros = c("10","11","97","98","99"))

homicidios$ESCOLARIDA <-  as.factor(homicidios$ESCOLARIDA)
homicidios$ESCOLARIDA <-  fct_collapse(homicidios$ESCOLARIDA,
                                      prim_o_menos    = c("1", "2", "3", "4","5"),
                                      secu = c("6","7"),
                                      ems = c("8"),
                                      sup_o_mas = c("9", "10"),
                                      na = c("88", "99"))


#Total de homicidios de mujeres
cro(homicidios$LUGAR_OCUR, homicidios$SEXO)
  # |                       |                               | homicidios$SEXO |       |                 |
  # |                       |                               |          Hombre | Mujer | No Especificado |
  # | --------------------- | ----------------------------- | --------------- | ----- | --------------- |
  # | homicidios$LUGAR_OCUR |                      vivienda |            3311 |   936 |               1 |
  # |                       |                         otros |           10663 |  1144 |              72 |
  # |                       |                   vía_pública |           17796 |  1560 |              39 |
  # |                       | No aplica para muerte natural |                 |       |                 |
  # |                       |                  #Total cases |           31770 |  3640 |             112 |

#Tabla 0: Homicidios de mujeres nacional
tasa_nac <-  homicidios %>%
  filter(SEXO=="Mujer")%>%
  summarize(n=n())%>%
  mutate(Percent = n / sum(n),
         tasa=((n/64426273)*100000))

#Tabla 1: Homicidios de mujeres por Estado
tasa_edo <-  homicidios %>%
  filter(SEXO=="Mujer")%>%
  group_by(ENT_OCURR) %>%
  summarize(n=n())%>%
  mutate(Percent = n / sum(n))
#Importar base con pob total
pob_x_est<- read.csv(file = "pob_x_est.csv", sep=",") 
#Convertir a numerica y unir con base 
tasa_edo$ENT_OCURR <- as.numeric(tasa_edo$ENT_OCURR)
class(tasa_edo$ENT_OCURR)  
class(pob_x_est$ENT_OCURR)  
tasa_edo <- left_join(x=tasa_edo, y=pob_x_est, by="ENT_OCURR")
#Crear variable de tasa  
tasa_edo <-  tasa_edo %>%
  mutate(tasa=((n/mujeres)*100000))


#Tabla 2: Lugar de homicidio
lugar <-  homicidios %>%
  group_by(SEXO,LUGAR_OCUR) %>%
  summarize(n=n())%>%
  mutate(Percent = n / sum(n)) %>%
  dplyr::filter(SEXO!="No Especificado")%>%
  mutate(tasa=((n/64426273)*100000))


#Tabla 3: Lugar de homicidio por Estado
lugar_ent <-  homicidios %>%
  group_by(ENT_OCURR,SEXO,LUGAR_OCUR ) %>%
  summarize(n=n())%>%
  mutate(Percent = n / sum(n)) %>%
  dplyr::filter(SEXO!="No Especificado")%>%
  mutate(tasa=((n/64426273)*100000))%>%
  pivot_wider(names_from =LUGAR_OCUR, 
              values_from = c("n", "Percent", "tasa"),
              names_sep = "_") %>%
  filter(SEXO=="Mujer")
  

#Tabla 4: Homicidio de mujeres por  educación
edu <-  homicidios %>%
  group_by(SEXO, ESCOLARIDA ) %>%
  summarize(n=n())%>%
  mutate(Percent = n / sum(n),
         tasa=((n/64426273)*100000)) %>%
  filter(SEXO!="No Especificado")%>%
  pivot_wider(names_from =SEXO, 
              values_from = c("n", "Percent", "tasa"),
              names_sep = "_")
  
#Tabla 5: Homicidio de mujeres por trabajo 
ocup <-  homicidios %>%
  group_by(SEXO, OCUPACION ) %>%
  summarize(n=n())%>%
  mutate(Percent = n / sum(n),
         tasa=((n/64426273)*100000)) %>%
  filter(SEXO!="No Especificado")%>%
  pivot_wider(names_from =SEXO, 
              values_from = c("n", "Percent", "tasa"),
              names_sep = "_")




