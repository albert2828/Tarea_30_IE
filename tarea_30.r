library(ggplot2)
library(dplyr)

source("extras.r")
source("auxiliares.r")

my_filedate <- "201122"


tt <- system.time(BD <- leo_base(my_filedate))[3]
tt # tiempo de lectura de la base 
# 33 seg cuando es local (se leyó del servidor anteriormente y quedó guardada)
# 110 seg cuando se lee de la página de la SSA

head(BD)
str(BD)
names(BD)


## Extraer solo las variables que nos interesan para los anális
tarea <- BD %>% 
            filter(CLASIFICACION_FINAL %in% c(1,2,3)) %>%
            select(c(6,8,9,10,11,12,13,14,15,16,18,21,22,23,24,25,26,27,28,29,30,32,33,34,38))
#head(tarea)
#str(tarea)

## Cambiar las variables de fechas a vectores de tipo date
tarea$FECHA_INGRESO <- as.Date(tarea$FECHA_INGRESO)
tarea$FECHA_SINTOMAS <- as.Date(tarea$FECHA_SINTOMAS)

tarea$FECHA_DEF <- unlist(lapply(tarea$FECHA_DEF,cambio))
tarea$FECHA_DEF <- as.Date(tarea$FECHA_DEF)
tarea$FECHA_DEF[tarea$FECHA_DEF==as.Date("1900-01-31")] = NA            

            
## Cambiar varaible de sexo por H - M
tarea$SEXO <- sapply(tarea$SEXO, cambio_sex)

## Cambiar varaibres a 0's  1's
tarea$INTUBADO <- sapply(tarea$INTUBADO, cambio_sino)
tarea$NEUMONIA <- sapply(tarea$NEUMONIA, cambio_sino)
tarea$EMBARAZO <- sapply(tarea$EMBARAZO, cambio_sino)
tarea$DIABETES <- sapply(tarea$DIABETES, cambio_sino)
tarea$EPOC <- sapply(tarea$EPOC, cambio_sino)
tarea$ASMA <- sapply(tarea$ASMA, cambio_sino)
tarea$INMUSUPR <- sapply(tarea$INMUSUPR, cambio_sino)
tarea$HIPERTENSION <- sapply(tarea$HIPERTENSION, cambio_sino)
tarea$OTRA_COM <- sapply(tarea$OTRA_COM, cambio_sino)
tarea$CARDIOVASCULAR <- sapply(tarea$CARDIOVASCULAR, cambio_sino)
tarea$OBESIDAD <- sapply(tarea$OBESIDAD, cambio_sino)
tarea$RENAL_CRONICA <- sapply(tarea$RENAL_CRONICA, cambio_sino)
tarea$TABAQUISMO <- sapply(tarea$TABAQUISMO, cambio_sino)
tarea$TOMA_MUESTRA <- sapply(tarea$TOMA_MUESTRA, cambio_sino)
tarea$UCI <- sapply(tarea$UCI, cambio_sino)


## Dividir por rangos de edad

tarea <- mutate(tarea, RANGO_DE_EDAD = as.factor(sapply(tarea$EDAD, rango_edad)))

## Pregunta 1