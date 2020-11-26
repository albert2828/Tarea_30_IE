library(ggplot2)
library(dplyr)

source("extras.r")

my_filedate <- "201122"


tt <- system.time(BD <- leo_base(my_filedate))[3]
tt # tiempo de lectura de la base 
# 33 seg cuando es local (se leyó del servidor anteriormente y quedó guardada)
# 110 seg cuando se lee de la página de la SSA

head(BD)
str(BD)
names(BD)


## Extraer solo las variables que nos interesan para los anális
tarea <- BD[,c(6,10,11,12,13,14,15,16,18,21,22,23,24,25,26,27,28,29,30,31,32,33,34,38)]
tarea <- filter(tarea, tarea$RESULTADO_LAB==1)
#head(tarea)
#str(tarea)

## Cambiar las variables de fechas a vectores de tipo date
tarea$FECHA_INGRESO <- as.Date(tarea$FECHA_INGRESO)
tarea$FECHA_SINTOMAS <- as.Date(tarea$FECHA_SINTOMAS)

cambio <- function(fecha){
            if (fecha == "9999-99-99"){
            fecha = "1900-01-31"
}            
            else {
            fecha = fecha
            }
fecha
}

tarea$FECHA_DEF <- unlist(lapply(tarea$FECHA_DEF,cambio))
tarea$FECHA_DEF <- as.Date(tarea$FECHA_DEF)
            

            
## Cambiar varaible de sexo por H - M
cambio_sex <- function(s){
            if(s==1)s = "M"
            else s="H"
                        
}

tarea$SEXO <- sapply(tarea$SEXO, cambio_sex)
## Cambiar comorbilidades a 0 - 1

## 