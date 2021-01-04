library(ggplot2)
library(dplyr)

source("extras.r")
source("auxiliares.r")

my_filedate <- "210103" # format(Sys.Date(), "%y%m%d")
# formato "yymmdd", es mejor probar con el día anterior


tt <- system.time(BD <- leo_base(my_filedate))[3]
tt # tiempo de lectura de la base 
# 33 seg cuando es local (se leyó del servidor anteriormente y quedó guardada)
# 110 seg cuando se lee de la página de la SSA

head(BD)
str(BD)
names(BD)


## Extraer solo las variables que nos interesan para los anális
tarea <- BD %>% 
            filter(RESULTADO_LAB==1) %>%
            select(c(6,8,9,11,12,13,16,21,22,23,24,25,27,28,29,30))
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

## Cambiar variables a 0's  1's
tarea$DIABETES <- sapply(tarea$DIABETES, cambio_sino)
tarea$EPOC <- sapply(tarea$EPOC, cambio_sino)
tarea$ASMA <- sapply(tarea$ASMA, cambio_sino)
tarea$INMUSUPR <- sapply(tarea$INMUSUPR, cambio_sino)
tarea$HIPERTENSION <- sapply(tarea$HIPERTENSION, cambio_sino)
tarea$CARDIOVASCULAR <- sapply(tarea$CARDIOVASCULAR, cambio_sino)
tarea$OBESIDAD <- sapply(tarea$OBESIDAD, cambio_sino)
tarea$RENAL_CRONICA <- sapply(tarea$RENAL_CRONICA, cambio_sino)
tarea$TABAQUISMO <- sapply(tarea$TABAQUISMO, cambio_sino)


## Dividir por rangos de edad
tarea <- mutate(tarea, RANGO_DE_EDAD = as.factor(sapply(tarea$EDAD, rango_edad)))

## Agregar variable de si fallecieron o no
tarea <- mutate(tarea, DEFUNCION = 1*(!is.na(tarea$FECHA_DEF)))

## Pregunta 1


p1 <- tarea %>% filter(FECHA_DEF >= as.Date("2020-04-01"), ENTIDAD_RES == 9) %>%
            select(FECHA_SINTOMAS, FECHA_DEF, SEXO, RANGO_DE_EDAD, DEFUNCION) %>%
            group_by(FECHA_DEF, SEXO, RANGO_DE_EDAD) %>%
            summarize(NUM_DEF = sum(DEFUNCION)) %>%
            arrange(SEXO, RANGO_DE_EDAD) 

p1["Sexo_Edad"] <- paste(p1$SEXO, p1$RANGO_DE_EDAD, sep= " ")
p1$Sexo_Edad <- as.factor(p1$Sexo_Edad)

p <- ggplot(p1, aes(x=FECHA_DEF, y=NUM_DEF, group= Sexo_Edad, colour = Sexo_Edad))+
            geom_line(size = 1.2)
p+scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 week")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 14))
dev.copy(png, file="pregunta1.png")
dev.off()

## Pregunta 2

p2 <- select(tarea, DEFUNCION, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION, CARDIOVASCULAR,
             OBESIDAD, RENAL_CRONICA, TABAQUISMO) 
tb_diabetes <- table(filter(p2[,c("DEFUNCION", "DIABETES")], !is.na(p2$DIABETES)))
tb_diabetes

tb_epoc <- table(filter(p2[,c("DEFUNCION", "EPOC")], !is.na(p2$EPOC)))
tb_epoc

tb_asma <- table(filter(p2[,c("DEFUNCION", "ASMA")], !is.na(p2$ASMA)))
tb_asma

tb_inmun <- table(filter(p2[,c("DEFUNCION", "INMUSUPR")], !is.na(p2$INMUSUPR)))
tb_inmun

tb_hipert <- table(filter(p2[,c("DEFUNCION", "HIPERTENSION")], !is.na(p2$HIPERTENSION)))
tb_hipert

tb_cardio <- table(filter(p2[,c("DEFUNCION", "CARDIOVASCULAR")], !is.na(p2$CARDIOVASCULAR)))
tb_cardio

tb_obesidad <- table(filter(p2[,c("DEFUNCION", "OBESIDAD")], !is.na(p2$OBESIDAD)))
tb_obesidad

tb_renal <- table(filter(p2[,c("DEFUNCION", "RENAL_CRONICA")], !is.na(p2$RENAL_CRONICA)))
tb_renal

tb_tab <- table(filter(p2[,c("DEFUNCION", "TABAQUISMO")], !is.na(p2$TABAQUISMO)))
tb_tab

## PRegunta 3
## Y si nos quedamos solo con los que fallecieron y hacemos un t-test entre las diferencias de los d?as?

p3 <- tarea %>% select(FECHA_SINTOMAS, FECHA_INGRESO, FECHA_DEF) %>%
            mutate(SINT_INGR = FECHA_INGRESO - FECHA_SINTOMAS, 
                   SINT_FALLECE =FECHA_DEF-FECHA_SINTOMAS)
p3$SINT_INGR <- as.integer(p3$SINT_INGR)
p3$SINT_FALLECE <- as.integer(p3$SINT_FALLECE)

hist(p3$SINT_INGR)
dev.copy(png, file="pregunta3_sintomas_ingreso.png")
dev.off()
