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
tarea <- mutate(tarea, DEFUNCION = !is.na(tarea$FECHA_DEF))

## Pregunta 1

p1 <- tarea %>% filter(FECHA_DEF >= as.Date("2020-04-01"), ENTIDAD_RES == 9) %>%
            select(FECHA_DEF, SEXO, RANGO_DE_EDAD, DEFUNCION) %>%
            group_by(FECHA_DEF, SEXO, RANGO_DE_EDAD) %>%
            summarize(NUM_DEF = sum(DEFUNCION))            

H_J <- p1 %>% filter(SEXO=="H", RANGO_DE_EDAD == "0-19") %>%
            select(FECHA_DEF,NUM_DEF)

p <- ggplot(H_J, aes(x=FECHA_DEF, y=NUM_DEF)) +
            geom_bar(stat = "identity")+
            xlab("")+
            ggtitle("Número de defunciones del sexo masculino por día en el rango de edad 0-19 en la CDMX")
p+scale_x_date(date_labels = "%Y %b %d")
dev.copy(png, file="pregunta1_H_0-19.png")
dev.off()

M_J <- p1 %>% filter(SEXO=="M", RANGO_DE_EDAD == "0-19") %>%
            select(FECHA_DEF,NUM_DEF)

p <- ggplot(M_J, aes(x=FECHA_DEF, y=NUM_DEF)) +
            geom_bar(stat = "identity")+
            xlab("")+
            ggtitle("Número de defunciones del sexo femenino por día en el rango de edad 0-19 en la CDMX")
p+scale_x_date(date_labels = "%Y %b %d")
dev.copy(png, file="pregunta1_M_0-19.png")
dev.off()

H_A <- p1 %>% filter(SEXO=="H", RANGO_DE_EDAD == "20-39") %>%
            select(FECHA_DEF,NUM_DEF)

p <- ggplot(H_A, aes(x=FECHA_DEF, y=NUM_DEF)) +
            geom_bar(stat = "identity")+
            xlab("")+
            ggtitle("Número de defunciones del sexo masculino por día en el rango de edad 20-39 en la CDMX")
p+scale_x_date(date_labels = "%Y %b %d")
dev.copy(png, file="pregunta1_H_20-39.png")
dev.off()

M_A <- p1 %>% filter(SEXO=="M", RANGO_DE_EDAD == "20-39") %>%
            select(FECHA_DEF,NUM_DEF)

p <- ggplot(M_A, aes(x=FECHA_DEF, y=NUM_DEF)) +
            geom_bar(stat = "identity")+
            xlab("")+
            ggtitle("Número de defunciones del sexo femenino por día en el rango de edad 20-39 en la CDMX")
p+scale_x_date(date_labels = "%Y %b %d")
dev.copy(png, file="pregunta1_M_20-39.png")
dev.off()

H_M <- p1 %>% filter(SEXO=="H", RANGO_DE_EDAD == "40-59") %>%
            select(FECHA_DEF,NUM_DEF)

p <- ggplot(H_J, aes(x=FECHA_DEF, y=NUM_DEF)) +
            geom_bar(stat = "identity")+
            xlab("")+
            ggtitle("Número de defunciones del sexo masculino por día en el rango de edad 40-59 en la CDMX")
p+scale_x_date(date_labels = "%Y %b %d")
dev.copy(png, file="pregunta1_H_40-59.png")
dev.off()

M_M <- p1 %>% filter(SEXO=="M", RANGO_DE_EDAD == "40-59") %>%
            select(FECHA_DEF,NUM_DEF)

p <- ggplot(M_M, aes(x=FECHA_DEF, y=NUM_DEF)) +
            geom_bar(stat = "identity")+
            xlab("")+
            ggtitle("Número de defunciones del sexo femenino por día en el rango de edad 40-59 en la CDMX")
p+scale_x_date(date_labels = "%Y %b %d")
dev.copy(png, file="pregunta1_M_40-59.png")
dev.off()

H_V <- p1 %>% filter(SEXO=="H", RANGO_DE_EDAD == "60 +") %>%
            select(FECHA_DEF,NUM_DEF)

p <- ggplot(H_J, aes(x=FECHA_DEF, y=NUM_DEF)) +
            geom_bar(stat = "identity")+
            xlab("")+
            ggtitle("Número de defunciones del sexo masculino por día en el rango de edad 60 + la CDMX")
p+scale_x_date(date_labels = "%Y %b %d")
dev.copy(png, file="pregunta1_H_60.png")
dev.off()

M_V <- p1 %>% filter(SEXO=="M", RANGO_DE_EDAD == "60 +") %>%
            select(FECHA_DEF,NUM_DEF)

p <- ggplot(M_M, aes(x=FECHA_DEF, y=NUM_DEF)) +
            geom_bar(stat = "identity")+
            xlab("")+
            ggtitle("Número de defunciones del sexo femenino por día en el rango de edad 60 + en la CDMX")
p+scale_x_date(date_labels = "%Y %b %d")
dev.copy(png, file="pregunta1_M_60.png")
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
