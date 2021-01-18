library(ggplot2)
library(dplyr)

source("extras.r")
source("auxiliares.r")

if (!dir.exists("graficas")){
            dir.create("graficas")
}

my_filedate <-  "210117" # format(Sys.Date(), "%y%m%d")
# formato "yymmdd", es mejor probar con el día anterior


tt <- system.time(BD <- leo_base(my_filedate))[3]
tt # tiempo de lectura de la base 
# 33 seg cuando es local (se leyó del servidor anteriormente y quedó guardada)
# 110 seg cuando se lee de la página de la SSA

#head(BD)
#str(BD)
#names(BD)


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
p1$rango_esp <-  p1$Sexo_Edad == "H 40-59" | p1$Sexo_Edad == "H 60 +" | p1$Sexo_Edad == "M 60 +"

mycolors <- c("#1a277d", "#6c289c", "#cbd11f", "#ad2121", "#960c89", "#d1611b", "#1f3b1d", "#04b09f")

png(filename = "./graficas/pregunta1.png", width = 960, height = 720, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)

p <- ggplot(p1, aes(x=FECHA_DEF, y=NUM_DEF, colour = Sexo_Edad))+
            geom_line(aes(linetype = rango_esp), size = 1.1)
p+scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 week")+
            labs(title = "Muertes por día en la CDMX a partir del 4 de abril de 2020",
                 subtitle = "Divido por sexo y rangos de edad",
                 x = "Fecha",
                 y= "Muertes",
                 color = "Sexo_edad")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 20),
                  plot.subtitle = element_text(size = 16))+
            scale_linetype_manual(values = c("dashed", "solid"), guide = "none")+
            scale_color_manual(values = mycolors)

dev.off()

## Analizamos cambios en la tendencia analizando los intervalos de confianza para la media antes y 
## después de un tiempo t0.

## Cambio en la tendencia en hombres mayores de 60 años.
## El primer cambio de tendencia lo observamos el 22 de junio de 2020

t0 <- as.Date("2020-06-22")
t1 <- as.Date("2020-11-01")

h.60.primer.tend <- p1 %>% filter(FECHA_DEF<t0, Sexo_Edad == "H 60 +")
h.60.primer.tend <- h.60.primer.tend$NUM_DEF

h.60.segunda.tend <- p1 %>% filter(FECHA_DEF>=t0, FECHA_DEF<t1, Sexo_Edad == "H 60 +")
h.60.segunda.tend <- h.60.segunda.tend$NUM_DEF

h.60.tercer.tend <-  p1 %>% filter(FECHA_DEF>=t1, Sexo_Edad == "H 60 +")
h.60.tercer.tend <- h.60.tercer.tend$NUM_DEF

## Hacemos boostrap para calcular los intervalos de confianza
bootstap1 = replicate(n=1000, sample(h.60.primer.tend, replace = TRUE))

media.muestral.1 <- apply(bootstap1, MARGIN = 2, FUN = mean)
int.conf1 = quantile(media.muestral.1, probs = c(.025,.975))
int.conf1

bootstap2 = replicate(n=1000, sample(h.60.segunda.tend, replace = TRUE))

media.muestral.2 <- apply(bootstap2, MARGIN = 2, FUN = mean)
int.conf2 = quantile(media.muestral.2, probs = c(.025,.975))
int.conf2

bootstap3 = replicate(n=1000, sample(h.60.tercer.tend, replace = TRUE))

media.muestral.3 <- apply(bootstap3, MARGIN = 2, FUN = mean)
int.conf3 = quantile(media.muestral.3, probs = c(.025,.975))
int.conf3

## Aquí podría hacer una gráfica solo de la tendencia de muertes de personas del sexo masculino mayores de 60 años
## y señalar los puntos de las tendencias utilizando annotate de ggplot2
            
## Pregunta 2

p2 <- select(tarea, DEFUNCION, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION, CARDIOVASCULAR,
             OBESIDAD, RENAL_CRONICA, TABAQUISMO) 

tables <- list(NULL)

for (i in 2:10) {
            tables[[i-1]] <- table(filter(p2[,c(1, i)], !is.na(p2[,i])))
}



## Pregunta 3

p3 <- tarea %>% select(FECHA_SINTOMAS, FECHA_INGRESO, FECHA_DEF, DEFUNCION) %>%
            mutate(SINT_INGR = as.integer(FECHA_INGRESO - FECHA_SINTOMAS), 
                   SINT_FALLECE = as.integer(FECHA_DEF-FECHA_SINTOMAS))

p3i <- p3 %>% select(FECHA_SINTOMAS, SINT_INGR) %>%
            filter(SINT_INGR<16)

p3f <- p3 %>% select(FECHA_SINTOMAS, SINT_FALLECE) %>%
            filter(SINT_FALLECE<40, SINT_FALLECE>=0)

p <- ggplot(p3i, aes(x=SINT_INGR))+
            geom_histogram()
p +  stat_bin(binwidth = 1, drop = FALSE, right = FALSE, col = "black")+
            labs(title = "Frequencia de días que tarda una persona en acudir a la clínica",
         x= "Número de días",
         y= "Frecuencia") +
            theme(axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15))

dev.copy(png, file="./graficas/pregunta3_sintomas_ingreso.png")
dev.off()

p <- ggplot(p3f, aes(x=SINT_FALLECE))+
            geom_bar()
p + stat_count(geom = "bar")+
            labs(title = "Frequencia de días que tarda una persona en morir a partir de que tiene sìntomas",
                 x= "Número de días",
                 y= "Frecuencia") +
            theme(axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15))

dev.copy(png, file="./graficas/pregunta3_sintomas_fallecimiento.png")
dev.off()


lambda1 <- mean(p3i$SINT_INGR)
lambda2 <- mean(p3f$SINT_FALLECE)
sdi <- sd(p3i$SINT_INGR)
sdf <- sd(p3f$SINT_FALLECE)




## Queremos saber si mientras más tiempo tarda una persona en acudir al hospital, más probabilidad de morir tiene





## Calcular la tasa de positividad para la CDMX por alcaldia

