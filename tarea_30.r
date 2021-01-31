library(ggplot2)
library(dplyr)
library(xtable)
library(readxl)
library(grid)
library(gridExtra)


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

png(filename = "./graficas/pregunta1_g1.png", width = 560, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)

p <- ggplot(p1, aes(x=FECHA_DEF, y=NUM_DEF, colour = Sexo_Edad))+
            geom_line(size = 1)
p+scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 week")+
    theme_bw()+
            labs(title = "Fallecimientos por día en la CDMX a partir del 4 de abril de 2020",
                 subtitle = "Divido por sexo y rangos de edad",
                 x = "Fecha",
                 y= "Muertes",
                 color = "Sexo_edad")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15),
                  plot.subtitle = element_text(size = 16))+
            scale_color_manual(values = mycolors)

dev.off()

png(filename = "./graficas/pregunta1_g2.png", width = 560, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)

p <- ggplot(p1, aes(x=FECHA_DEF, y=NUM_DEF, colour = Sexo_Edad))+
            geom_line(aes(linetype = rango_esp), size = 1)
p+scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 week")+
            theme_bw()+
            labs(title = "Fallecimientos por día en la CDMX a partir del 4 de abril de 2020",
                 subtitle = "Divido por sexo y rangos de edad. Tres grupos resaltados",
                 x = "Fecha",
                 y= "Muertes",
                 color = "Sexo_edad")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15),
                  plot.subtitle = element_text(size = 16))+
            scale_linetype_manual(values = c("dashed", "solid"), guide = "none")+
            scale_color_manual(values = mycolors)

dev.off()


## Analizamos cambios en la tendencia analizando los intervalos de confianza para la media antes y 
## después de un tiempo t0.

## Cambio en la tendencia en hombres mayores de 60 años.
## El primer cambio de tendencia lo observamos alrededor del 15 de junio de 2020 y el segundo alrededor del 28 de septiembre

t0 <- as.Date("2020-06-15")
t1 <- as.Date("2020-09-28")

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
# Intervalo de confianza para la media de muertes de H- 60+ antes del 2020-06-15
int.conf1

bootstap2 = replicate(n=1000, sample(h.60.segunda.tend, replace = TRUE))

media.muestral.2 <- apply(bootstap2, MARGIN = 2, FUN = mean)
int.conf2 = quantile(media.muestral.2, probs = c(.025,.975))
# Intervalo de confianza para la media de muertes de H- 60+ después del 2020-06-15 antes del 2020-09-28
int.conf2

bootstap3 = replicate(n=1000, sample(h.60.tercer.tend, replace = TRUE))

media.muestral.3 <- apply(bootstap3, MARGIN = 2, FUN = mean)
int.conf3 = quantile(media.muestral.3, probs = c(.025,.975))
# Intervalo de confianza para la media de muertes de H- 60+ después del 2020-09-28
int.conf3

## Gráfica con los cambios de tendencias
png(filename = "./graficas/pregunta1_g3.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
p1 %>% filter(Sexo_Edad == "H 60 +") %>%
            ggplot(aes(x=FECHA_DEF, y=NUM_DEF, colour = Sexo_Edad))+
            geom_line(size = 1)+
            geom_smooth()+
            scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 week")+
            theme_bw()+
            labs(title = "Fallecimientos por día en la CDMX a partir del 4 de abril de 2020",
                 subtitle = "Hombres mayores de 60 años",
                 x = "Fecha",
                 y= "Muertes")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15),
                  plot.subtitle = element_text(size = 16))+
            scale_color_manual(values = "#ad2121", guide = "none")+
            annotate(geom = "curve", x=as.Date("2020-07-05"), y= 50, xend = as.Date("2020-06-15"),
                     yend = 47, arrow= arrow(length = unit(4, "mm")))+
            annotate(geom = "text", x=as.Date("2020-07-06"), y=50, label="Primer cambio", hjust = "left")+
            annotate(geom = "curve", x=as.Date("2020-10-06"), y= 28, xend = as.Date("2020-09-28"),
                     yend = 18, arrow= arrow(length = unit(4, "mm")))+
            annotate(geom = "text", x=as.Date("2020-10-07"), y=28, label="Segundo cambio", hjust = "left")
dev.off()
            
## Cambios de tendencia en hombres de entre 40 y 60 años.
## Igualmente el primer cambio lo observamos alrededor del 15 de junio y el segundo alrededor del 12 de octubre

t0 <- as.Date("2020-06-15")
t1 <- as.Date("2020-10-12")

h.40.59.primer.tend <- p1 %>% filter(FECHA_DEF<t0, Sexo_Edad == "H 40-59")
h.40.59.primer.tend <- h.40.59.primer.tend$NUM_DEF

h.40.59.segunda.tend <- p1 %>% filter(FECHA_DEF>=t0, FECHA_DEF<t1, Sexo_Edad == "H 40-59")
h.40.59.segunda.tend <- h.40.59.segunda.tend$NUM_DEF

h.40.59.tercer.tend <-  p1 %>% filter(FECHA_DEF>=t1, Sexo_Edad == "H 40-59")
h.40.59.tercer.tend <- h.40.59.tercer.tend$NUM_DEF

bootstap4 <- replicate(n=1000, sample(h.40.59.primer.tend, replace = T))
media.muestral.4 <- apply(bootstap4, MARGIN = 2, FUN = mean)
int.conf4 <- quantile(media.muestral.4, probs = c(.025,.975))
# Intervalo de confianza para la media de muertes de H- 40-59 después del 2020-06-15 antes del 2020-09-28
int.conf4

bootstap5 <- replicate(n=1000, sample(h.40.59.segunda.tend, replace = T))
media.muestral.5 <- apply(bootstap5, MARGIN = 2, FUN = mean)
int.conf5 <- quantile(media.muestral.5, probs = c(.025,.975))
# Intervalo de confianza para la media de muertes de H- 40-59 después del 2020-06-15 antes del 2020-10-12
int.conf5

bootstap6 <- replicate(n=1000, sample(h.40.59.tercer.tend, replace = T))
media.muestral.6 <- apply(bootstap6, MARGIN = 2, FUN = mean)
int.conf6 <- quantile(media.muestral.6, probs = c(.025,.975))
# Intervalo de confianza para la media de muertes de H- 40-59 después del 2020-09-28
int.conf6

## Gráfica con los cambios de tendencias
png(filename = "./graficas/pregunta1_g4.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
p1 %>% filter(Sexo_Edad == "H 40-59") %>%
            ggplot(aes(x=FECHA_DEF, y=NUM_DEF, colour = Sexo_Edad))+
            geom_line(size = 1)+
            geom_smooth()+
            scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 week")+
            theme_bw()+
            labs(title = "Fallecimientos por día en la CDMX a partir del 4 de abril de 2020",
                 subtitle = "Hombres de entre 40 y 59 años",
                 x = "Fecha",
                 y= "Muertes")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15),
                  plot.subtitle = element_text(size = 16))+
            scale_color_manual(values = "#cbd11f", guide = "none")+
            annotate(geom = "curve", x=as.Date("2020-07-05"), y= 30, xend = as.Date("2020-06-15"),
                     yend = 24, arrow= arrow(length = unit(4, "mm")))+
            annotate(geom = "text", x=as.Date("2020-07-06"), y=30, label="Primer cambio", hjust = "left")+
            annotate(geom = "curve", x=as.Date("2020-10-18"), y= 15, xend = as.Date("2020-10-12"),
                     yend = 8, arrow= arrow(length = unit(4, "mm")))+
            annotate(geom = "text", x=as.Date("2020-10-18"), y=16, label="Segundo cambio", hjust = "left")
dev.off()


## Cambios de tendencia en mujeres mayores de 60 años
## El primer cambio de tendencia lo observamos alrededor del 20 de junio y el segundo alrededor del 28 de septiembre
t0 = as.Date("2020-06-20")
t1 = as.Date("2020-09-28")

m.60.primer.tend <- p1 %>% filter(FECHA_DEF<t0, Sexo_Edad == "M 60 +")
m.60.primer.tend <- m.60.primer.tend$NUM_DEF

m.60.segunda.tend <- p1 %>% filter(FECHA_DEF>=t0, FECHA_DEF<t1, Sexo_Edad == "M 60 +")
m.60.segunda.tend <- m.60.segunda.tend$NUM_DEF

m.60.tercer.tend <- p1 %>% filter(FECHA_DEF>t1, Sexo_Edad == "M 60 +")
m.60.tercer.tend <- m.60.tercer.tend$NUM_DEF

bootstap7 <- replicate(n=1000, sample(m.60.primer.tend, replace =T))
media.muestral.7 <- apply(bootstap7, MARGIN = 2, FUN = mean)
int.conf7 <- quantile(media.muestral.7, probs = c(.025,.975))
# Intervalo de confianza para la media de muertes de M- 60+ antes del 2020-06-20
int.conf7

bootstap8 <- replicate(n=1000, sample(m.60.segunda.tend, replace =T))
media.muestral.8 <- apply(bootstap8, MARGIN = 2, FUN = mean)
int.conf8 <- quantile(media.muestral.8, probs = c(.025,.975))
# Intervalo de confianza para la media de muertes de M- 60+ después del 2020-06-20 y antes del "2020-09-28"
int.conf8

bootstap9 <- replicate(n=1000, sample(m.60.tercer.tend, replace =T))
media.muestral.9 <- apply(bootstap9, MARGIN = 2, FUN = mean)
int.conf9 <- quantile(media.muestral.9, probs = c(.025,.975))
# Intervalo de confianza  para la media de muertes de M- 60+ después del 2020-09-28
int.conf9

## Gráfica con los cambios de tendencias
png(filename = "./graficas/pregunta1_g5.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
p1 %>% filter(Sexo_Edad == "M 60 +") %>%
            ggplot(aes(x=FECHA_DEF, y=NUM_DEF, colour = Sexo_Edad))+
            geom_line(size = 1)+
            geom_smooth()+
            scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 week")+
            theme_bw()+
            labs(title = "Fallecimientos por día en la CDMX a partir del 4 de abril de 2020",
                 subtitle = "Mujeres mayores de 60 años",
                 x = "Fecha",
                 y= "Muertes")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15),
                  plot.subtitle = element_text(size = 16))+
            scale_color_manual(values = "#04b09f", guide = "none")+
            annotate(geom = "curve", x=as.Date("2020-07-10"), y= 20, xend = as.Date("2020-06-20"),
                     yend = 16, arrow= arrow(length = unit(4, "mm")))+
            annotate(geom = "text", x=as.Date("2020-07-06"), y=20, label="Primer cambio", hjust = "left")+
            annotate(geom = "curve", x=as.Date("2020-10-05"), y= 15, xend = as.Date("2020-09-28"),
                     yend = 11, arrow= arrow(length = unit(4, "mm")))+
            annotate(geom = "text", x=as.Date("2020-10-06"), y=15, label="Segundo cambio", hjust = "left")
dev.off()

## Además, observamos que hay un cambio entre las tendencias de muertes de hombres de entre 40 y 59 años
## y mujeres de más de 60 años alrededor del 12 de julio

t0 <- as.Date("2020-07-12")

h.40.59.tend.mayor <- p1 %>% filter(FECHA_DEF<=t0, Sexo_Edad == "H 40-59")
h.40.59.tend.mayor <- h.40.59.tend.mayor$NUM_DEF

h.40.59.tend.menor <- p1 %>% filter(FECHA_DEF>t0, Sexo_Edad == "H 40-59")
h.40.59.tend.menor <- h.40.59.tend.menor$NUM_DEF

m.60.tend.menor <- p1 %>% filter(FECHA_DEF<=t0, Sexo_Edad == "M 60 +")
m.60.tend.menor <- m.60.tend.menor$NUM_DEF

m.60.tend.mayor <- p1 %>% filter(FECHA_DEF>t0, Sexo_Edad == "M 60 +")
m.60.tend.mayor <- m.60.tend.mayor$NUM_DEF

bootstap10 <- replicate(n=1000, sample(h.40.59.tend.mayor, replace = T))
media.muestral.10 <- apply(bootstap10, MARGIN = 2, FUN = mean)
int.conf10 <- quantile(media.muestral.10, probs = c(.025,.975))
## Intervalo del 95% de confianza de la media de fallecimientos de hombres de entre 40 y 59 años de edad
## antes del 20 de julio
int.conf10

bootstap11 <- replicate(n=1000, sample(m.60.tend.menor, replace = T))
media.muestral.11 <- apply(bootstap11, MARGIN = 2, FUN = mean)
int.conf11 <- quantile(media.muestral.11, probs = c(.025,.975))
## Intervalo del 95% de confianza de la media de fallecimientos de mujeres mayores de 60 años de edad
## después del 20 de julio y antes del 
int.conf11

bootstap12 <- replicate(n=1000, sample(h.40.59.tend.menor, replace = T))
media.muestral.12 <- apply(bootstap12, MARGIN = 2, FUN = mean)
int.conf12 <- quantile(media.muestral.12, probs = c(.025,.975))
## Intervalo del 95% de confanza de la media de fallecimientos de hombres de entre 40 y 59 años de edad
## después del 20 de julio
int.conf12

bootstap13 <- replicate(n=1000, sample(m.60.tend.mayor, replace = T))
media.muestral.13 <- apply(bootstap13, MARGIN = 2, FUN = mean)
int.conf13 <- quantile(media.muestral.13, probs = c(.025,.975))
## Intervalo del 95% de confianza de la media de fallecimientos de mujeres mayores de 60 años de edad
## después del 20 de julio
int.conf13

## La evidencia sugiera que a partir de esta fecha, realmente hay un cambio en la tendencia de muertes

## Gráfica con los cambios de tendencias
png(filename = "./graficas/pregunta1_g6.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
p1 %>% filter(Sexo_Edad == "M 60 +" | Sexo_Edad == "H 40-59") %>%
            ggplot(aes(x=FECHA_DEF, y=NUM_DEF, colour = Sexo_Edad))+
            geom_smooth()+
            geom_line(size = 1)+
            scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 week")+
            theme_bw()+
            labs(title = "Fallecimientos por día en la CDMX a partir del 4 de abril de 2020",
                 subtitle = "Hombres de entre 40 y 59 años y mujeres mayores de 60 años",
                 x = "Fecha",
                 y= "Muertes")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15),
                  plot.subtitle = element_text(size = 16))+
            scale_color_manual(values = c("#cbd11f","#04b09f"))+
            annotate(geom = "curve", x=as.Date("2020-07-20"), y= 20, xend = as.Date("2020-07-12"),
                     yend = 14, arrow= arrow(length = unit(4, "mm")))+
            annotate(geom = "text", x=as.Date("2020-07-20"), y=20, label="Cambio de tendencias", hjust = "left")
dev.off()

## Pregunta 2

p2 <- select(tarea, DEFUNCION, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION, CARDIOVASCULAR,
             OBESIDAD, RENAL_CRONICA, TABAQUISMO) 

probabilidades = data.frame(comorbilidad = rep(NA, 9), prob = rep(NA, 9))

for(i in 2:10){
    probabilidades[i-1,1] = names(p2)[i]
    pc = table(p2$DEFUNCION, p2[,i])[[2,2]]
    pf = sum(p2$DEFUNCION)
    probabilidades[i-1,2] = round(pc/pf, digits = 3)
}

png(filename = "./graficas/pregunta2_porcentajes_comorbilidades.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
probabilidades %>% ggplot(aes(x=reorder(comorbilidades, -prob), y=prob))+
    geom_bar(stat = "identity", color="black", fill=rgb(.2, .7,  .2) )+
    theme_bw()+
    labs(title="Probabilidad de fallecer por comorbilidad",
         x="Comorbilidad",
         y="Probabilidad de fallecimiento")+
    theme(axis.text.x = element_text(angle = 20, hjust = 1),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 16))
dev.off()

porct_comorb = c(10.3, 10, 7, 4, 25.5, 70.3, 75.2, 12.2, 8.8)
probabilidades$porct_comorb = porct_comorb
probabilidades <- probabilidades %>% mutate(Numero_esperado = round(100000*prob*porct_comorb/100, digits = 0))
probabilidades
print(xtable(probabilidades, type = "latex"), file = "fallecimientos_esperados_comorbilidades.tex")

png(filename = "./graficas/pregunta3_numero_fallecimientos.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
probabilidades %>% ggplot(aes(x=reorder(comorbilidades, -Numero_esperado), y=Numero_esperado))+
    geom_bar(stat = "identity", color="black", fill=rgb(0.6,0.1,0.3) )+
    theme_bw()+
    labs(title="Número esperado de fallecimientos por comorbilidad",
         subtitle = "Por cada cienmil infectados",
         x="Comorbilidad",
         y="Número de fallecimientos")+
    theme(axis.text.x = element_text(angle = 20, hjust = 1),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 15),
          plot.subtitle = element_text(size = 16))
dev.off()

## Investigar el porcentaje de la población que tiene cada comorbilidad
## Diabetes https://www.milenio.com/ciencia-y-salud/diabetes-en-mexico-2020-estadisticas-y-porcentaje
## Obesidad https://politica.expansion.mx/mexico/2019/12/09/el-75-2-de-los-mexicanos-padece-obesidad-y-10-3-diabetes-ensanut
## Hipertensión https://www.insp.mx/avisos/5398-hipertension-arterial-problema-salud-publica.html
## Tabaquismo https://www.paho.org/mex/index.php?option=com_content&view=article&id=97:tabaco-cifras-mexico&Itemid=387
## EPOC https://www.gob.mx/salud/prensa/10-por-ciento-de-la-poblacion-mexicana-padece-epoc
## Asma https://www.gob.mx/salud/prensa/siete-por-ciento-de-la-poblacion-en-mexico-padece-asma
## Cardiovascular 70.3 % https://asociacionale.org.mx/enfermedades-cardiovasculares-principal-causa-de-muerte-entre-los-mexicanos/#:~:text=A.C.%20(PACO).-,En%20M%C3%A9xico%2C%20el%2019%25%20de%20mujeres%20y%20hombres%20de%2030,o%20dislipidemia%20(14%20millones)%2C
## Renal cronica 12.2 % https://www.insp.mx/avisos/5296-enfermedad-renal-cronica-mexico.html#:~:text=En%202017%2C%20se%20report%C3%B3%20una,habitantes%20en%20M%C3%A9xico(2).
## Inmunospr 4 a nivel global % https://www.pisa.com.mx/personas-con-enfermedades-autoinmunes-y-en-tratamiento-inmunosupresor-vulnerables-al-covid-19/

## Pregunta 3

p3 <- tarea %>% select(FECHA_SINTOMAS, FECHA_INGRESO, FECHA_DEF, DEFUNCION, RANGO_DE_EDAD, DIABETES, EPOC, 
                       ASMA, INMUSUPR, HIPERTENSION, CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, TABAQUISMO ) %>%
            mutate(SINT_INGR = as.integer(FECHA_INGRESO - FECHA_SINTOMAS), 
                   SINT_FALLECE = as.integer(FECHA_DEF-FECHA_SINTOMAS))

p3i <- p3 %>% select(FECHA_SINTOMAS, SINT_INGR)
p3f <- p3 %>% select(FECHA_SINTOMAS, SINT_FALLECE) %>%
            filter( SINT_FALLECE>=0)

p3i %>% filter(SINT_INGR<=40) %>% 
    ggplot(aes(x=SINT_INGR))+
            geom_bar(color="black", fill=rgb(.8,.3,.6))+
    theme_bw()+
            stat_count(geom = "bar")+
            labs(title = "Frequencia de días que tarda una persona en acudir a la clínica",
                 subtitle = "Gráfica hasta n=40",
         x= "Número de días",
         y= "Frecuencia") +
            theme(axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15),
                  plot.subtitle = element_text(size = 14))

dev.copy(png, file="./graficas/pregunta3_sintomas_ingreso.png")
dev.off()

p3f %>% filter(SINT_FALLECE<=60) %>% 
    ggplot(aes(x=SINT_FALLECE))+
            geom_bar()+
    theme_bw()+
    stat_count(geom = "bar")+
            labs(title = "Frequencia de días que tarda una persona en fallecer a partir de que tiene sìntomas",
                 subtitle = "Gráfica hasta n=60",
                 x= "Número de días",
                 y= "Frecuencia") +
            theme(axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15))
dev.copy(png, file="./graficas/pregunta3_sintomas_fallecimiento.png")
dev.off()

## Estadísticas centrales de los días que tarda una persona en acudir a la clínica desde que tiene síntomas
(media.i=mean(p3i$SINT_INGR))
(desviacion.std.i = sd(p3i$SINT_INGR))
(mediana.i=median(p3i$SINT_INGR))
(quantiles.i=quantile(p3i$SINT_INGR, probs = c(.25,.75)))
(min.i=min(p3i$SINT_INGR))
(max.i=max(p3i$SINT_INGR))

## Estadísticas centrales de los días que tarda una persona en fallecer desde que tiene sintomas
(media.f=mean(p3f$SINT_FALLECE))
(desviacion.std.f=sd(p3f$SINT_FALLECE))
(meiana.f=median(p3f$SINT_FALLECE))
(quantiles.f=quantile(p3f$SINT_FALLECE, probs = c(.25,.75)))
(min.f=min(p3f$SINT_FALLECE))
(max.f=min(p3f$SINT_FALLECE))

# Queremos ajustar la distribución de los días que tarda una persona en fallecer a partir de que tiene síntimas

(mu = mean(p3f$SINT_FALLECE))
(sigma = sd(p3f$SINT_FALLECE))
(p=0.09497474)
(r=round((mu^2)/(sigma^2-mu),digits = 0))

set.seed(22)
bin.neg= rnbinom(n=nrow(p3f), size=2.57, prob = 0.09497474)
bin.neg = data.frame(bin.neg=bin.neg, categoría=rep("simulacion", length(bin.neg)))

p3f.cat = data.frame(bin.neg=p3f$SINT_FALLECE, categoría=rep("muestra", nrow(p3f)))

densidades = rbind(bin.neg,p3f.cat)

densidades %>% ggplot(aes(x=bin.neg, fill=categoría))+
    geom_density(alpha=0.4)+
    theme_bw()+
    labs(title = "Comparación de la densidad de fallecimientos vs binomial negativa",
         x="k",
         y="Probabilidad")+
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 15),
          legend.title = element_blank())+
    annotate(geom = "text", x=190, y=0.06, label="Binomial-Negativa")+
    annotate(geom = "text", x=195, y=.055, label="r=3")+
    annotate(geom = "text", x=200, y=.050, label="p=0.095")
dev.copy(png, file="./graficas/pregunta3_densidades.png")
dev.off()

## Queremos ver si existe diferencias entre comorbilidades y rangos de edad
## Por rango de edad

edad.0.19 = p3 %>% filter(RANGO_DE_EDAD=="0-19") %>% select(SINT_INGR)
edad.0.19 = edad.0.19$SINT_INGR

edad.20.39 = p3 %>% filter(RANGO_DE_EDAD=="20-39") %>% select(SINT_INGR)
edad.20.39=edad.20.39$SINT_INGR

edad.40.59 = p3 %>% filter(RANGO_DE_EDAD=="40-59") %>% select(SINT_INGR)
edad.40.59 = edad.40.59$SINT_INGR

edad.60 = edad.20.39 = p3 %>% filter(RANGO_DE_EDAD=="60 +") %>% select(SINT_INGR)
edad.60 = edad.60$SINT_INGR

bootstap14 <- replicate(n=1000, sample(edad.0.19, replace = T))
media.muestral.14 <- apply(bootstap14, MARGIN = 2, FUN = mean)
## Intervalo de confianza para la media de días que tarda una persona de entre 0 y 19 años en acudir a la clínica
(int.conf14 <- quantile(media.muestral.14, probs = c(.025,.975)))

bootstap15 <- replicate(n=1000, sample(edad.20.39, replace = T))
media.muestral.15 <- apply(bootstap15, MARGIN = 2, FUN = mean)
## Intervalo de confianza para la media de días que tarda una persona de entre 20 y 39 años en acudir a la clínica
(int.conf15 <- quantile(media.muestral.15, probs = c(.025,.975)))

bootstap16 <- replicate(n=1000, sample(edad.40.59, replace = T))
media.muestral.16 <- apply(bootstap16, MARGIN = 2, FUN = mean)
## Intervalo de confianza para la media de días que tarda una persona de entre 40 y 59 años en acudir a la clínica
(int.conf16 <- quantile(media.muestral.16, probs = c(.025,.975)))

bootstap17 <- replicate(n=1000, sample(edad.60, replace = T))
media.muestral.17 <- apply(bootstap17, MARGIN = 2, FUN = mean)
## Intervalo de confianza para la media de días que tarda una persona mayor de 60 años en acudir a la clínica
(int.conf17 <- quantile(media.muestral.17, probs = c(.025,.975)))


## Por comorbilidad
p3.3 = p3[tarea$DEFUNCION==1,]
dias.diabetes = p3.3$SINT_FALLECE[(!is.na(p3.3$DIABETES))&(p3.3$DIABETES==1)]
dias.epoc = p3.3$SINT_FALLECE[(!is.na(p3.3$EPOC))&(p3.3$EPOC==1)]
dias.asma = p3.3$SINT_FALLECE[(!is.na(p3.3$ASMA))& (p3.3$ASMA==1)]
dias.inmusupr = p3.3$SINT_FALLECE[(!is.na(p3.3$INMUSUPR))&(p3.3$INMUSUPR==1)]
dias.hipertension = p3.3$SINT_FALLECE[(!is.na(p3.3$HIPERTENSION))&(p3.3$HIPERTENSION==1)]
dias.cardio = p3.3$SINT_FALLECE[(!is.na(p3.3$CARDIOVASCULAR))&(p3.3$CARDIOVASCULAR==1)]
dias.obesidad = p3.3$SINT_FALLECE[(!is.na(p3.3$OBESIDAD))&(p3.3$OBESIDAD==1)]
dias.renal = p3.3$SINT_FALLECE[(!is.na(p3.3$RENAL_CRONICA))&(p3.3$RENAL_CRONICA==1)]
dias.tabaco = p3.3$SINT_FALLECE[(!is.na(p3.3$TABAQUISMO))&(p3.3$TABAQUISMO==1)]
    
bootstap18 = replicate(n=100, sample(dias.diabetes, replace = TRUE))
media.muestral.18 = apply(bootstap18, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con diabetes
(int.conf18 = quantile(media.muestral.18, probs = c(.25,.75), na.rm = TRUE))

bootstap19 = replicate(n=100, sample(dias.epoc, replace = TRUE))
media.muestral.19 = apply(bootstap19, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con epoc
(int.conf19 = quantile(media.muestral.19, probs = c(.25,.75), na.rm = TRUE))

bootstap20 = replicate(n=100, sample(dias.asma, replace = TRUE))
media.muestral.20 = apply(bootstap20, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con asma
(int.conf20 = quantile(media.muestral.20, probs = c(.25,.75), na.rm = TRUE))

bootstap21 = replicate(n=100, sample(dias.inmusupr, replace = TRUE))
media.muestral.21 = apply(bootstap21, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con inmusupr
(int.conf21 = quantile(media.muestral.21, probs = c(.25,.75), na.rm = TRUE))

bootstap22 = replicate(n=100, sample(dias.hipertension, replace = TRUE))
media.muestral.22 = apply(bootstap22, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con hipertensión
(int.conf22 = quantile(media.muestral.22, probs = c(.25,.75), na.rm = TRUE))

bootstap23 = replicate(n=100, sample(dias.cardio, replace = TRUE))
media.muestral.23 = apply(bootstap23, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con cardiovascular
(int.conf23 = quantile(media.muestral.23, probs = c(.25,.75), na.rm = TRUE))

bootstap24 = replicate(n=100, sample(dias.obesidad, replace = TRUE))
media.muestral.24 = apply(bootstap24, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con obesidad
(int.conf24 = quantile(media.muestral.24, probs = c(.25,.75), na.rm = TRUE))

bootstap25 = replicate(n=100, sample(dias.renal, replace = TRUE))
media.muestral.25 = apply(bootstap25, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con renal crónica
(int.conf25 = quantile(media.muestral.25, probs = c(.25,.75), na.rm = TRUE))

bootstap26 = replicate(n=100, sample(dias.tabaco, replace = TRUE))
media.muestral.26 = apply(bootstap26, MARGIN = 2, FUN = mean)
# Intervalo de confianza de la media de días que tarda en fallecer una persona con tabaquismo
(int.conf26 = quantile(media.muestral.26, probs = c(.25,.75), na.rm = TRUE))

## Queremos saber si mientras más tiempo tarda una persona en acudir al hospital, más probabilidad de morir tiene

proporciones_previas <- c(NULL)
proporciones_posteriores <- c(NULL)
for (i in 0:21){
    a=p3$DEFUNCION[p3$SINT_INGR<=i]
    b=sum(p3$SINT_INGR<=i)
    c=p3$DEFUNCION[p3$SINT_INGR>i]
    d=sum(p3$SINT_INGR>i)
    proporciones_previas[i+1]= table(a)[[2]]/b
    proporciones_posteriores[i+1]= table(c)[[2]]/d
}

proporciones = proporciones_posteriores/proporciones_previas
tabla.proporciones = data.frame(Num_dias = 0:21, proporciones = proporciones)

png(filename = "./graficas/pregunta3_taza_proporciones.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
tabla.proporciones %>% ggplot(aes(x=Num_dias, y=proporciones))+
    geom_line(size=2, color = "red")+
    theme_bw()+
    labs(title = "Taza de proporciones de personas que fallecieron",
         x="Número de días",
         y="Taza de proporción")+
    theme(legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 15))
dev.off()

## Calcular la tasa de positividad para la CDMX por alcaldia

alcaldias <- read_excel("./diccionario_datos_covid19/Catalogos_071020.xlsx", sheet = "Catálogo MUNICIPIOS")
alcaldias <- alcaldias[alcaldias$CLAVE_ENTIDAD == "09",]
alcaldias <- alcaldias[1:16,1:2]


p4 <- BD %>% select(ENTIDAD_RES ,MUNICIPIO_RES, TOMA_MUESTRA_LAB, RESULTADO_LAB) %>%
            filter(ENTIDAD_RES == 9, TOMA_MUESTRA_LAB == 1) %>%
            mutate(RESULTADO = 1*(RESULTADO_LAB==1)) %>%
            group_by(MUNICIPIO_RES) %>%
            summarize(num_casos = sum(TOMA_MUESTRA_LAB), num_positivos = sum(RESULTADO)) %>%
            mutate(positividad = 100*num_positivos/num_casos)

p4 <- p4[1:16,]
p4$MUNICIPIO_RES <- alcaldias$MUNICIPIO
print(xtable(p4, type = "latex"), file = "positividad_por_alcaldias_cdmx.tex")

png(filename = "./graficas/pregunta4_positividad_x_alcaldia.png", width = 500, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
p4 %>% ggplot(aes(x=reorder(MUNICIPIO_RES, -positividad), y=positividad))+
    geom_bar(stat = "identity", color="black", fill=rgb(.4,.7,.2))+
    theme_bw()+
    labs(title = "Positividad por alcaldía en la CDMX",
         x="Alcaldía",
         y="Positividad en porcentaje")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 15),
          plot.subtitle = element_text(size = 16))
dev.off()
