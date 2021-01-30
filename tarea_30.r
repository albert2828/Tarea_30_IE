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
int.conf1

bootstap2 = replicate(n=1000, sample(h.60.segunda.tend, replace = TRUE))

media.muestral.2 <- apply(bootstap2, MARGIN = 2, FUN = mean)
int.conf2 = quantile(media.muestral.2, probs = c(.025,.975))
int.conf2

bootstap3 = replicate(n=1000, sample(h.60.tercer.tend, replace = TRUE))

media.muestral.3 <- apply(bootstap3, MARGIN = 2, FUN = mean)
int.conf3 = quantile(media.muestral.3, probs = c(.025,.975))
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
int.conf4

bootstap5 <- replicate(n=1000, sample(h.40.59.segunda.tend, replace = T))
media.muestral.5 <- apply(bootstap5, MARGIN = 2, FUN = mean)
int.conf5 <- quantile(media.muestral.5, probs = c(.025,.975))
int.conf5

bootstap6 <- replicate(n=1000, sample(h.40.59.tercer.tend, replace = T))
media.muestral.6 <- apply(bootstap6, MARGIN = 2, FUN = mean)
int.conf6 <- quantile(media.muestral.6, probs = c(.025,.975))
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
int.conf7

bootstap8 <- replicate(n=1000, sample(m.60.segunda.tend, replace =T))
media.muestral.8 <- apply(bootstap8, MARGIN = 2, FUN = mean)
int.conf8 <- quantile(media.muestral.8, probs = c(.025,.975))
int.conf8

bootstap9 <- replicate(n=1000, sample(m.60.tercer.tend, replace =T))
media.muestral.9 <- apply(bootstap9, MARGIN = 2, FUN = mean)
int.conf9 <- quantile(media.muestral.9, probs = c(.025,.975))
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
## antes del 20 de julio
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

tables <- list(NULL)

for (i in 2:10) {
            tables[[i-1]] <- table(p2$DEFUNCION, p2[,i])
}


x <- seq(0.001,.999, by = 0.001)

betas <- list(NULL)
comorbilidades <- names(p2[,2:10])
for (i in 2:10){
            a = tables[[i-1]][1,2]
            b = tables[[i-1]][1,1] + tables[[i-1]][2,1] + tables[[i-1]][2,2]
            betas[[i-1]] <- data.frame(x, dist = dbeta(x, shape1 = a, shape2 = b), 
                                       comorbilidad = rep(comorbilidades[i-1], times = length(x)))
}

df_betas <- betas[[1]]

for (i in 2:9) {
        df_betas <- rbind(df_betas, betas[[i]])
}


df_betas %>% ggplot(aes(x=x, y=dist, colour = comorbilidad))+
    geom_line(size=2)

Ebetas <- c(NULL)
for (i in 1:9){
    a = tables[[i]][1,2] + 1/2
    b = tables[[i]][1,1] + tables[[i]][2,1] + tables[[i]][2,2] + 3/2
    Ebetas[i] = a/(a+b)
}

Ebetas = data.frame(comorbilidades, Esperanza = Ebetas)
Ebetas <- Ebetas %>% arrange(desc(Esperanza))
Ebetas
print(xtable(Ebetas, type = "latex"), file = "probabilidad_comorbilidades.tex")

png(filename = "./graficas/pregunta2_porcentajes_comorbilidades.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
Ebetas %>% ggplot(aes(x=reorder(comorbilidades, -Esperanza), y=Esperanza))+
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

porct_comorb = c(25.5, 75.2, 10.3, 8.8, 10, 7, 70.3, 12.2, 4)
Ebetas$porct_comorb = porct_comorb
Ebetas <- Ebetas %>% mutate(Numero_esperado = round(100000*Esperanza*porct_comorb/100, digits = 0))
Ebetas

print(xtable(Ebetas, type = "latex"), file = "fallecimientos_esperados_comorbilidades.tex")

png(filename = "./graficas/pregunta3_numero_fallecimientos.png", width = 480, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
Ebetas %>% ggplot(aes(x=reorder(comorbilidades, -Numero_esperado), y=Numero_esperado))+
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
## Obesidad https://www.jornada.com.mx/ultimas/economia/2020/06/16/mexico-el-pais-con-mayor-obesidad-de-al-ocde-4377.html
## Hipertensión https://www.insp.mx/avisos/5398-hipertension-arterial-problema-salud-publica.html
## Tabaquismo https://www.paho.org/mex/index.php?option=com_content&view=article&id=97:tabaco-cifras-mexico&Itemid=387
## EPOC https://www.gob.mx/salud/prensa/10-por-ciento-de-la-poblacion-mexicana-padece-epoc
## Asma https://www.gob.mx/salud/prensa/siete-por-ciento-de-la-poblacion-en-mexico-padece-asma
## Cardiovascular 70.3 % https://asociacionale.org.mx/enfermedades-cardiovasculares-principal-causa-de-muerte-entre-los-mexicanos/#:~:text=A.C.%20(PACO).-,En%20M%C3%A9xico%2C%20el%2019%25%20de%20mujeres%20y%20hombres%20de%2030,o%20dislipidemia%20(14%20millones)%2C
## Renal cronica 12.2 % https://www.insp.mx/avisos/5296-enfermedad-renal-cronica-mexico.html#:~:text=En%202017%2C%20se%20report%C3%B3%20una,habitantes%20en%20M%C3%A9xico(2).
## Inmunospr 4 a nivel global % https://www.pisa.com.mx/personas-con-enfermedades-autoinmunes-y-en-tratamiento-inmunosupresor-vulnerables-al-covid-19/

## Pregunta 3

p3 <- tarea %>% select(FECHA_SINTOMAS, FECHA_INGRESO, FECHA_DEF, DEFUNCION) %>%
            mutate(SINT_INGR = as.integer(FECHA_INGRESO - FECHA_SINTOMAS), 
                   SINT_FALLECE = as.integer(FECHA_DEF-FECHA_SINTOMAS))

## Revisar los outliers

p3i <- p3 %>% select(FECHA_SINTOMAS, SINT_INGR)

p3f <- p3 %>% select(FECHA_SINTOMAS, SINT_FALLECE) %>%
            filter( SINT_FALLECE>=0)

p <- ggplot(p3i, aes(x=SINT_INGR))+
            geom_bar()
p +  stat_count(geom = "bar")+
            labs(title = "Frequencia de días que tarda una persona en acudir a la clínica",
         x= "Número de días",
         y= "Frecuencia") +
            theme(axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15))

dev.copy(png, file="./graficas/pregunta3_sintomas_ingreso.png")
dev.off()

ll = function(l){
    n=length(p3i$SINT_INGR)
    loglh = log(l)*sum(p3i$SINT_INGR) - n*l - sum(log(p3i$SINT_INGR))
    -loglh
}


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

boxplot(p3f$SINT_FALLECE)
boxplot(p3i$SINT_INGR)

## Queremos saber si mientras más tiempo tarda una persona en acudir al hospital, más probabilidad de morir tiene

proporciones <- c(NULL)


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
