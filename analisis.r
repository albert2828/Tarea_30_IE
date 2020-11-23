# DIRECTORIO DE TRABAJO
setwd("/Users/carloserwin/Google Drive2/INFERENCIA/6EXTRAS/COVID_CLASE")
# CARGO FUNCIONES Y LIBRERIAS (SE IRÁN AGREGANDO FUNCIONES)
source("R/extras.r")

# LEO LA BASE DE DATOS DEL SITIO DE LA SSA
my_filedate <- "201109"     # format(Sys.Date(), "%y%m%d")
                            # formato "yymmdd", es mejor probar con el día anterior
tt <- system.time(BD <- leo_base(my_filedate))[3]
tt # tiempo de lectura de la base 
   # 33 seg cuando es local (se leyó del servidor anteriormente y quedó guardada)
   # 110 seg cuando se lee de la página de la SSA

# Variables 
names(BD)

# Como se ven los primeros renglones de la BD
head(BD)

###########################################################
# ES IMPERATIVO REVISAR BIEN EL DICCIONARIO DE VARIABLES  #
###########################################################

# CAMBIAMOS LAS FECHAS A FORMATO FECHA
BD$FECHA_INGRESO <- as.Date(BD$FECHA_INGRESO, "%Y-%m-%d")
BD$FECHA_SINTOMAS <- as.Date(BD$FECHA_SINTOMAS, "%Y-%m-%d")
BD$FECHA_DEF[BD$FECHA_DEF == "9999-99-99"] <- NA
BD$FECHA_DEF <- as.Date(BD$FECHA_DEF, "%Y-%m-%d")

# CURVA EPIDÉMICA OBSERVADA NACIONAL
BD$CASO <- 1*(BD$RESULTADO_LAB == 1)  # CASOS CONFIRMADOS X PRUEBA DE LABORATORIO
table(BD$CASO)

tb <- table(BD$FECHA_SINTOMAS[BD$CASO == 1])

CASOS <- data.frame(FECHA = as.Date(names(tb)), CASOS = as.numeric(tb))
id <- CASOS$FECHA > "2020-02-28" &  CASOS$FECHA < max(CASOS$FECHA) - 15
CASOS <- CASOS[id, ]

# Graficamos desde el 29 de febrero 
# (la SSA dice que en esa fecha se registró el primer caso) 
# hasta 15 días antes de la fecha de corte de la base
# (tardan mucho en actualizar los datos)
plot(CASOS, type = "b", col = "red", ylab = "", xlab = "", 
     pch = 20, lwd = 2, axes = FALSE, ylim = c(0, 10000))
axis.Date(1, at = seq(min(CASOS$FECHA), max(CASOS$FECHA), by = 15), 
          las = 2, format = "%Y-%m-%d", cex.axis = 0.8)
my_seq <- seq(0, 10000, by = 1000)
axis(2, at = my_seq,
     labels = format(my_seq, big.mark=",", scientific=FALSE), 
     las = 2, cex.axis = 0.8)
# Ajusto un spline sólo para ver la tendencia claramente
my_spline <- smooth.spline(CASOS, df = 7)
lines(my_spline, col = "black", lwd = 2)
box(lwd = 2)