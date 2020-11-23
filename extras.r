# Se lee la base del sitio de la SSA o de manera local
leo_base <- function(my_filedate){
  # FECHA DE HOY EN FORMATO EN EL QUE NOMBRAN LOS ARCHIVOS
  # ARCHIVOS GUARDADOS
  my_filenames <- list.files(path = "BD", full.names=TRUE)
  # VEMOS SI YA ESTA EL ARCHIVO 
  r <- grepl(my_filedate, my_filenames)
  # LEO LA BASE DE LA PÁGINA O LOCAL
  if(sum(r) == 0){
    temp <- tempfile()
    download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
    my_name <- paste0(my_filedate, "COVID19MEXICO.csv")
    BD <- read.csv(unz(temp, my_name), stringsAsFactors = FALSE)
    unlink(temp)
    write.csv(BD, paste0("BD/", my_name), row.names = FALSE)
    message("La base se leyó del sitio de la SSA y se guardó localmente")
  }else{
    BD <- read.csv(my_filenames[r], as.is = TRUE)
    message("La base se leyó localmente")
  }
  BD
}
