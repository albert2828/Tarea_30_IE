library(dplyr)
library(ggplot2)

cambio <- function(fecha){
            if (fecha == "9999-99-99"){
                        fecha = "1900-01-31"
            }            
            else {
                        fecha = fecha
            }
            fecha
}


cambio_sex <- function(s){
            if(s==1)s = "M"
            else s="H"
            
}

cambio_sino <- function(s){
            if(s==1)s = 1
            else if(s==2) s=0
            else s=NA
}