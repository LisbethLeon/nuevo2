# MAPA DE PRECIPITACIÓN CON DATOS PISCO
En este trabajo nos centraremos en la comparación de datos de Senamhi de 5 estaciones del departamento de Cusco. Este trabajo fue realizado por los siguientes estudiantes

Camacho Vega Mercedes

Leon Chaiña Lisbeth Karola

Ramos Cerna Alejandra Gianella

1. COMENZAREMOS INSTALANDO NUESTRAS LIBRERÍAS
  
       install.packages("raster")
       install.packages("ncdf4")
       library(raster)
       library(ncdf4)

2. SE COLOCA LA DIRECCIÓN DE TU DIRECCIÓN DE TRABAJO

       setwd("C:/Users/LEO/Desktop/proyecto/FINAL")
3. CARGAMOS EL ARCHIVO EN CSV Y 

       long_lat <- read.csv("estaciones_c.csv", header = T)
       View(long_lat)
4. ENSAMBLAMOS LOS DATOS .nc DE NUESTRA DATA GRILLADA

        raster_pp   <- raster::brick("FINAL/DATA GRILLADA/Precipitacion/Mensual/Prec.nc")
        raster_tmax <- raster::brick("FINAL/DATA GRILLADA/Temperatura/Mensual/Tmax.nc")
        raster_tmin <- raster::brick("FINAL/DATA GRILLADA/Temperatura/Mensual/Tmin.nc")

5. ASIGNAMOS LAS COORDENADAS

        sp::coordinates(long_lat) <- ~XX+YY

        raster::projection(long_lat) <- raster::projection(raster_pp)
        raster::projection(long_lat) <- raster::projection(raster_tmax)
        raster::projection(long_lat) <- raster::projection(raster_tmin)

6. LEEMOS LA PRECIPITACION

        points_long_lat_pp <- raster::extract(raster_pp[[1]], long_lat, cellnumbers = T)[,1]
        data_long_lat_pp <- t(raster_pp[points_long_lat_pp])
        colnames(data_long_lat_pp) <- as.character(long_lat$NN)
        write.csv(data_long_lat_pp, "FINAL/DATA PISCO/PP MENSUAL/prep.csv", quote = F)

7. LEEMOS LAS TEMPERATURAS

        #TEMPERATURA MAXIMA

        points_long_lat_tmax <- raster::extract(raster_tmax[[1]], long_lat, cellnumbers = T)[,1]
        data_long_lat_tmax <- t(raster_tmax[points_long_lat_tmax])
        colnames(data_long_lat_tmax) <- as.character(long_lat$NN)
        write.csv(data_long_lat_tmax, "FINAL/DATA PISCO/TMAX/tmax.csv", quote = F)

        #TEMPERATURA MINIMA

        points_long_lat_tmin <- raster::extract(raster_tmin[[1]], long_lat, cellnumbers = T)[,1]
        data_long_lat_tmin <- t(raster_tmin[points_long_lat_tmin])
        colnames(data_long_lat_tmin) <- as.character(long_lat$NN)
        write.csv(data_long_lat_tmin, "FINAL/DATA PISCO/TMIN/tmin.csv", quote = F)

        #TEMPERATURA MEDIA

        tmedia <- (data_long_lat_tmax+data_long_lat_tmin)/2
        write.csv(tmedia, "FINAL/DATA PISCO/tmedia.csv", quote = F)

8. INSTALAMOS LAS LIBRERÍAS

        install.packages("tidyverse")
        install.packages("ggplot2")
        install.packages("dplyr")
        library(tidyverse)
        library(ggplot2)
        library(dplyr)
        
9. ASIGNACIÓN DE SERIES TEMPORALES PARA LAS PRECIPITACIONES Y TEMPERATURAS, SE ASIGNA DESDE 1981 HASTA EL 2016 DEBIDO A LA DISTRIBUCIÓN TEMPORAL DE LOS DATOS

        Pp_pisco <- read.csv("FINAL/DATA PISCO/PP MENSUAL/prep.csv", header = T, sep = ",") %>%
          tibble() %>%
          dplyr::select(-X) %>%
          mutate(fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month"))
        View(Pp_pisco)
        write.csv(Pp_pisco, "FINAL/DATA EN CSV/PP ESTACIONES/pp.csv")
        colnames(Pp_pisco)

        tmin_pisco <- read.csv("FINAL/DATA PISCO/TMIN/tmin.csv", header = T, sep = ",") %>%
          tibble() %>%
          dplyr::select(-X) %>%
          mutate(fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month"))
        View(tmin_pisco)
        write.csv(tmin_pisco, "FINAL/DATA EN CSV/PP ESTACIONES/tmin.csv")
        colnames(tmin_pisco)

        tmax_pisco <- read.csv("DATA PISCO/TMAX/tmax.csv", header = T, sep = ",") %>%
          tibble() %>%
          dplyr::select(-X) %>%
          mutate(fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month"))
        View(tmax_pisco)
        write.csv(tmax_pisco, "DATA EN CSV/PP ESTACIONES/tmax.csv")
        colnames(tmax_pisco)

        tmedia_pisco <- read.csv("FINAL/DATA PISCO/tmedia.csv", header = T, sep = ",") %>%
          tibble() %>%
          dplyr::select(-X) %>%
          mutate(fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month"))
        View(tmedia_pisco)
        write.csv(tmedia_pisco, "FINAL/DATA EN CSV/PP ESTACIONES/tmedia.csv")
        colnames(tmedia_pisco)
        
10. GRAFICAS CON R NATIVO DE LOS 5 CENTROS HIDROMETEOROLÓGICOS

        pisac <- plot(Pp_pisco$fecha,Pp_pisco$PISAC, type = "l", col= 'blue',
                      main= 'Serie de Tiempo de la estación Pisac', xlab= 'Año',
                      ylab= 'Precipitación')
![Rplot](https://user-images.githubusercontent.com/77855207/107093083-a8570380-67d2-11eb-9b43-c75ceefe0bc4.png)

        Paruro <- plot(Pp_pisco$fecha, Pp_pisco$PARURO, type = "l", col= 'blue',
                       main= 'Serie de Tiempo de la estación Paruro', xlab= 'Año',
                       ylab= 'Precipitación')
![Rplot01](https://user-images.githubusercontent.com/77855207/107093144-c0c71e00-67d2-11eb-897d-ca071962eafc.png)

        colquepata <- plot(Pp_pisco$fecha,Pp_pisco$COLQUEPATA, type = "l", col= 'blue',
                           main= 'Serie de Tiempo de la estación Colquepata', xlab= 'Año',
                           ylab= 'Precipitación')
![Rplot02](https://user-images.githubusercontent.com/77855207/107093489-51056300-67d3-11eb-8c33-90ee9f0eb9eb.png)

        catca <-  plot(Pp_pisco$fecha,Pp_pisco$CCATCCA, type = "l", col= 'blue',
                       main= 'Serie de Tiempo de la estación Ccatcca', xlab= 'Año',
                       ylab= 'Precipitación')
![CCATCCA](https://user-images.githubusercontent.com/77855207/107093233-e6542780-67d2-11eb-9fcb-f64554447656.png)

        caycay <- plot(Pp_pisco$fecha,Pp_pisco$CAICAY, type = "l", col= 'blue',
                       main= 'Serie de Tiempo de la estación Cay cay', xlab= 'Año',
                       ylab= 'Precipitación')
![Rplot04](https://user-images.githubusercontent.com/77855207/107093550-6da19b00-67d3-11eb-9dfe-f3ec9e07cffa.png)

11. GRAFICAS AVANZADAS DE LOS 5 CENTROS HIDROMETEOROLÓGICOS
Cargamos la librería ggplot2

        library(ggplot2)

        ggplot(Pp_pisco, aes(fecha, PISAC)) +
          geom_line() +
          geom_smooth(span = 0.4)
        names(Pp_pisco)
        
        ggplot(Pp_pisco, aes(fecha, PARURO)) +
          geom_line() +
          geom_smooth(span = 0.4)
          
        ggplot(Pp_pisco, aes(fecha, COLQUEPATA)) +
          geom_line() +
          geom_smooth(span = 0.5)
          
        ggplot(Pp_pisco, aes(fecha, CAICAY)) +
          geom_line() +
          geom_smooth(span = 0.5)
          
        ggplot(Pp_pisco, aes(fecha, CCATCCA)) +
          geom_line() +
          geom_smooth(span = 0.5)
          
Con la función de ggplot nos ayudamos para crear historigramas de las frecuencias de temperaturas

        ggplot(tmedia_pisco, aes(PISAC)) + geom_histogram(colour= 'blue')
        
        
        ggplot(tmedia_pisco, aes(PARURO)) + geom_histogram(colour= 'blue')
        
        
        ggplot(tmedia_pisco, aes(COLQUEPATA)) + geom_histogram(colour= 'blue')
        
        
        ggplot(tmedia_pisco, aes(CCATCCA)) + geom_histogram(colour= 'blue')
        
        
        ggplot(tmedia_pisco, aes(CAICAY)) + geom_histogram(colour= 'blue')

