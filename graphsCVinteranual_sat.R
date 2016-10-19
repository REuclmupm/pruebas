## graphs to show diferences in CV

library(raster)
library(rasterVis)
library(rgdal)
library(maps)
library(mapdata)
library(maptools)

###########################################
## 1. DATOS
##########################################

## 1.1 Datos de satelite para hacer el gráfico de media anual

load("mediaAnual_byCluster_resolucion.Rdata") ## media anual de radiación
load("mediaCVanual_byCluster_resolucion.Rdata") ## CV por cluster

##############################################################
## 2. Manipulación de datos para representar
#############################################################

## 2.1 satelite MEDIA ANUAL

media_byCluster_sat <- as.data.frame(media_byCluster)
mediaCV_byCluster_sat <- as.data.frame(mediaCV_byCluster)

media_byCluster_sat$cv <- mediaCV_byCluster_sat[,2]
media_byCluster_sat$Model <- 'SAT'

satelite <- media_byCluster_sat

## 2.2 satelite MEDIA MENSUAL de variabilidad por cluster

load("mediaCVmensual_byCluster_resolucion.Rdata")

###################################################################
## 3. Gráficas 
#################################################################

## para la media anual de radiación. Es la gráfica CuadranteAnual pero sin las medias de la peninsula. 

xyplot(cv ~ mean, data = satelite,
       group = as.factor(zone),
       main='Daily irradiation annual mean vs coefficient of variation',
       xlab='kWh',
       type = 'b', 
       par.settings = custom.theme(pch = 21),
       auto.key = list(space = 'right',
title = 'cluster', cex.title = 1))

## gráfica para los meses:
 
myTheme <- custom.theme.2(pch = 8, cex = 0.3)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

xyplot(value ~ Month|as.factor(zone), data = cvCluster,
       
       par.settings = myTheme,
       panel=function(...){
           panel.xyplot(...)
           panel.grid()},
       alpha = 1,
       as.table = TRUE,
       ylab='CV',
       auto.key = list(space = 'right',
title = 'Model', cex.title = 1))


## Gráficas para hacer boxplot. Necesito los rasters:

cv_mensual <- stack("cv_mensual_resolucion.grd")
idx <- seq(as.Date('1983-01-01'), as.Date('2013-12-31'), 'month')
cv_mensual <- setZ(cv_mensual, idx)

month <- function(x) as.numeric(format(x, '%m'))

cv_mensual12 <- zApply(cv_mensual, by=month, fun='mean')

## Para obtener los datos por meses que estan guardados como .Rdata puedo usar zonal

## ksB$fun <- min
## ksB <- do.call(mosaic, ksB)

## a  <- zonal(cv_mensual12, ksB)

ksB2 <- stack(ksB)

by_Clusters <- function(x, iCluster)
    {
        ## x es el raster que quiero analizar e iCluster el índice que me indica cual de los 19 clusters del raster
        mask(x, ksB2[[iCluster]])
    }

load("mascaraClusters19enTierra_resolucion.Rdata")

nClusters <- length(ksB)

## 1.2 media por cluster

ksB2 <- stack(ksB)

rastersCV_byCluster <- lapply(seq_len(nClusters),
                          FUN=function(i)
                              by_Clusters(cv_mensual12
                                         ,i)
                          )

## Esta última lista contiene 19 rasterStack cada uno de ellos tiene 12 capas con el ciclo anual para cada cluster.

prueba <- lapply(rastersCV_byCluster, as.data.frame) ## contiene 19 dataframes con los datos de variabilidad por mes de cada cluster. (no son las medias por cluster, es cada celda)

prueba[[1]]$zone <- 1
prueba[[2]]$zone <- 2

rbind(prueba[[1]],prueba[[2]])
