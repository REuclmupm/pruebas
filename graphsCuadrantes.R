## Gráficos de cudrantes para comparar entre clusters y modelos.

library(raster)
library(rasterVis)
library(maps)
library(mapdata)
library(maptools)
library(reshape2)

##############################################################
## 1. DATOS
#############################################################

## Cargo los datos de media por cluster pero también los datos  por cluster.
## Cargo también la máscara

load("data/mascaraClusters20enTierra.Rdata")

## 1.1 satelite

media_sat <- stack("data/media_sat.grd") ## media anual de radiación de todo el periodo (20 años)
CV_anual_sat <- stack("data/CV_anual_sat.grd")

## media_byCluster_sat <- media_byCluster
## mediaCV_byCluster_sat <- mediaCV_byCluster
 
## creo una lista pasando la mascara de clusters.

## creo una lista con los datos de cv

by_Clusters <- function(x, iCluster)
    {
        ## x es el raster que quiero analizar e iCluster el índice que me indica cual de los 6 clusters del raster
        mask(x, ksB[[iCluster]])
    }

nClusters <- length(ksB)

## 1.2 media por cluster

rasters_byCluster <- lapply(seq_len(nClusters),
                          FUN=function(i)
                              by_Clusters(media_sat
                                         ,i)
                          )

radiacion_byCluster <- stack(rasters_byCluster)
radiacion_byCluster <- as.data.frame(radiacion_byCluster)
names(radiacion_byCluster) <- seq(1:20)

r <- melt(radiacion_byCluster)

## 1.3 cv por clusters

## calculo la variabilidad de la media anual:


rastersCV_byCluster <- lapply(seq_len(nClusters),
                          FUN=function(i)
                              by_Clusters(CV_anual_sat
                                         ,i)
                          )

variabilidad_byCluster <- stack(rastersCV_byCluster)
variabilidad_byCluster <- as.data.frame(variabilidad_byCluster)
names(variabilidad_byCluster) <- seq(1:20)

v <- melt(variabilidad_byCluster)

c <- cbind(r, v[,2])
names(c) <- c("cluster", "radiation", "cv")

xyplot(c[,2]~ c[,3]|c[,1])

xyplot(radiation~cv|cluster, data=c)

c$Model <- 'SAT'

## Cargo los datos de promes:

radiacion_promes <- raster("data/media_modelo.grd")
CV_anual_promes <- raster("data/CV_anual_PROMES.grd")

rasters_promes_byCluster <- lapply(seq_len(nClusters),
                          FUN=function(i)
                              by_Clusters(radiacion_promes
                                         ,i)
                          )

radiacion_promes_byCluster <- stack(rasters_promes_byCluster)
radiacion_promes_byCluster <- as.data.frame(radiacion_promes_byCluster)
names(radiacion_promes_byCluster) <- seq(1:20)

rp <- melt(radiacion_promes_byCluster)

## datos de cv de promes

rastersCV_promes_byCluster <- lapply(seq_len(nClusters),
                          FUN=function(i)
                              by_Clusters(CV_anual_promes
                                         ,i)
                          )
 
variabilidad_promes_byCluster <- stack(rastersCV_promes_byCluster)
variabilidad_promes_byCluster <- as.data.frame(variabilidad_promes_byCluster)
names(variabilidad_promes_byCluster) <- seq(1:20)

vp <- melt(variabilidad_promes_byCluster)

cp <- cbind(rp, vp[,2])
names(cp) <- c("cluster", "radiation", "cv")

cp$Model <- 'PROMES'

## cargo los datos de aladin

radiacion_aladin <- raster("data/media_aladin.grd")
CV_anual_aladin <- raster("data/CV_anual_PROMES.grd")

rasters_aladin_byCluster <- lapply(seq_len(nClusters),
                          FUN=function(i)
                              by_Clusters(radiacion_aladin
                                         ,i)
                          )

radiacion_aladin_byCluster <- stack(rasters_aladin_byCluster)
radiacion_aladin_byCluster <- as.data.frame(radiacion_aladin_byCluster)
names(radiacion_aladin_byCluster) <- seq(1:20)

ra <- melt(radiacion_aladin_byCluster)

## datos de cv de aladin

rastersCV_aladin_byCluster <- lapply(seq_len(nClusters),
                          FUN=function(i)
                              by_Clusters(CV_anual_aladin
                                         ,i)
                          )
 
variabilidad_aladin_byCluster <- stack(rastersCV_aladin_byCluster)
variabilidad_aladin_byCluster <- as.data.frame(variabilidad_aladin_byCluster)
names(variabilidad_aladin_byCluster) <- seq(1:20)

va <- melt(variabilidad_aladin_byCluster)

ca <- cbind(ra, va[,2])
names(ca) <- c("cluster", "radiation", "cv")

ca$Model <- 'ALADIN'


## uno los datos de promes a c:

c <- rbind(c, cp, ca) 

## pruebo a dibujarlo todo:

xyplot(radiation~ cv|factor(cluster), group=Model, data=c,
       main='CV vs irradiation by cluster and model',
       par.settings = custom.theme(pch = 21),
       auto.key = list(space = 'right',
                       title = 'Model', cex.title = 1))

## Para añadir las líneas de media de radiación y cv en la península

load("data/boundaries.Rdata")

mascara <- mask(media_sat, boundaries_sp)
media <- cellStats(mascara, stat='mean')

mascaracv <- mask(CV_anual_sat, boundaries_sp)
mediacv <-  cellStats(mascaracv, stat='mean')

xyplot(cv~radiation|factor(cluster), group=Model, data=c,
       main='CV vs irradiation by cluster and model',
       panel=function(...){
         panel.abline(h=mediacv,v=media, col=4)
         panel.xyplot(...)         
       },
       par.settings = custom.theme(pch = 21),
       auto.key = list(space = 'right',
                       title = 'Model', cex.title = 1))
#####################################################################











