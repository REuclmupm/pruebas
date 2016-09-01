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
## Genera RasterStack de la lista
ksB <- stack(ksB)

## 1.1 satelite
media_sat <- raster("data/media_sat.grd") ## media anual de radiación de todo el periodo (20 años)
CV_anual_sat <- raster("data/CV_anual_sat.grd")

## media_byCluster_sat <- media_byCluster
## mediaCV_byCluster_sat <- mediaCV_byCluster
 
## 1.2 media por cluster

radiacion_byCluster <- mask(media_sat, ksB)
radiacion_byCluster <- as.data.frame(radiacion_byCluster)
names(radiacion_byCluster) <- 1:20

r <- melt(radiacion_byCluster)

## 1.3 cv por clusters

## calculo la variabilidad de la media anual:


                          FUN=function(i)
                              by_Clusters(CV_anual_sat
                                         ,i)
                          )

variabilidad_byCluster <- mask(CV_anual_sat, ksB)
variabilidad_byCluster <- as.data.frame(variabilidad_byCluster)
names(variabilidad_byCluster) <- 1:20

v <- melt(variabilidad_byCluster)

M <- cbind(r, v[,2])
names(M) <- c("cluster", "radiation", "cv")

xyplot(radiation~cv|cluster, data = M)

M$Model <- 'SAT'

## Cargo los datos de promes:

radiacion_promes <- raster("data/media_modelo.grd")
CV_anual_promes <- raster("data/CV_anual_PROMES.grd")

radiacion_promes_byCluster <- mask(radiacion_promes, ksB)
radiacion_promes_byCluster <- as.data.frame(radiacion_promes_byCluster)
names(radiacion_promes_byCluster) <- seq(1:20)

rp <- melt(radiacion_promes_byCluster)

## datos de cv de promes 
variabilidad_promes_byCluster <- mask(CV_anual_promes, ksB)
variabilidad_promes_byCluster <- as.data.frame(variabilidad_promes_byCluster)
names(variabilidad_promes_byCluster) <- 1:20

vp <- melt(variabilidad_promes_byCluster)

cp <- cbind(rp, vp[,2])
names(cp) <- c("cluster", "radiation", "cv")

cp$Model <- 'PROMES'

## cargo los datos de aladin

radiacion_aladin <- raster("data/media_aladin.grd")
CV_anual_aladin <- raster("data/CV_anual_PROMES.grd")

radiacion_aladin_byCluster <- mask(radiacion_aladin, ksB)
radiacion_aladin_byCluster <- as.data.frame(radiacion_aladin_byCluster)
names(radiacion_aladin_byCluster) <- 1:20

ra <- melt(radiacion_aladin_byCluster)

## datos de cv de aladin

variabilidad_aladin_byCluster <- mask(CV_anual_aladin, ksB)
variabilidad_aladin_byCluster <- as.data.frame(variabilidad_aladin_byCluster)
names(variabilidad_aladin_byCluster) <- 1:20

va <- melt(variabilidad_aladin_byCluster)

ca <- cbind(ra, va[,2])
names(ca) <- c("cluster", "radiation", "cv")

ca$Model <- 'ALADIN'


## uno los datos de promes a c:

M <- rbind(M, cp, ca) 

## pruebo a dibujarlo todo:
myTheme <- custom.theme.2(pch = 21, cex = 0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

xyplot(radiation ~ cv|factor(cluster), group = Model, data = M,
       main='CV vs irradiation by cluster and model',
       par.settings = myTheme,
       alpha = 0.6,
       as.table = TRUE,
       auto.key = list(space = 'right',
                       title = 'Model', cex.title = 1))

## Para añadir las líneas de media de radiación y cv en la península

load("data/boundaries.Rdata")

mascara <- mask(media_sat, boundaries_sp)
media <- cellStats(mascara, stat='mean')

mascaracv <- mask(CV_anual_sat, boundaries_sp)
mediacv <-  cellStats(mascaracv, stat='mean')

xyplot(cv ~ radiation|factor(cluster), group = Model, data = M,
       main = 'CV vs irradiation by cluster and model',
       panel = function(...){
         panel.abline(h = mediacv, v = media, col=4)
         panel.xyplot(...)         
       },
       par.settings = myTheme,
       alpha = 0.7,
       as.table = TRUE,
       auto.key = list(space = 'right',
                       title = 'Model', cex.title = 1))
#####################################################################

densityplot(~ radiation|factor(cluster), groups = Model, data = M,
            as.table = TRUE,
            par.settings = myTheme,
            auto.key = list(space = 'right',
                            title = 'Model', cex.title = 1))

densityplot(~ cv|factor(cluster), groups = Model, data = M,
            as.table = TRUE,
            par.settings = myTheme,
            auto.key = list(space = 'right',
                            title = 'Model', cex.title = 1))












