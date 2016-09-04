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

## clusters

load("data/mascaraClusters20enTierra.Rdata")

## lista de clusters como stack

ksB <- stack(ksB)

## 1.1 Datos de satelite

media_sat <- raster("data/media_sat.grd") ## media anual de radiación
CV_anual_sat <- raster("data/CV_anual_sat.grd") ## CV interanual
 
## Hago la media por cluster

media_byCluster_sat <- mask(media_sat, ksB) ## radiación por cluster
media_byCluster_sat <- cellStats(media_byCluster_sat, stat='mean')

mediaCV_byCluster_sat <- mask(CV_anual_sat, ksB)
mediaCV_byCluster_sat <- cellStats(mediaCV_byCluster_sat, stat='mean')


## 1.2 Datos de PROMES

media_promes <- raster("data/media_modelo.grd")
CV_anual_promes <- raster("data/CV_anual_PROMES.grd")

## Hago la media por cluster

media_byCluster_promes <- mask(media_promes, ksB) ## radiación por cluster
media_byCluster_promes <- cellStats(media_byCluster_promes, stat='mean')

mediaCV_byCluster_promes <- mask(CV_anual_promes, ksB)
mediaCV_byCluster_promes <- cellStats(mediaCV_byCluster_promes, stat='mean')


## 1.3 Datos de ALADIN

media_aladin <- raster("data/media_aladin.grd")
CV_anual_aladin <- raster("data/CV_anual_aladin.grd")


## Hago la media por cluster

media_byCluster_aladin <- mask(media_aladin, ksB) ## radiación por cluster
media_byCluster_aladin <- cellStats(media_byCluster_aladin, stat='mean')

mediaCV_byCluster_aladin <- mask(CV_anual_aladin, ksB)
mediaCV_byCluster_aladin <- cellStats(mediaCV_byCluster_aladin, stat='mean')


##############################################################
## 2. Manipulación de datos para representar
#############################################################

## 2.1 satelite

names(media_byCluster_sat) <- 1:20
names(mediaCV_byCluster_sat) <- 1:20

media_byCluster_sat <- as.data.frame(media_byCluster_sat)
mediaCV_byCluster_sat <- as.data.frame(mediaCV_byCluster_sat)

satelite <- cbind(media_byCluster_sat, mediaCV_byCluster_sat)
names(satelite) <- c("radiacion", "cv")

satelite$zone <- rownames(satelite)
satelite$Model <- "SAT"

## 2.2 PROMES

names(media_byCluster_promes) <- 1:20
names(mediaCV_byCluster_promes) <- 1:20

media_byCluster_promes <- as.data.frame(media_byCluster_promes)
mediaCV_byCluster_promes <- as.data.frame(mediaCV_byCluster_promes)

promes <- cbind(media_byCluster_promes, mediaCV_byCluster_promes)
names(promes) <- c("radiacion", "cv")

promes$zone <- rownames(promes)
promes$Model <- "PROMES"

## 2.3 ALADIN

names(media_byCluster_aladin) <- 1:20
names(mediaCV_byCluster_aladin) <- 1:20

media_byCluster_aladin <- as.data.frame(media_byCluster_aladin)
mediaCV_byCluster_aladin <- as.data.frame(mediaCV_byCluster_aladin)

aladin <- cbind(media_byCluster_aladin, mediaCV_byCluster_aladin)
names(aladin) <- c("radiacion", "cv")

aladin$zone <- rownames(aladin)
aladin$Model <- "ALADIN"

###################################################################
## 3. Gráficas 
#################################################################

## un unico dataframe

comparacion <- rbind(satelite,promes,aladin)

xyplot(cv ~ radiacion | as.factor(Model), data = comparacion,
       group = as.factor(zone),
       layout=c(1,3),
       main='Daily irradiation annual mean vs coefficient of variation',
       xlab='kWh',
       type = 'b', 
       par.settings = custom.theme(pch = 21),
       auto.key = list(space = 'right',
title = 'cluster', cex.title = 1))

xyplot(cv ~ radiacion | as.factor(zone), data = comparacion,
       group = as.factor(Model),
       main='Daily irradiation annual mean vs coefficient of variation',
       xlab='kWh',
       type = 'b', 
       par.settings = custom.theme(pch = 21),
       auto.key = list(space = 'right',
title = 'cluster', cex.title = 1))

