## Comparison of productivity by cluster for each data.

library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)

#############################################################################
## 1. DATOS
#############################################################################

## Datos de máscara de clusters los voy cambiando en función del método de clustering y el resultado de índices etc. la máscara del clustering que hice al principio y que era incorrecta está en la carpeta de CVbycluster dentro de satelite

load("data/mascaraClusters20enTierra.Rdata")

## Cargo los datos de productividad para los tres casos:

## FIXED ##

YearlyProductivity_fixedSAT <- stack("data/YearlyProductivity30_fixed_sat.grd")
YearlyProductivity_fixedPROMES <- stack("data/YearlyProductivity20_fixed_promes.grd")
YearlyProductivity_fixedALADIN <- stack("data/YearlyProductivity20_fixed_aladin.grd")

## HORIZ ##

YearlyProductivity_horizSAT <- stack("data/YearlyProductivity30_horiz_sat.grd")
YearlyProductivity_horizPROMES <- stack("data/YearlyProductivity20_horiz_promes.grd")
YearlyProductivity_horizALADIN <- stack("data/YearlyProductivity20_horiz_aladin.grd")

## TWO ##

YearlyProductivity_twoSAT <- stack("data/YearlyProductivity30_two_sat.grd")
YearlyProductivity_twoPROMES <- stack("data/YearlyProductivity20_two_promes.grd")
YearlyProductivity_twoALADIN <- stack("data/YearlyProductivity20_two_aladin.grd")

## los datos de máscara de la península, los hago de nuevo para no incluir las islas.

ext <- as.vector(extent(YearlyProductivity_fixedSAT))
boundaries <- map('worldHires', region=c('Spain','Portugal', 'Andorra'),fill=TRUE, xlim=ext[1:2], ylim= ext[3:4], exact=TRUE, plot=FALSE) #
# EXACT=TRUE es lo que me da únicamente la PI y no las islas
boundaries$names
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=CRS(projection(YearlyProductivity_fixedSAT)))

linea <- as(boundaries_sp, "SpatialLines") ## just the coast line of the IP

###############################################################################
## 2. PREPARACIÓN DE LOS DATOS PARA GRÁFICAS
##############################################################################

library(reshape2)
ksB$fun <- min
ksB <- do.call(mosaic, ksB)

## Media de productividad por cluster:

## FIXED ##

prodCluster_fixedSAT <- zonal(YearlyProductivity_fixedSAT, ksB)
prodCluster_fixedSAT <- as.data.frame(prodCluster_fixedSAT)

prodCluster_fixedPROMES <- zonal(YearlyProductivity_fixedPROMES, ksB)
prodCluster_fixedPROMES <- as.data.frame(prodCluster_fixedPROMES)

prodCluster_fixedALADIN <- zonal(YearlyProductivity_fixedALADIN, ksB)
prodCluster_fixedALADIN <- as.data.frame(prodCluster_fixedALADIN)

## HORIZ ##

prodCluster_horizSAT <- zonal(YearlyProductivity_horizSAT, ksB)
prodCluster_horizSAT <- as.data.frame(prodCluster_horizSAT)

prodCluster_horizPROMES <- zonal(YearlyProductivity_horizPROMES, ksB)
prodCluster_horizPROMES <- as.data.frame(prodCluster_horizPROMES)

prodCluster_horizALADIN <- zonal(YearlyProductivity_horizALADIN, ksB)
prodCluster_horizALADIN <- as.data.frame(prodCluster_horizALADIN)

## TWO ##

prodCluster_twoSAT <- zonal(YearlyProductivity_twoSAT, ksB)
prodCluster_twoSAT <- as.data.frame(prodCluster_twoSAT)

prodCluster_twoPROMES <- zonal(YearlyProductivity_twoPROMES, ksB)
prodCluster_twoPROMES <- as.data.frame(prodCluster_twoPROMES)

prodCluster_twoALADIN <- zonal(YearlyProductivity_twoALADIN, ksB)
prodCluster_twoALADIN <- as.data.frame(prodCluster_twoALADIN)

## añado la categoría 'tipo de seguidor' a cada uno de los dataframes que he creado arriba

prodCluster_fixedSAT$type <- 'FIXED'
prodCluster_horizSAT$type <- 'HORIZ'
prodCluster_twoSAT$type <- 'TWO'

prodCluster_SAT <- rbind(prodCluster_fixedSAT, prodCluster_horizSAT, prodCluster_twoSAT)
prodCluster_SAT$Model <-'SAT'

prodCluster_fixedPROMES$type <- 'FIXED'
prodCluster_horizPROMES$type <- 'HORIZ'
prodCluster_twoPROMES$type <- 'TWO'

prodCluster_PROMES <- rbind(prodCluster_fixedPROMES, prodCluster_horizPROMES, prodCluster_twoPROMES)
prodCluster_PROMES$Model <-'PROMES'

prodCluster_fixedALADIN$type <- 'FIXED'
prodCluster_horizALADIN$type <- 'HORIZ'
prodCluster_twoALADIN$type <- 'TWO'

prodCluster_ALADIN <- rbind(prodCluster_fixedALADIN, prodCluster_horizALADIN, prodCluster_twoALADIN)
prodCluster_ALADIN$Model <-'ALADIN'


## En un mismo dataframe pongo todos los datos:

prodCluster <- rbind(prodCluster_SAT, prodCluster_PROMES, prodCluster_ALADIN)

prodCluster$zone <- factor(prodCluster$zone)

## gráfico de barras

barchart(value~type | zone, data = prodCluster,
         groups= Model,
         stack=FALSE,
         par.settings = custom.theme(pch = 21),
         auto.key = list(space = 'top',
             title = 'model', cex.title = 1),
         main='Yearly productivity by cluster and tracker',
         ylab='Yearly productivity [kWh/kWp]')

## xyplot gráfico

xyplot(value~zone | type, data = prodCluster,
         groups= Model,
         par.settings = custom.theme(pch = 21),
         auto.key = list(space = 'top',
             title = 'model', cex.title = 1),
       panel=function(...) {
           panel.xyplot(...)
           panel.grid()},
       layout=c(3,1),
         main='Yearly productivity by cluster and tracker',
         ylab='Yearly productivity [kWh/kWp]')
