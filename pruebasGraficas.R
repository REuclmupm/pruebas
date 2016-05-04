library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(rgdal)
library(ncdf4)

##  DATOS

media_modelo <- raster("data/media_modelo.grd")
media_aladin <- raster("data/media_aladin.grd")
media_sat <- raster("data/media_sat.grd")
load("data/boundaries.Rdata")
load("data/linea.Rdata")

###################################################################
## 1. GRÁFICA DIFERENCIA RELATIVA MEDIA TOTAL
##################################################################3

## 1.1. Calculo la diferencia relativa entre los modelos y el satelite

compaRe <- function(x,y,z)
    {
                                        # z puede ser 'bias' o 'rel' según quiera calcular el bias o la diferencia relativa de x y z
        if (z == 'bias')
            {
        bias <- (x)-(y)
        return(bias)
    }
        else
            {
        rel <-((x-y)/(x))*100
        return(rel)
    }
}


PromesSat <- compaRe(media_modelo, media_sat, 'dif')
AladinSat <- compaRe(media_aladin, media_sat, 'dif')

## 1.2. GRÁFICA



## a) primera opción (mejor para datos simétricos).

## Una paleta con un número impar de valores para que el centro podamos asociarlo al cero
div.pal <- brewer.pal(n=11, 'RdGy')
## Reemplazamos el "blanco" de la paleta por un blanco puro.
div.pal[6] <- "#FFFFFF"

showPal <- function(pal, labs=pal, cex=0.6, ...){
    barplot(rep(1, length(pal)), col=pal, names.arg=labs, cex.names=cex, axes=FALSE,...)
}

div.pal <- rev(div.pal)

divTheme <- rasterTheme(region=div.pal)

## Sólo necesitamos los valores en terreno (filtramos el mar)
promesSatMk <- mask(PromesSat, boundaries_sp)

levelplot(promesSatMk,
          margin=FALSE,
          contour=TRUE,
          par.settings=divTheme) +
    layer(sp.lines(linea))

## b) segunda opción (mejor para datos asimétricos).
## Ver explicación en https://github.com/oscarperpinan/spacetime-vis/blob/master/raster.R#L97
rng <- range(PromesSat[])
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(PromesSat[], breaks, rightmost.closed=TRUE)
mids <-tapply(PromesSat[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)
showPal(pal, round(mids,1))

levelplot(promesSatMk,
          at = breaks,
          margin=FALSE, contour=TRUE,
          par.settings=rasterTheme(region=pal)) +
    layer(sp.lines(linea))

##################

## OPCION 1:

levelplot(mask(PromesSat, boundaries_sp), margin=FALSE,, contour=TRUE, par.settings=divTheme)+layer(sp.lines(linea))
levelplot(mask(AladinSat, boundaries_sp), margin=FALSE,, contour=TRUE, par.settings=divTheme)+layer(sp.lines(linea))

## OPCION 2:

ab <- stack(PromesSat, AladinSat)
names(ab) <- c("PROMES", "ALADIN")

levelplot(mask(ab, boundaries_sp), margin=FALSE,, contour=TRUE, par.settings=rasterTheme(region=pal))+layer(sp.lines(linea))

################################################################################
## 2. GRÁFICA VARIABILIDAD MENSUAL POR CLUSTER
###############################################################################3

## cargo los datos y la máscara de clusters

load("data/mascaraClustersSat.Rdata")
 
CV_mensual_aladinCor <- stack("data/CV_mensual_aladinCor.grd")
CV_mensual_modeloCor <- stack("data/CV_mensual_modeloCor.grd")
CV_mensual_sat <- stack("data/CV_mensual_sat.grd")

media_mensual240_aladinCor <- stack("data/media_mensual240_aladinCor.grd")
media_mensual240_modeloCor <- stack("data/media_mensual240_modeloCor.grd")
media_mensual240_sat <- stack("data/media_mensual240_sat.grd")

idx <- seq(as.Date('1989-01-01'), as.Date('2008-12-31'), 'month')

media_mensual240_aladinCor <- setZ(media_mensual240_aladinCor, idx)
media_mensual240_modeloCor <- setZ(media_mensual240_modeloCor, idx)
media_mensual240_sat <- setZ(media_mensual240_sat, idx)

by_Clusters <- function(x, iCluster)
    {
        ## x es el raster que quiero analizar e iCluster el índice que me indica cual de los 6 clusters del raster
        mask(x, ksB[[iCluster]])
    }

nClusters <- length(ksB)

month <- function(x) as.numeric(format(x, '%m'))

VariabilidadMensual <- function(x) {
    sd <- zApply(x, by=month, fun='sd')
    media <- zApply(x, by=month, fun='mean')
    COV <- sd/media
    return(COV)
}


## Calculo la media de variabilidad por cluster

## 4.2.a variabilidad mensual PROMES

cv_byCluster_mensual_modeloCor<- lapply(seq_len(nClusters),
                      FUN=function(i)
                          by_Clusters(VariabilidadMensual(media_mensual240_modeloCor),
                                      i))


mediacv_byCluster_mensual_modeloCor <- lapply(cv_byCluster_mensual_modeloCor,
                                      FUN=function(i)
                                                     cellStats(i,
                                                               'mean'))


## 4.2.b variabilidad mensual ALADIN

cv_byCluster_mensual_aladin <- lapply(seq_len(nClusters),
                      FUN=function(i)
                          by_Clusters(VariabilidadMensual(media_mensual240_aladinCor),
                                      i))



mediacv_byCluster_mensual_aladin <- lapply(cv_byCluster_mensual_aladin,
                                      FUN=function(i)
                                                     cellStats(i,
                                                               'mean'))


## 4.2.c variabilidad mensual CMSAF

cv_byCluster_mensual_sat<- lapply(seq_len(nClusters),
                      FUN=function(i)
                          by_Clusters(VariabilidadMensual(media_mensual240_sat),
                                      i))


mediacv_byCluster_mensual_sat <- lapply(cv_byCluster_mensual_sat,
                                      FUN=function(i)
                                                     cellStats(i,
                                                               'mean'))


## GRÁFICA. Datos en data.frame


a <- lapply(mediacv_byCluster_mensual_modeloCor, FUN=function(x) as.data.frame(x))
b <- lapply(mediacv_byCluster_mensual_aladin, FUN=function(x) as.data.frame(x))
c <- lapply(mediacv_byCluster_mensual_sat, FUN=function(x) as.data.frame(x))

a2 <- unlist(a)
b2 <- unlist(b)
c2 <- unlist(c)

clusters <- c(rep(1,12), rep(2,12),rep(3,12), rep(4,12),rep(5,12), rep(6,12))
meses <- rep(1:12, 6)

data <- data.frame(a2,b2,c2)
data$month <- meses
data$clusters <- clusters

variabilidad <- c(a2,b2,c2)
modelos <- c(rep("PROMES", 72), rep("ALADIN", 72), rep("SAT",72))

datos <- data.frame(modelos,variabilidad, clusters, meses)

xyplot(variabilidad~meses|clusters, data=datos, group=modelos, type='l', auto.key=TRUE)
