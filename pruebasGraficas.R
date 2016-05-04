library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(rgdal)
library(ncdf4)
library(reshape2)

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

## cargo la máscara de clusters
load("data/mascaraClustersSat.Rdata")
## Convierto la lista de Raster a un RasterLayer con mosaic
ksB$fun <- min
ksB <- do.call(mosaic, ksB)
## Cargo datos de modelos 
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


## Cálculo del coeficiente de variación mensual
month <- function(x) as.numeric(format(x, '%m'))

VariabilidadMensual <- function(x) {
    sd <- zApply(x, by=month, fun='sd')
    media <- zApply(x, by=month, fun='mean')
    COV <- sd/media
    return(COV)
}



## 4.2.a variabilidad mensual PROMES
cvMensualPROMES <- VariabilidadMensual(media_mensual240_modeloCor)
## Coeficiente de variación por zonas
cvClusterPROMES <- zonal(cvMensualPROMES, ksB)
## El resultado es una matriz. Lo paso a data.frame...
cvClusterPROMES <- as.data.frame(cvClusterPROMES)
## En formato largo con reshape2::melt
cvClusterPROMES <- melt(cvClusterPROMES,
                     id = 'zone',
                     variable.name = 'Month')
cvClusterPROMES$Month <- as.numeric(gsub("X", "",
                                         cvClusterPROMES$Month))
cvClusterPROMES$Model <- 'PROMES'

## 4.2.b variabilidad mensual ALADIN
cvMensualALADIN <- VariabilidadMensual(media_mensual240_aladinCor)
cvClusterALADIN <- zonal(cvMensualALADIN, ksB)
cvClusterALADIN <- as.data.frame(cvClusterALADIN)
cvClusterALADIN <- melt(cvClusterALADIN,
                     id = 'zone',
                     variable.name = 'Month')
cvClusterALADIN$Month <- as.numeric(gsub("X", "",
                                         cvClusterALADIN$Month))
cvClusterALADIN$Model <- 'ALADIN'

## 4.2.c variabilidad mensual CMSAF
cvMensualSAT <- VariabilidadMensual(media_mensual240_sat)
cvClusterSAT <- zonal(cvMensualSAT, ksB)
cvClusterSAT <- as.data.frame(cvClusterSAT)
cvClusterSAT <- melt(cvClusterSAT,
                     id = 'zone',
                     variable.name = 'Month')
cvClusterSAT$Month <- as.numeric(gsub("X", "",
                                         cvClusterSAT$Month))
cvClusterSAT$Model <- 'CMSAF'

## GRÁFICA. Datos en data.frame
CVModels <- rbind(cvClusterPROMES,
                  cvClusterALADIN,
                  cvClusterSAT)
CVModels$zone <- factor(CVModels$zone)

xyplot(value ~ Month | zone, data = CVModels,
       group = Model,
       type = 'b', 
       par.settings = custom.theme(pch = 21),
       auto.key = list(space = 'right',
                       title = 'Model', cex.title = 1))

xyplot(value ~ Month | Model, data = CVModels,
       group = zone,
       type = 'b', layout = c(3, 1),
       par.settings = custom.theme(pch = 21),
       auto.key = list(space = 'right',
                       title = 'Cluster', cex.title = 1))
