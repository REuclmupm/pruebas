library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(rgdal)
library(ncdf4)

## 1. DATOS

media_modelo <- raster("data/media_modelo.grd")
media_aladin <- raster("data/media_aladin.grd")
media_sat <- raster("data/media_sat.grd")
load("data/boundaries.Rdata")
load("data/linea.Rdata")

## 2. Calculo la diferencia relativa entre los modelos y el satelite

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

## 3. GRÁFICAS

library(RColorBrewer)

## 3.a primera opción (mejor para datos simétricos).


div.pal <- brewer.pal(n=10, 'RdGy')
div.pal[6] <- "#FFFFFF"

showPal <- function(pal, labs=pal, cex=0.6, ...){
    barplot(rep(1, length(pal)), col=pal, names.arg=labs, cex.names=cex, axes=FALSE,...)
}

div.pal <- rev(div.pal)

divTheme <- rasterTheme(region=div.pal)

levelplot(mask(PromesSat, boundaries_sp), margin=FALSE,, contour=TRUE, par.settings=divTheme)+layer(sp.lines(linea))

## 3.b segunda opción (mejor para datos asimétricos).

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

break2pal <- function(x,msx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)
showPal(pal, round(mids,1))

levelplot(mask(PromesSat, boundaries_sp), margin=FALSE, contour=TRUE, par.settings=rasterTheme(region=pal))+layer(sp.lines(linea))

##################

## OPCION 1:

levelplot(mask(PromesSat, boundaries_sp), margin=FALSE,, contour=TRUE, par.settings=divTheme)+layer(sp.lines(linea))
levelplot(mask(AladinSat, boundaries_sp), margin=FALSE,, contour=TRUE, par.settings=divTheme)+layer(sp.lines(linea))

## OPCION 2:

ab <- stack(PromesSat, AladinSat)
names(ab) <- c("PROMES", "ALADIN")

levelplot(mask(ab, boundaries_sp), margin=FALSE,, contour=TRUE, par.settings=rasterTheme(region=pal))+layer(sp.lines(linea))




