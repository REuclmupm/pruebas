library(raster)
library(lattice)
library(rasterVis)
## ESta gráfica representa la media anual de todo el periodo de radiación y de CV interanual.

load("mediaAnual_byCluster_resolucion.Rdata")
load("mediaCVanual_byCluster_resolucion.Rdata")
load("mediaCVanual_IP.Rdata")
load("mediaAnual_radiacionIP.Rdata")

anual <- cbind(media_byCluster, mediaCV_byCluster[,2])
colnames(anual) <- c("zone", "mean", "cv")
anual <- as.data.frame(anual)

## Las medias están mal, tengo qhe hacer el cálculo con toda la peninsula

myTheme <- custom.theme(pch = 19, cex = 1.3)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'


xyplot(cv ~ mean, data = anual,
       main = 'CV vs irradiation by cluster',
       scales=list(x=list(cex=1.5), y=list(cex=1.5)),
       xlab=list(label = expression(paste(G[da], (Wh/m^2))), cex=1.3),
       ylab=list(label='CV', cex=1.3),
       par.settings = myTheme
       ) +
    layer({
        panel.abline(h = cV_media, v = media_radiacion, col=4)
        panel.text(x = x + 1.5, y = y - 0.06,
                   labels = zone)
    }, data = anual)




