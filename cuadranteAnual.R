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

myTheme <- custom.theme.2(pch = 21, cex = 1.3)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'


xyplot(cv ~ mean, group = as.factor(zone), data = anual,
       main = 'CV vs irradiation by cluster',
       scales=list(x=list(cex=1.5), y=list(cex=1.5)),
       xlab=list(label = expression(paste(G[da], (Wh/m^2))), cex=1.3),
       ylab=list(label='CV', cex=1.3),
       panel = function(...){
         panel.abline(h = cV_media, v = media_radiacion, col=4)
         panel.text(x=anual[,2]+1.5, y= anual[,3]-0.06,labels=anual[,1])
         panel.xyplot(...)         
       },
       par.settings = myTheme,
       alpha = 1,
       as.table = TRUE
       #auto.key = list(space = 'right',
        #   title = 'cluster', cex.title = 1))
)



