## ESta gráfica representa la media anual de todo el periodo de radiación y de CV interanual.

load("mediaAnual_byCluster_resolucion.Rdata")
load("mediaCVanual_byCluster_resolucion.Rdata")

anual <- cbind(media_byCluster, mediaCV_byCluster[,2])
colnames(anual) <- c("zone", "mean", "cv")

xyplot(cv ~ mean, group = zone, data = anual,
       main = 'CV vs irradiation by cluster',
       panel = function(...){
         panel.abline(h = mean(anual[,3]), v = mean(anual[,2]), col=4)
         panel.xyplot(...)         
       },
       par.settings = myTheme,
       alpha = 0.7,
       as.table = TRUE,
       auto.key = list(space = 'right',
title = 'Model', cex.title = 1))
