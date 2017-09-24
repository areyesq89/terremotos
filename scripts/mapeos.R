
library(rgdal)
library(ggmap)
library(tidyverse)
library(cowplot)

data2017 <- readOGR("../data/Coordenadas2017.kml")
data1989 <- readOGR("../data/Coordenadas1985.kml")

colnames( coords1989 ) <- c("lon", "lat", "alt")
coords1989 <- as.data.frame( coords1989 )
coords2017 <- data2017@coords
colnames( coords2017 ) <- c("lon", "lat", "alt")
coords2017 <- as.data.frame( coords2017 )

coords <- rbind(
    mutate( coords1989, year=1985 ),
    mutate( coords2017, year=2017 ) )
coords <- mutate( coords, year=as.factor(year) )

cdmxTerr <- get_map( 'Mexico city', zoom=11 )
ggmap( cdmxTerr ) +
    xlim(-99.20, -99.10) + ylim(19.30, 19.47)
cdmx <- get_map( 'Mexico city', zoom = 11, maptype="terrain" )

contourPlots <- ggmap(cdmx) + stat_density2d(aes(x = lon, y = lat, 
                                 fill=year,
                                 color=year,
                                 alpha=..level..
                                 ), bins = 20, 
  data = coords, geom = 'polygon') +
    xlim(-99.20, -99.10) + ylim(19.30, 19.47) +
    facet_grid(~year) +
    scale_alpha(range = c(0.001, 0.1)) +
    guides(alpha=FALSE, fill=FALSE, color=FALSE) +
    xlab("Longitud") + ylab("Latitud") + theme( axis.text.x=element_blank() )

contourPlots

boxplotLat <- ggplot( coords, aes( year, lat, col=year ) ) +
    geom_boxplot(outlier.shape=NA) + geom_jitter(alpha=0.3, size=.7, width=.2) +
    guides(color=FALSE) + xlab("Año") + ylab("Latitud")
boxplotLat

boxplotLong <- ggplot( filter( coords, lat > 19.375 ), aes( year, lon, col=year ) ) +
    geom_boxplot(outlier.shape=NA) + geom_jitter(alpha=0.3, size=.7, width=.2) +
    guides(color=FALSE) + xlab("Año") + ylab("Longitud")
boxplotLong

puntosDerrumbes <- ggmap(cdmx) +
    geom_point( aes(x=lon, y=lat, color=year), data=coords, size=.5, alpha=0.9) +
    xlim(-99.20, -99.10) + ylim(19.30, 19.47) +
    xlab("Longitud") + ylab("Latitud") +
    guides(color=guide_legend(title=element_text("Año") )) +
    theme(axis.text.x=element_blank())

save_plot( "boxplots.png", plot_grid( boxplotLat, boxplotLong ) )
save_plot( "derrumbesConcentracion.png", contourPlots )
save_plot( "derrumbes.png", puntosDerrumbes )
