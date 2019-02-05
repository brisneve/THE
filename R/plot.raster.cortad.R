plot.raster.cortad <- function(r, ext){
  if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
  if(!require(ggthemes)){install.packages("ggthemes"); library(ggthemes)}
  if(!require(rasterVis)){install.packages("rasterVis"); library(rasterVis)}
  WorldData <- map_data('world')
  myPalette <- colorRampPalette(c("white", "sky blue", "light green", "yellow", "orange", "red"))
  gplot(r) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradientn(expression(degree~C),
                         colours = myPalette(6),
                         na.value = "white")+
    geom_map(data=WorldData, map=WorldData,
             aes(x=long, y=lat, group=group, map_id=region),
             fill="white", colour="white", size=0.5)+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_cartesian(xlim = c(ext[1], ext[2]), ylim = c(ext[3], ext[4]))+
    theme_few()+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    theme(legend.background = element_blank())
}
