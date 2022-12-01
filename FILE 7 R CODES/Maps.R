library(mapview)
library(ggmap)
library(zipcodeR)
library(rgeos)
library(sp)
library(maptools)
library(BelgiumMaps.StatBel)
library(leaflet)
library(readr)
library(mapview)
library(leafsync)
library(viridis)
library(manipulateWidget)
library(htmlwidgets)
library(psych)
library(StatMeasures)
library(tidyr)
library(broman)
library(base)
library(dplyr)
library(htmlwidgets)
library(lemon)
library(gridExtra)
library(broom)
library(rgdal)
library(ggpubr)
library(SUMMER)
library(geojsonio)
library(readr)
library(classInt)
library(openxlsx)
library(rgdal)
library(readr)
library(GISTools)  

# maps - download the shapefiles and don't forget to change the pathway

bimd2001 <- read.csv("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/BIMD2001_DOMAINS_STATISTICAL_SECTOR_ELLIS_WIDE.csv")
bimd2001 <- dplyr::select(bimd2001, c(1,22))
names(bimd2001)[2] <- "deciles"

becounty.shp2001 <- readOGR("C:/Users/otavova/OneDrive - UCL/Documents/R/win-library/4.0/SCBEL01Z5/SCBEL01Z5.shp") 
tx_ll <- spTransform(becounty.shp2001,  CRS("+proj=longlat +lat_0=90 +lon_0=4.36748666666667 +lat_1=49.8333339 +lat_2=51.1666673333333 +x_0=150000.01256 +y_0=5400088.4378 +ellps=intl +datum=NAD83 +units=m +no_defs"))

mymap2001 <- merge(tx_ll, bimd2001, by.x = "CS102001", by.y = "CD_RES_SECTOR", all = T)
mymap.df2001 <- merge(fortify(mymap2001), as.data.frame(mymap2001),by.x="id", by.y=0)
mymap.df2001$deciles <- as.factor(mymap.df2001$deciles)


map2001 <- ggplot() +
  geom_polygon(data = mymap.df2001, aes(x = long, y = lat, group = group, fill = deciles))  +
  scale_fill_manual( name = "Deciles", 
                     labels = c("1", "2","3","4","5","6","7","8","9","10"),
                     values = c( "#831818","#c62320","#f05b43","#f78462","#feac81",   
                                 "#f7dea3", "#ced1af","#98ab76","#748f46", "#47632a" )) +
  coord_quickmap() +
  theme_void()+
  theme(legend.key.size = unit(0.7, units = "cm")) + labs(title = "\nBIMD2001\n") +
  theme(legend.position = "bottom", legend.justification = "center",
        legend.text = element_text(size = 8))+
  guides(fill=guide_legend(title ="Deciles \n" , label.position = "bottom", nrow = 1))+
  theme(plot.margin = unit(c(0,0,0,0), "lines"))

# plot 2011 

bimd2011 <-read.csv("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%202%20BIMD2011%20DOMAINS%20(SCORES%2C%20RANKS%2C%20DECILES)/BIMD2011_DOMAINS_STATISTICAL_SECTOR_ELLIS_WIDE.csv")

bimd2011 <- dplyr::select(bimd2011, c(1,22))
names(bimd2011)[2] <- "deciles"

becounty.shp2011 <- readOGR("C:/Users/otavova/OneDrive - UCL/Statistical sectors/SHAPE_FILES_2011/Shapefile_shp/Shapefile_2011.shp") 
tx_ll <- spTransform(becounty.shp2011,  CRS("+proj=longlat +lat_0=90 +lon_0=4.36748666666667 +lat_1=49.8333339 +lat_2=51.1666673333333 +x_0=150000.01256 +y_0=5400088.4378 +ellps=intl +datum=NAD83 +units=m +no_defs"))

mymap2011 <- merge(tx_ll, bimd2011, by.x = "CD_SECTOR", by.y = "CD_RES_SECTOR", all = T)

mymap.df2011 <- merge(fortify(mymap2011), as.data.frame(mymap2011),by.x="id", by.y=0)
mymap.df2011$deciles <- as.factor(mymap.df2011$deciles)


map2011<- ggplot() +
  geom_polygon(data = mymap.df2011, aes(x = long, y = lat, group = group, fill = deciles))  +
  scale_fill_manual( name = "Deciles", 
                     labels = c("1", "2","3","4","5","6","7","8","9","10"),
                     values = c( "#831818","#c62320","#f05b43","#f78462","#feac81",   
                                 "#f7dea3", "#ced1af","#98ab76","#748f46", "#47632a" )) +
  coord_quickmap() +
  theme_void()+
  theme(legend.key.size = unit(0.7, units = "cm")) + labs(title = "\nBIMD2011\n") +
  theme(legend.position = "bottom", legend.justification = "center",
        legend.text = element_text(size = 8))+
  guides(fill=guide_legend(title ="Deciles \n" , label.position = "bottom", nrow = 1))+
  theme(plot.margin = unit(c(0,0,0,0), "lines"))


mylegend<-g_legend(map2001)

z <- ggarrange( map2001,
                map2011, nrow=1,ncol=2, common.legend = TRUE, legend="bottom", legend.grob = mylegend)


tiff("file.tiff", , res = 300,
     compression = "lzw", width = 25, height = 15, units = "cm", pointsize = 25)

z
dev.off()
