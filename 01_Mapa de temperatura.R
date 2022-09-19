library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

Peru  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Per  <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Arequipa =  subset(Peru , NAME_1 == "Arequipa")

Arequipaa = st_read("SHP/Arequipa.geojson")  %>% st_as_sf()

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

lbl         <- data.frame(month_abb = month.abb, mes = 1:12)

# Extraemos los datos raster de Precipitacion -----------------------------------------------
Prec        <- getData("worldclim", var = "tmean", res=0.5, lon=-74.8773, lat=-11.54012)

Prec_Peru    <- crop(Prec, Arequipa)
Prec_Peru    <- Prec_Peru <- mask(Prec_Peru,Arequipa)
PPAnual_Peru <- do.call("sum", unstack(Prec_Peru))

PPAnual_Per = PPAnual_Peru/100
Geo_data       <-  rasterToPoints(PPAnual_Per)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "Tmean")
summary(Geo_data_frame$Tmean)
# Elaboramos los 

colores<- c("#2892C7", 
  "#6DA9B3", 
  "#A0C29B", 
  "#CEDE81", 
  "#FAFA64", 
  "#FCC44C", 
  "#FA8D34", 
  "#F25922", 
  "#E81115")


library(elevatr)
elev = get_elev_raster(Arequipa, z=10)
Poligo_alt    <- crop(elev, Arequipa)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Arequipaa )
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)


library(ggnewscale) 

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Per , fill="gray", color="black")+
  geom_sf(data = Arequipa, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")
SurA
SurA.grob  <- ggplotGrob(SurA)

summary(Geo_data_frame$Tmean)
Mapa=ggplot()+
  geom_sf(data = Peru, fill="white", color="black", size=0.4)+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame,aes(x,y, fill = Tmean), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30),
                       na.value = 'white',
                       labels = c("[0 -2] ","[2 - 4]", "[4 -6]", "[6 -8]", "[8 -10]", "[10 -12]",
                                  "[12 -14]","[14 - 16]","[16 - 18]","[18 - 20]","[20 - 22]","[22 - 24]"
                                  ,"[24 - 26]","[26 - 28]","[28 - 30]"),
                       name='Temperatura \nPromedio Anual ºC')+
  geom_sf(data = Arequipa, fill=NA, color="black", size=0.01)+
  coord_sf(xlim = c(-75.07264, -70.7), ylim = c(-17.4 ,-14.63263)) +
  theme_classic()+
  theme(legend.position = c(0.35, 0.17),
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#a9def9"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -72.5, y = -17, hjust = 0, vjust = 1, 
           label = "Oeano Pacifico",size = 3, family="serif", color = 
             "#03045e",  fontface="italic")+
  annotate(geom = "text", x = -71, y = -17, hjust = 0, vjust = 1,face="bold",
           label = "MOQUEGUA",size = 3, family="serif", color = 
             "black")+
  annotate(geom = "text", x = -70.8, y = -15.5, hjust = 0, vjust = 1,face="bold",
           label = "PUNO",size = 3, family="serif", color = 
             "black")+
  annotate(geom = "text", x = -71.7, y = -14.7, hjust = 0, vjust = 1,face="bold",
           label = "CUSCO",size = 3, family="serif", color = 
             "black")+
  annotate(geom = "text", x = -74, y = -15, hjust = 0, vjust = 1,face="bold",
           label = "AYACUCHO",size = 3, family="serif", color = 
             "black")+
  annotate(geom = "text", x = -75, y = -15, hjust = 0, vjust = 1,face="bold",
           label = "ICA",size = 3, family="serif", color = 
             "black")+
  annotation_custom(SurA.grob, xmin = -76, xmax = -73.5, ymin =-17.4, ymax=-16)+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)


ggsave(plot=Mapa ,"Mapa/Mapa de Temperatura2.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)

















