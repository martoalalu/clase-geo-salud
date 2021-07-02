library(tidyverse)
library(sf)


#Con esto ves todas las capas disponibles en el servicio
st_layers("WFS:https://geoservicios.indec.gob.ar/geoserver/wfs?getcapabilities")

#st_layers("https://wms.ign.gob.ar/geoserver/ows?service=wfs&version=1.1.0&request=GetCapabilities")

baseurl <- "https://geoservicios.indec.gob.ar/geoserver/wfs?"

#Aca puse un ejemplo para armar el request de la capa de actividad_pcia
wfs_request <- "request=GetFeature&service=WFS&typeName=sig:v_departamentos"

#Lees la capa
provincias <- st_read(paste0(baseurl,wfs_request))

getwd()
deptos <- st_read("clase-geo-salud/data/pxdptodatosok.shp")
prov <- st_read("clase-geo-salud/pxpciadatosok.shp")
prov2 <- st_read("clase-geo-salud/arg_admbnda_adm1_unhcr2017.shp")

arg_admbnda_adm0_unhcr2017.shx

prov2 %>% 
st_simplify(dTolerance = 0.005) %>% 
  ggplot() + 
  geom_sf()

pxpciadatosok.dbf
View(head(a))

a <- st_cast(prov, "POLYGON")
a %>% 
  st_simplify(dTolerance = 0.005) %>% 
  filter(link != "94") %>% 
ggplot() + 
  geom_sf(aes(fill= link))


prov <- deptos %>% st_simplify(dTolerance = 0.005) %>% 
  filter(!departamen %in% c("Antártida Argentina", "Islas del Atlántico Sur")) %>%
  group_by(provincia) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ggplot() + 
  geom_sf(aes(fill = provincia))





pais_dosis1 <- shape %>% st_simplify(dTolerance = 0.005) %>% 
  filter(! departamen %in% c("Antártida Argentina", "Islas del Atlántico Sur")) %>% 
  ggplot() + geom_sf(aes(fill=pob_dosis_1), size=0.05) +
  colorspace::scale_fill_continuous_sequential(palette="Greens", limits=c(0,100)) +
  coord_sf(datum=NA) + theme_minimal() +
  labs(title=paste0("Porcentaje de la población \n con al menos una dosis, ", fecha_latina), 
       fill="%")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "bottom", 
        legend.key.size = unit(0.6, 'cm'), #change legend key size
        legend.key.height = unit(0.6, 'cm'), #change legend key height
        legend.key.width = unit(0.6, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=8),
        legend.margin=margin(-15), 
        plot.title=element_text(hjust=0.5, vjust=-3, face="bold", size=16)) +
  inset_element(amba_dosis1,  left =  0.5, bottom =  0.05, right = 0.8, top = 0.4)



options(scipen=999)

ggplot() +
  geom_sf(data = provincias) +
  ylim(c(4000000,NA))