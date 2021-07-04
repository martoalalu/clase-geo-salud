library(tidyverse)
library(janitor)
library(sf)
install.packages("lwgeom")
install.packages("units")
library(units)
options(scipen=999)

caba <- read.csv("Downloads/casos_covid19.csv")

caba %>% 
  filter(clasificacion == "confirmado" & !is.na(barrio) & barrio != "") %>% 
  group_by(barrio, tipo_contagio) %>% 
  count() %>%
  pivot_wider(names_from = tipo_contagio, values_from = n) %>% 
  adorn_totals(where = "col") %>% 
  clean_names() %>% 
  write.csv("clase-geo-salud/data/covid_barrios_caba.csv")


prov <- readRDS("clase-geo-salud/data/gadm36_ARG_1_sf.RDS")
prov %>% 
  select(NAME_1, geometry) %>% 
  rename(provincia = NAME_1) %>% 
  st_simplify(dTolerance = 0.005) %>% 
  st_write("clase-geo-salud/data/ar_provincias.shp")

prov <- read_sf("clase-geo-salud/data/ar_provincias.shp")

head(prov)
st_crs(prov)

 
ggplot(prov) +
    geom_sf(aes(fill=provincia)) +
    theme_minimal()


st_area(prov) #nos devuelve en unidades raras, pasamos a km2
set_units(st_area(prov), km^2)

st_length(prov) / 1000

prov$area <- as.numeric(set_units(st_area(prov), km^2))
prov$perimetro <- as.numeric(st_length(prov) / 1000)


ggplot(prov) +
  geom_sf(aes(fill=area)) +
  theme_minimal()


ggplot(prov) + 
  geom_bar(aes(x= reorder(provincia, area), weight=area, fill=area)) +
  coord_flip() 

prov <- prov %>% 
  mutate(region =
  case_when(
        provincia %in% (c("Buenos Aires", "Córdoba", "La Pampa", "Entre Ríos", "Santa Fe", "Ciudad de Buenos Aires")) ~ "Pampeana",
        provincia %in% (c("Mendoza", "San Luis", "San Juan")) ~ "Cuyo",
        provincia %in% (c("La Rioja", "Catamarca", "Jujuy", "Salta", "Tucumán", "Santiago del Estero")) ~ "Noroeste",
        provincia %in% (c("Corrientes", "Misiones", "Formosa", "Chaco")) ~ "Noreste",
        TRUE ~ "Patagonia"
        )
  )

ggplot(prov) + 
  geom_bar(aes(x= reorder(provincia, area), weight=area, fill=region)) +
  coord_flip() 

ggplot(prov) + 
  geom_bar(aes(x= reorder(provincia, perimetro), weight=perimetro, fill=region)) +
  coord_flip() 


salud <- read.csv("https://github.com/martoalalu/clase-geo-salud/raw/master/data/refes-hospitales.csv", fileEncoding = 'UTF-8')
salud %>% 
  select(NOMBRE, ORIGEN_FINANCIAMIENTO,DOMICILIO, LATITUD, LONGITUD) %>% 
  write.csv("clase-geo-salud/data/refes-hospitales.csv")


caba <- readRDS("clase-geo-salud/data/gadm36_ARG_1_sf.RDS")
caba %>% 
  filter(NAME_1 == "Ciudad de Buenos Aires") %>%
  rename(provincia = NAME_1) %>% 
  st_write("clase-geo-salud/data/caba.shp")


caba %>% 
  filter(NAME_1 == "Ciudad de Buenos Aires") %>%