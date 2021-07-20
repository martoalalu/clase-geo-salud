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
  
  
  
  # Yapa II: Del punto al polígono
  
  Asi como transformamos polígonos en puntos, ahora vamos a hacer lo inverso.
Vamos a trazar un área de influencia de 200 metros alrededor de los centroides de las plaza. Esto se llama buffer, y es uno de los modos de convertir puntos en polígonos.

La función se llama **st_buffer** y acepta 2 parámetros principales: los datos de input y la distancia (dist)..

```{r}
st_buffer(plazas_c, dist = 200)
```

R nos está advirtiendo algo muy importante. Como nuestra unidad de medida son grados (recordemos que la latitud y longitud se mide en grados de inclinación con el Ecuador y el meridiano de Greenwich), entonces el área de influencia (buffer) la va a hacer en grados también.

O sea que nuestro buffer va a ser de 200º a la redonda de cada centroide de plaza!
  Veamos como quedaría el mapa.

```{r}
ggplot() +
  geom_sf(data = st_buffer(plazas_c, dist = 200))
```

No es lo que buscamos, nosotros queremos que el radio sea de 200 metros y para eso nuestra geometría tiene que estar expresadas en dicha unidad de medida.

Para eso vamos a pasar nuestro CRS a coordenadas planas, aquellas que expresen su ubicación en metros. Como estamos usando datos de Buenos Aires vamos a usar el [CRS = 5347](https://epsg.io/5347).

Dato: Si hubieramos usado datos de Argentina recomendamos usar el CRS = 5345.

```{r}
#Pasamos a un CRS con unidades en metros!
plazas_c_caba <- st_transform(plazas_c, 5347)

head(plazas_c_caba)
```

Ven como ahora cambió el campo de geometría. Las coordenadas no están más en grados (-34, 58).

Al mapa!
  ```{r}
ggplot() +
  geom_sf(data = st_buffer(plazas_c_caba, dist = 200))
```

Agreguemos la capa de barrios y algo de color.
```{r}
plazas_buffer <- st_buffer(plazas_c_caba, dist = 400)

ggplot() +
  geom_sf(data=radios) +
  geom_sf(data = plazas_buffer, fill = 'seagreen2', color = 'seagreen', alpha = 0.5)
```

¿Y si queremos quedarnos solo con los radios que seal alcanzados por el radio?
  st_intersection!
  
  ```{r}

# radios_caba <- st_transform(radios, 5347)

#Mapa

ggplot() +
  geom_sf(data=radios_caba[st_intersection(radios_caba,st_buffer(plazas_c_caba, 200), sparse = FALSE),]) +
  geom_sf(data=st_buffer(plazas_c_caba, 200)) + 
  theme_void()
```
