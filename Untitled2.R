library(tidyverse)
library(janitor)

caba <- read.csv("Downloads/casos_covid19.csv")
caba2 <- read.csv("clase-geo-salud/data/covid-caba.csv")
caba3 <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/reporte-covid/dataset_reporte_covid_sitio_gobierno.csv")

barrios <- caba %>% 
  filter(clasificacion == "confirmado" & !is.na(barrio) & barrio != "") %>% 
  group_by(barrio) %>% 
  count()

write.csv(barrios, "clase-geo-salud/data/covid_barrios_caba.csv")

unique(caba$clasificacion)


caba %>% 
  filter(clasificacion == "confirmado" & !is.na(barrio) & barrio != "") %>% 
  group_by(barrio, tipo_contagio) %>% 
  count() %>%
  pivot_wider(names_from = tipo_contagio, values_from = n) %>% 
  adorn_totals(where = "col") %>% 
  clean_names() %>% 
  write.csv("clase-geo-salud/data/covid_barrios_caba.csv")


readRDS("clase-geo-salud/data/gadm36_ARG_1_sf.RDS")
