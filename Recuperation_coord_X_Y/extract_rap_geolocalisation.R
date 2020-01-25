library(tidyverse)
devtools::install_github(repo = 'rCarto/photon') # package développé pour faire le lien entre R et l'API Photon
library(photon)
library(sf) # package spatial
library(tmap) # package de visualisation spatiale dynamique

rap_picardie <- read_table("data/RAP.txt") # lire le tableau de données
rap_picardie_df <- rap_picardie %>%
  rename(variable = `Étude par Dépt. Commune Site Responsable Datation`) %>%
  separate(col = variable, c("Etude", "Departement", "Commune"), sep = " ", remove = FALSE) %>%
  # séparation des premiers éléments en 4 colonnes afin de récupérer le nom des communes et des département
  mutate(hdf = "Hauts-de-France, France", # création d'une colonne avec informations complémentaires spatiales
         lieux = str_c(Commune, Departement, hdf, sep = ", ")) # concaténation des colonnes liées aux noms des communes, dép, région et pays
rap_geoloca <- geocode(location = rap_picardie_df$lieux, key = "place", limit = 1) # récupération des coordonnées avec photon
rap_geoloca_sf <- st_as_sf(x = rap_geoloca, coords = c("lon", "lat")) %>% # construction d'un objet "sf"
  st_set_crs(4326) %>%
  st_transform(crs = 2154) # transformation du système de coordonnées en Lambert 93


# visualisation de la localisation des études caropologiques de manière dynamique
tmap_mode("view")
tm_shape(rap_geoloca_sf) +
  tm_bubbles(size = 0.4, col = "darkorange")

# export en .shp
st_write(rap_geoloca_sf, "data/geoloca_etudes_carpo_RAP25.shp")

# carte
sf_fond_europe <- read_sf("data/geom_eu34_nuts13.shp") %>%
  st_transform(crs = 2154)
france <- read_sf("data/REGION.shp") %>%
  st_transform(crs = 2154)
bbox_haut_de_france <- st_bbox(france %>% filter(INSEE_REG == "32"))

ggplot() + 
  geom_sf(data = sf_fond_europe, fill = "grey98", color = "grey98") +
  geom_sf(data = france, fill = "grey98", color = "grey40") +
  geom_sf(data = rap_geoloca_sf, color = "grey30", fill = "#e66101", shape = 21, size = 2) +
  coord_sf(xlim = bbox_haut_de_france[c(1,3)], ylim = bbox_haut_de_france[c(2,4)]) +
  ggthemes::theme_igray() +
  labs(subtitle = "Les études carpologiques réalisées en Picardie en 2005",
       caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : Malrain, Gaudefroy, Gransar 2005") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5)
