library(tidyverse)
library(sf)
library(tmap)
library(SnowballC)
library(tm)
library(tidytext)
library(patchwork)
library(readxl)

tmap_mode(mode = "view")

# import dataset
data_inrap <- readRDS(file = "final_list.RDS")

name_liste <- data_inrap$data
name_liste <- names(name_liste)

data_inrap_wide <- data_inrap %>%
  mutate(name_liste = name_liste) %>%
  pivot_wider(data = ., id_cols = id, names_from = name_liste, values_from = data, values_fn = list)


#### extract lat/long
lat_long <- data_inrap_wide$spatial

lat_long_liste <- list()

for (i in 1:length(lat_long)) {
  id <- data_inrap_wide$id[[i]]
  names(id) <- "id"
  lat_long_liste[[i]] <- c(lat_long[[i]], id)
}


t_lat_long <- tibble(id = as.character(),
                   lat = as.numeric(),
                   lon = as.numeric())

# pb avec:
lat_long_liste[[10232]]
lat_long_liste[[17004]]

for (i in 1:(length(lat_long_liste)-2)) {
  lat_long_liste2 <- lat_long_liste[c(-10232,-17004)]

  # extracting elements
  spatial_information <- getElement(object = lat_long_liste2[[i]], name = "spatial") # same as: lat_long_liste2[[i]]$spatial
  extracting_lat_long <- map_df(.x = spatial_information, .f = "centroid")
  print(i)
  extract_id <- tibble(id = getElement(object = lat_long_liste2[[i]], name = "id"))

  # creation of tibble
  t_lat_long <- t_lat_long %>%
    bind_rows(
      bind_cols(extract_id, extracting_lat_long)
    )
}

t_lat_long <- t_lat_long %>%
  st_as_sf(coords = c("lon", "lat"))

tm_shape(shp = t_lat_long) + tm_dots()

#### extract description
description <- data_inrap_wide$description

description_liste <- list()

for (i in 1:length(lat_long)) {
  id <- data_inrap_wide$id[[i]]
  names(id) <- "id"
  description_liste[[i]] <- c(description[[i]], id)
}

t_description <- tibble(id = as.character(),
                     description = as.character())

# pb avec pas mal d'éléments...
description_liste <- description_liste[lengths(description_liste) > 1]

for (i in 1:length(description_liste)) {
   # extracting elements
  desc_information <- getElement(object = description_liste[[i]], name = "description")
  desc_information <- tibble(description = desc_information[[1]])
  print(i)
  extract_id <- tibble(id = getElement(object = description_liste[[i]], name = "id"))
  
  # creation of tibble
  t_description <- t_description %>%
    bind_rows(
      bind_cols(extract_id, desc_information)
    )
}

#### spatial and temporal analyses ####
#### sf final
final_sf <- t_lat_long %>%
  left_join(., y = t_description, by = "id") %>%
  st_set_crs(x = ., value = 4326)

write_sf(final_sf, "final_sf.gpkg")

europ <- st_read(dsn = "Europe.shp")

final_sf_fr <- final_sf %>%
  mutate(a_quartier = str_detect(string = description, pattern = "quartier")) %>%
  st_transform(x = ., crs = 2154) %>%
  st_join(x = ., y = europ %>% st_transform(x = ., crs = 2154), join = st_within)

ggplot() +
  geom_sf(data = europ %>% filter(NAME == "France"), color = "grey70", fill = "grey80", size = 0.2) +
  geom_sf(data = final_sf_fr %>% 
            filter(NAME == "France"),
          color = "grey20", size = 0.3, alpha = 0.5) +
  ggthemes::theme_map() +
  ggtitle(label = 'Contient le mot "quartier" dans le résumé du rapport INRAP') +
  facet_wrap(~a_quartier) +
  labs(caption = "J. Gravier 2022\ndata: INRAP, Ariadne portal\n@_AFUrbain", 
       subtitle = "sélection des opérations en France métropolitaine, n = 21,824")
  
ggsave(filename = "sorties/carto.png", plot = last_plot(), device = "png", width = 20, height = 12, units = "cm")

#### text analyses ####
text_table <- final_sf %>%
  mutate(geom = geometry) %>%
  st_drop_geometry() %>%
  mutate(revue_description = str_replace_all(string = description, pattern = "\n", replacement = " ")) %>% 
  mutate(revue_description = str_to_lower(string = revue_description)) %>%
  mutate(revue_description = gsub('[[:punct:]]', " ", revue_description)) %>%
  mutate(revue_description = gsub('[[:digit:]]+', ' ', revue_description))

# text_table$revue_deux <- wordStem(text_table$revue_description, language = "french")
text_table_token <- text_table %>%
  tidytext::unnest_tokens(output = word, input = revue_description, drop = TRUE) %>%
  anti_join(x = ., y = stop_words, by = "word") %>%
  anti_join(get_stopwords(language = "fr"))

# vue générake des mots les plus fréquemment utilisés dans le corpus
somme <- nrow(text_table_token)

a <- text_table_token %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n/sum(n)*100) %>%
  filter(n > 5000) %>%
  ggplot(mapping = aes(x = freq, y = reorder(word, n))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "fréquence", limits = c(0,1)) +
  theme(axis.title.y = element_blank()) +
  labs(subtitle = paste("résumés des rapports, n mots =", somme))

# pour les quartiers ?
text_table_token_q <- text_table %>%
  mutate(detectquartier = str_detect(string = revue_description, pattern = "quartier")) %>%
  filter(detectquartier == TRUE) %>%
  tidytext::unnest_tokens(output = word, input = revue_description, drop = TRUE) %>%
  anti_join(x = ., y = stop_words, by = "word") %>%
  anti_join(get_stopwords(language = "fr"))


somme_q <- nrow(text_table_token_q)
b <- text_table_token_q %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n/sum(n)*100) %>%
  rowid_to_column() %>%
  filter(rowid <= 40) %>%
  ggplot(mapping = aes(x = freq, y = reorder(word, n))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "fréquence", limits = c(0,1)) +
  theme(axis.title.y = element_blank()) +
  labs(subtitle = paste('résumés incluant "quartier", n mots =', somme_q))

# comparaison des deux corpus : général ou ayant "quartier"
a + b + plot_annotation(title = "Les 40 mots les plus fréquemment utilisés",
                        subtitle = "après suppression des stopwords et sans lemmatisation",
                        caption = "J. Gravier 2022\ndata: INRAP, Ariadne portal\n@_AFUrbain")

ggsave(filename = "sorties/comparaison.png", plot = last_plot(), device = "png", width = 25, height = 18, units = "cm")


# pour les résumés incluant "quartier", quels ngrams?
ngrams_2_quartier <- text_table %>%
  mutate(detectquartier = str_detect(string = revue_description, pattern = "quartier")) %>%
  filter(detectquartier == TRUE) %>%
  mutate(revue_description = stringi::stri_trans_general(str = revue_description, id = "Latin-ASCII")) %>%
  tidytext::unnest_tokens(output = bigrams, input = revue_description, token = "ngrams", n = 2, drop = TRUE) %>%
  separate(data = ., col = bigrams, into = c("word", "mot 2")) %>% 
  anti_join(x = ., y = stop_words, by = "word") %>%
  anti_join(get_stopwords(language = "fr")) %>%
  rename(mot = word, word = `mot 2`) %>%
  anti_join(x = ., y = stop_words, by = "word") %>%
  anti_join(get_stopwords(language = "fr")) %>%
  mutate(bigrams = paste0(mot, " ", word)) %>% 
  count(bigrams, sort = TRUE)  
  


ngrams_2_quartier %>% 
  mutate(freq = n/sum(n)*100) %>%
  rowid_to_column() %>%
  filter(rowid <= 60) %>%
  ggplot(mapping = aes(x = freq, y = reorder(bigrams, n))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "fréquence", limits = c(0,0.5)) +
  theme(axis.title.y = element_blank()) +
  labs(subtitle = 'ngrams = 2 dans les résumés incluant "quartier"')


ngrams_2_quartier %>%
  mutate(detectquartier = str_detect(string = bigrams, pattern = "quartier")) %>%
  filter(detectquartier == TRUE) %>%
  mutate(freq = n/sum(n)*100) %>%
  rowid_to_column() %>%
  filter(freq > 0.25) %>%
  ggplot(mapping = aes(x = freq, y = reorder(bigrams, n))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "fréquence") +
  theme(axis.title.y = element_blank()) +
  labs(subtitle = 'ngrams = 2, incluant le mot "quartier"',
       caption = "J. Gravier 2022\ndata: INRAP, Ariadne portal\n@_AFUrbain")

ggsave(filename = "sorties/bigram.png", plot = last_plot(), device = "png", width = 18, height = 22, units = "cm")


#### land cover and bigrams ####
bigram_spatial <- text_table %>%
  mutate(detectquartier = str_detect(string = revue_description, pattern = "quartier")) %>%
  filter(detectquartier == TRUE) %>%
  mutate(revue_description = stringi::stri_trans_general(str = revue_description, id = "Latin-ASCII")) %>%
  tidytext::unnest_tokens(output = bigrams, input = revue_description, token = "ngrams", n = 2, drop = TRUE) %>%
  separate(data = ., col = bigrams, into = c("word", "mot 2")) %>% 
  anti_join(x = ., y = stop_words, by = "word") %>%
  anti_join(get_stopwords(language = "fr")) %>%
  rename(mot = word, word = `mot 2`) %>%
  anti_join(x = ., y = stop_words, by = "word") %>%
  anti_join(get_stopwords(language = "fr")) %>%
  mutate(bigrams = paste0(mot, " ", word)) %>%
  mutate(detectquartier = str_detect(string = bigrams, pattern = "quartier")) %>%
  filter(detectquartier == TRUE)


bigram_spatial <- bigram_spatial %>%
  left_join(x = ., y = data_inrap_wide %>%
              select(id, issued) %>%
              mutate(issued = as.character(issued)) %>%
              mutate(issued = qdapRegex::ex_between(issued, 'list(issued = \"', '\"'),
                     issued = as.numeric(issued)), 
            by = "id")


# CLC data
CLC_2006 <- st_read(dsn = "CLCover/CLC06_FR_RGF.shp")
CLC_2018 <- st_read(dsn = "CLCover/CLC18_FR.shp")
referentiel_CLC_niv1 <- read_excel(path = "CLCover/clc-nomenclature-c_1.xls", sheet = "nomenclature_clc_niveau_1")
referentiel_CLC_niv2 <- read_excel(path = "CLCover/clc-nomenclature-c_1.xls", sheet = "nomenclature_clc_niveau_2")
referentiel_CLC_niv3 <- read_excel(path = "CLCover/clc-nomenclature-c_1.xls", sheet = "nomenclature_clc_niveau_3")

CLC_2006 <- CLC_2006 %>%
  mutate(niv1 = str_sub(CODE_06, 1, 1)) %>%
  mutate(niv2 = str_sub(CODE_06, 1, 2)) %>%
  left_join(y = referentiel_CLC_niv1 %>%
              select(-libelle_en:-bleu) %>%
              rename(libelle_niv1 = libelle_fr), by = c("niv1" = "code_clc_niveau_1")) %>%
  left_join(y = referentiel_CLC_niv2 %>%
              select(-libelle_en) %>%
              rename(libelle_niv2 = libelle_fr), by = c("niv2" = "code_clc_niveau_2")) %>%
  left_join(y = referentiel_CLC_niv3 %>%
              select(-libelle_en) %>%
              rename(libelle_niv3 = libelle_fr), by = c("CODE_06" = "code_clc_niveau_3")) %>%
  mutate(libelle_reconstitue = case_when(
    CODE_06 %in% c('111', '112') ~ libelle_niv3,
    niv2 %in% "12" ~ libelle_niv2,
    TRUE ~ libelle_niv1
  ))

CLC_2018 <- CLC_2018 %>%
  mutate(niv1 = str_sub(CODE_18, 1, 1)) %>%
  mutate(niv2 = str_sub(CODE_18, 1, 2)) %>%
  left_join(y = referentiel_CLC_niv1 %>%
              select(-libelle_en:-bleu) %>%
              rename(libelle_niv1 = libelle_fr), by = c("niv1" = "code_clc_niveau_1")) %>%
  left_join(y = referentiel_CLC_niv2 %>%
              select(-libelle_en) %>%
              rename(libelle_niv2 = libelle_fr), by = c("niv2" = "code_clc_niveau_2")) %>%
  left_join(y = referentiel_CLC_niv3 %>%
              select(-libelle_en) %>%
              rename(libelle_niv3 = libelle_fr), by = c("CODE_18" = "code_clc_niveau_3")) %>%
  mutate(libelle_reconstitue = case_when(
    CODE_18 %in% c('111', '112') ~ libelle_niv3,
    niv2 %in% "12" ~ libelle_niv2,
    TRUE ~ libelle_niv1
  ))


# construction du dataset: within opération (bigram) in CLC depending on dates
# le plus simple: 2 datasets, then filter on date
bigram_spatial_2006 <- bigram_spatial %>%
  mutate(issued2 = if_else(issued > 2000 & issued <= 2013, 2006, 2018)) %>%
  filter(issued2 == 2006) %>%
  st_as_sf() %>%
  st_transform(x = ., crs = 2154) %>%
  st_join(x = ., y = CLC_2006, join = st_within)

bigram_spatial_2018 <- bigram_spatial %>%
  mutate(issued2 = if_else(issued > 2000 & issued <= 2013, 2006, 2018)) %>%
  filter(issued2 == 2018) %>%
  st_as_sf() %>%
  st_transform(x = ., crs = 2154) %>%
  st_join(x = ., y = CLC_2018, join = st_within)

bigram_spatial_CLC <- bind_rows(bigram_spatial_2006 %>% select(id, description, bigrams, libelle_reconstitue, issued),
                                bigram_spatial_2018 %>% select(id, description, bigrams, libelle_reconstitue, issued))

bigram_spatial_CLC <- bigram_spatial_CLC %>%
  filter(!is.na(libelle_reconstitue))

bigram_spatial_CLC_grp <- bigram_spatial_CLC %>%
  st_drop_geometry() %>%
  group_by(bigrams, libelle_reconstitue) %>%
  count() %>%
  ungroup()

a <- bigram_spatial_CLC_grp %>%
  mutate(libelle_reconstitue = if_else(libelle_reconstitue == "Zones industrielles ou commerciales et réseaux de communication",
                                       "Zones industrielles ou commerciales", false = libelle_reconstitue)) %>%
  filter(n > 1) %>%
  ggplot(mapping = aes(x = libelle_reconstitue, y = bigrams, size = n, color = n)) +
  geom_point() +
  theme_bw() +
  theme(axis.title = element_blank(), axis.text.x = element_text(angle = 90)) +
  scale_color_viridis_c() +
  labs(title = "Bigrams et types d'espace")


b <- bigram_spatial_CLC_grp %>%
  mutate(libelle_reconstitue = if_else(libelle_reconstitue == "Zones industrielles ou commerciales et réseaux de communication",
                                       "Zones industrielles ou commerciales", false = libelle_reconstitue)) %>%
  filter(n > 1) %>%
  filter(bigrams != "quartier saint") %>%
  ggplot(mapping = aes(x = libelle_reconstitue, y = bigrams, size = n, color = n)) +
  geom_point() +
  theme_bw() +
  theme(axis.title = element_blank(), axis.text.x = element_text(angle = 90)) +
  scale_color_viridis_c() +
  labs(caption = "J. Gravier 2022\ndata: INRAP, Ariadne portal, CORINE Land Cover\n@_AFUrbain", 
       title = 'sans "quartier saint"')

a + b + plot_annotation(title = "Occupation du sol actuelle et vocabulaire employé dans les résumés")

ggsave(filename = "sorties/bigram-espace.png", plot = last_plot(), device = "png", width = 35, height = 22, units = "cm")




