library(tidyverse)
library(sf)
library(tmap)
library(SnowballC)
library(tm)
library(tidytext)
library(patchwork)

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


# tdf_quartiers <- text_table %>%
#   mutate(detectquartier = str_detect(string = revue_description, pattern = "quartier")) %>%
#   filter(detectquartier == TRUE) %>%
#   mutate(revue_description = stringi::stri_trans_general(str = revue_description, id = "Latin-ASCII")) %>%
#   tidytext::unnest_tokens(output = word, input = revue_description, drop = TRUE) %>%
#   anti_join(x = ., y = stop_words, by = "word") %>%
#   anti_join(get_stopwords(language = "fr")) %>%
#   count(id, word, sort = TRUE) %>%
#   ungroup() %>%
#   bind_tf_idf(word, id, n) %>%
#   arrange(desc(tf_idf))
# 
# tdf_quartiers %>%
#   left_join(x = ., y = final_sf %>% select(-description)) %>%
#   st_as_sf(.) %>%
#   st_transform(x = ., crs = 2154)
# 
# tdf_quartiers %>%
#   group_by(id) %>%
#   top_n(15) %>%
#   ungroup() %>%
#   ggplot(aes(word, tf_idf, fill = id)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "tf-idf") +
#   facet_wrap(~id, ncol = 2, scales = "free") +
#   coord_flip()










