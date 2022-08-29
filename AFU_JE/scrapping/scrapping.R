library(tidyverse)
library(rvest)
library(stringr)
library(glue)
library(httr) # for timeout option response

##### Ariadne portal: inrap reports #####
# urlbase <- 'https://portal.ariadne-infrastructure.eu/api/search?q=&page={thispage}&range={date_requete}&sort=issued&order=desc'
urlbase <- "https://portal.ariadne-infrastructure.eu/api/search?q=&page={thispage}&sort=_score&order=asc&publisher=Institut national de recherches archéologiques préventives (Inrap)"

# l'api bug à partir de la page 1001
# debut <- seq(-10000, 1950, 50) %>% as_tibble()
# fin <- seq(-9951, 2001, 50) %>% as_tibble()
# date_choisie <- tibble(value = as.character())
# 
# for (i in 1:nrow(debut)) {
#   requete <- paste0(debut$value[i], "%2C", fin$value[i]) %>% as_tibble()
#   date_choisie <- date_choisie %>%
#     bind_rows(requete)
# }
# 
# date_choisie <- date_choisie$value

liste_de_results <- list()

# for (i in 1) {
#   
#   date_requete <- date_choisie[i]
  
  for (thispage in 1:3762){
    thisurl <- glue(urlbase) %>% as.character() %>% URLencode()
    print(thisurl)
    re_test <- RETRY(
      "GET",
      url = thisurl,
      timeout(30),
      times = 5
    )
    results <- jsonlite::fromJSON(content(re_test, as = "text"), 
                                  flatten = FALSE, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    liste_de_results[[thispage]] <- results
  }
#   
# }

# compilation of lists
liste_de_results_order_desc <- liste_de_results
liste_de_results_order_asc <- liste_de_results
liste_de_results_order_relevance <- liste_de_results

final_list <- c(liste_de_results_order_desc, liste_de_results_order_asc, liste_de_results_order_relevance)

# exportation des data en JSON
write(x = jsonlite::toJSON(final_list), file = "export_data_scrapping_ariadne_inrap.JSON")

rm(liste_de_results, liste_de_results_order_asc, liste_de_results_order_desc, liste_de_results_order_relevance)

# extracting ID to know if there is duplicate
# if yes, then selecting in final list, unique elements
extract_ids <- tibble(value = as.character())

for (i in 1:length(final_list)) {
  rapport_results <- final_list[[i]]$hits
  intermediaire <- tibble(value = as.character())
  
  for (j in 1:length(rapport_results)) {
    identifiants <- rapport_results[[j]]$id %>%
      as_tibble()
    intermediaire <- intermediaire %>%
      bind_rows(identifiants)
  }
  
  extract_ids <- extract_ids %>%
    bind_rows(intermediaire)
}

# is duplicated?
duplicated_id <- extract_ids %>%
  rowid_to_column() %>%
  mutate(is_duplicated = duplicated(x = value))

duplicated_id %>%
  filter(is_duplicated == TRUE) # yes it is

# selecting unique element in final list
final_list_unique <- map_df(.x = final_list, .f = "hits")

final_list_unique <- final_list_unique %>%
  left_join(x = ., y = duplicated_id, by = c("id"="value")) %>%
  filter(is_duplicated == FALSE)

saveRDS(final_list_unique, file = "final_list.RDS")

##### ISIDORE #####
urlbase <- "https://api.isidore.science/resource/search?q=title%3Aquartier&title%3Aquartier&scope=http%3A%2F%2Fisidore.science%2Fsubject%2Fpublications&page={thispage}&output=json&replies=100&discipline=http://aurehal.archives-ouvertes.fr/subject/shs.archeo"

liste_de_results <- list()

for (thispage in 1:8){
  thisurl <- glue(urlbase) %>% as.character() %>% URLencode()
  print(thisurl)
  re_test <- RETRY(
    "GET",
    url = thisurl,
    timeout(30),
    times = 5
  )
  results <- jsonlite::fromJSON(content(re_test, as = "text"), 
                                flatten = FALSE, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  liste_de_results[[thispage]] <- results
}

# exportation des data en JSON
write(x = jsonlite::toJSON(final_list), file = "export_data_scrapping_ariadne_inrap.JSON")





