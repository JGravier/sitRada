

data_isidore <- jsonlite::read_json(path = "export_ISIDORE_publication_archeo_quartier_title.JSON", simplifyVector = FALSE)

# elements to search
data_isidore[[2]]$response$replies$content$reply[[1]]$isidore$title$`$`
data_isidore[[1]]$response$replies$content$reply[[1]]$isidore$title[[1]][[1]]
data_isidore[[1]]$response$replies$content$reply[[1]]$isidore$date$`@origin`
data_isidore[[1]]$response$replies$content$reply[[8]]$isidore$abstract[[2]]$`$`[[1]]
data_isidore[[1]]$response$replies$content$reply[[1]]$isidore$type$type[[2]][[1]]

extract_title_date_type <- tibble(id = character(), 
                                  titre = list(),
                                  datepubli = character())

for (i in 1:length(data_isidore)) {
  
  reponse <- data_isidore[[i]]$response$replies$content$reply
  print(i)
  
  for (j in 1:length(reponse)) {
    
    t_isidore <- tibble(id = paste0(i, "_", j),
                        titre = reponse[[j]]$isidore$title,
                        datepubli = reponse[[j]]$isidore$date$`@origin`[[1]])
    
    
    extract_title_date_type <- extract_title_date_type %>%
      bind_rows(t_isidore)
    
  }
  
}

chara1 <- extract_title_date_type %>%
  separate(col = id, into = c("i", "j"), sep = "_", remove = FALSE) %>%
  mutate(est_ok = !duplicated(x = id, fromLast = TRUE),
         est_ok2 = !duplicated(x = id, fromLast = FALSE)) %>%
  filter(est_ok == TRUE & est_ok2 == TRUE) %>%
  mutate(titre = as.character(titre))
  
extractliste <- extract_title_date_type %>%
  separate(col = id, into = c("i", "j"), sep = "_", remove = FALSE) %>%
  mutate(est_ok = !duplicated(x = id, fromLast = TRUE),
         est_ok2 = !duplicated(x = id, fromLast = FALSE)) %>%
  filter(est_ok == FALSE | est_ok2 == FALSE) %>%
  group_by(id) %>%
  mutate(mon_sep = row_number()) %>%
  filter(mon_sep == 2) %>%
  ungroup()

chara2 <- extractliste %>%
  filter(lengths(titre) == 1) %>%
  mutate(titre = unlist(titre))


chara3 <- extractliste %>%
  filter(lengths(titre) == 2) %>%
  mutate(titre = as.character(titre)) %>%
  mutate(titre = qdapRegex::ex_between(titre, '`$` = list(\"', '\"))')) %>%
  mutate(titre = as.character(titre))

extract_title_date_type <- bind_rows(chara1, chara2, chara3)
rm(chara1, chara2, chara3, extractliste)

extract_title_date_type <- extract_title_date_type %>%
  select(titre, datepubli) %>%
  mutate(datepubli = str_sub(string = datepubli, start = 1, end = 4))

extract_title_date_type %>%
  mutate(datepubli = as.numeric(datepubli)) %>%
  filter(datepubli > 10) %>%
  ggplot(mapping = aes(x = datepubli)) +
  geom_bar(stat = "count")




