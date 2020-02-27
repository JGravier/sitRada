library(tidyverse)
library(pdftools)

fichiers <- list.files(pattern = "pdf$")

# création d'un objet type liste comprenant des chaines de caractères, sachant que :
# chaque pdf correspond à une liste
# chaque page d'un pdf correspond à une chaîne de caractère
fichiers <- lapply(fichiers, pdf_text)

# on veut découper les chaînes de caractères en autant de "lignes" qu'il y en a dans les pdf au départ :
data <- lapply(fichiers, function(x) strsplit(x, split = "\r\n")) %>% 
  unlist() %>%
  as.data.frame()

colnames(data) <- c("lignes")

data <- as_tibble(data)

# on récupère les patterns récurrents à tous les pdf et utiles dans l'analyse plus tard
# solution transformée à partir de : https://community.rstudio.com/t/multiple-str-detect-or-loop/25413/3
mes_pattern_a_garder <- c("AHRS Number:", "Site Name:", "Associations:", "Informal Associations:",
                         "Location Info:", "Location Reliability:", "Mapsheet(s):", "Point Representation:",
                         "Geometry Accuracy:", "AHRS Resource Nature:", "Resnat Subtype:", "Resource Keywords:",
                         "Acreage:", "Period(s):", "Associated Dates:", "Cultural Affiliation:", "Current Function(s):",
                         "Historic Function(s):", "Condition Code:", "Danger(s) of Destruction:", "Destruct Year:",
                         "Property Owner:", "Source Reliability:", "Assigned To:", "Date Issued:", "Card Status:",
                         "Attachments:", "Distinctive Features:", "Period of Significance:", "Area Significance(s):",
                         "Criteria:", "Considerations:", "Filed As:", "Comments:")

# fonction : détecter les patterns à garder
detect_a_garder <- function(x, texte){
  !str_detect(string = texte, fixed(x, ignore_case = TRUE))
}

# Sélection des lignes récurrentes sur lesquelles on pourra travailler
data <- data %>%
  # test de contenu de pattern ou non par ligne
  # résultat du test inclu dans une nouvelle colonne dénommée "a_virer"
  mutate(a_virer = mes_pattern_a_garder %>% 
           map( ~ detect_a_garder(.x, texte = lignes)) %>% 
           pmap_lgl(all)) %>%
  filter(!a_virer) %>% # virer les TRUE
  select(-a_virer) # colonne inutile par la suite que l'on peut donc supprimer


# Découpe de la colonne principale en 2 colonnes pour dissocier les noms de (futures) variables du contenu de l'information
data <- str_split_fixed(string = data$lignes, pattern = ":", n = 2) %>% # découpe la colonne en 2 à partir du moment où l'on a un ":"
  as.data.frame() %>% # trasformation en tableau
  as_tibble() %>% # transformation en tiblle
  mutate(titre = as.character(V1), # création d'une colonne contenant les futurs "titres"/en tête de tableau, type character et non factor
         infos = as.character(V2)) %>% # idem pour les infos
  select(-V1, -V2) # on supprime les colonnes V1 et V2 inutiles (doublons de "titre" et "infos")



# création d'une nouvelle colonne pour regrouper les lignes par site archéologique, correspondant à 1:n potentiels tableaux
data <- data %>%
  mutate(repetition = rep(
    x = seq(1, length(x = data$infos)/35), each = 35) # pour toutes les lignes, de 1 à n,
    # création d'une colonne dénommée "repetition", contenant un entier répété toutes les 35 lignes (correspondant au nbre de pattern gardés)
    ) %>% 
  group_by(repetition)

# dissocier les lignes du tableau à partir du regroupement (ici "repetition") en n listes
data <- group_split(data)


# fonction pour transposer les informations des tableaux contenus dans les n listes
tranfo_tableaux <- function(x){
  for (i in 1:length(x)) {
    x[[i]] <- as.data.frame(t(x[[i]]))
  }
  return(x)
}

# application de la fonction ci-dessus
data <- tranfo_tableaux(data)


# création d'un tableau (data.frame) contenant les éléments des n listes
data <- purrr::map(data, ~bind_rows(.x)) %>% bind_rows()

# Enlever les espaces présents avant et après toutes les chaînes de character contenues dans les 35 variables :
data <- data %>%
  as_tibble() %>% # fait un tiblle
  rowid_to_column() %>% # ajout d'une colonne qui nous servira d'identifiant pour pivoter le tableau
  pivot_longer(cols = V1:V35, names_to = "nom", values_to = "valeurs") %>% # format long de tableau
  mutate(valeurs = str_trim(string = valeurs, side = c("both"))) %>% # supprime les espace avant et après : both
  pivot_wider(id_cols = rowid, names_from = "nom", values_from = "valeurs") %>% # format wide de tableau
  select(-rowid) # on supprime les identifiants qui sont ici inutiles

# nommer les colonnes selon les informations de la première ligne
colnames(data) <- data[1,]


# parmi toutes ces informations, seules certaines nous intéressent :
# virer toutes les lignes relatives aux noms de colonnes
data <- data %>%
  mutate(filtre_1 = str_detect(`AHRS Number`, "AHRS Number")) %>% # détection de contenant
  filter(filtre_1 != TRUE) %>% # suppression des lignes contenant l'expression "AHRS Number"
  select(-filtre_1) # on supprime la colonne de détection qui n'a pas d'utilité pour la suite

# sélection enfin d'une ligne sur deux de notra tableau :
data <- data[seq(from = 1, to = nrow(data), by = 2),]


# récupération des Lat, Long dans deux colonnes différentes pour simplifier la manipulation spatiale ensuite :
extract_lat_long <- str_split_fixed(string = data$`Point Representation`, pattern = ",", n = 2) %>% as.data.frame()
extract_lat <- str_split_fixed(string = extract_lat_long$V1, pattern = ":", n = 2) %>% as.data.frame()
extract_long <- str_split_fixed(string = extract_lat_long$V2, pattern = ":", n = 2) %>% as.data.frame()

lat_long <- data.frame(lat = extract_lat$V2,
                       long = extract_long$V2)

data <- bind_cols(data, lat_long) %>%
  select(-`Point Representation`)

rm(extract_lat, extract_lat_long, extract_long, lat_long)

write.csv2(data, "Extraction_automatique_pdf_CT_M2/donnees_extractions-pdf.csv", row.names = FALSE)
