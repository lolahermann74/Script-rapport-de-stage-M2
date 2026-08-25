# Script Action 01_02 : Regroupement data télécapêche OP et hors OP et harmonisation Télécapêche : 
  # - 1. Importation des données
  # - 2. OP - Harmonisation 
  # - 3. hors OP - Harmonisation
  # - 4. Fusion des 2 datas
  # - 5. Ajout saison
  # - 6. Exportation
  # - 7. Ajout nom navires

# Auteur : Lola
# Date : 06/05/2026

# Packages
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(readxl)
library(writexl)

# TELECAPECHE

  # 1. Importation des données ####
dir_data <- "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/"
dir_output <- "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/"

op <- read.csv2(file.path(dir_data, "Télécapêche OP Estuaires_03042026.csv"))
horsop <- read.csv2(file.path(dir_data, "Telecapeche_horsOP_20260430.csv"))

  # 2. OP - Harmonisation ####
op1 <- op %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         pecheur = paste0(nom, " ", `prénom`),
         code_zone = code.zone,
         nom_zone = nom.zone, 
         poids = as.numeric(poids..kg.),
         destination = C.R, 
         adherent = "OP") %>%
  select(date, pecheur, poids, destination, adherent, code_zone, nom_zone, identifiant)

  # 3. hors OP - Harmonisation ####
horsop1 <- horsop %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         pecheur = paste0(nom, " ", `prénom`),
         code_zone = code.zone,
         nom_zone = nom.zone, 
         poids = as.numeric(poids..kg.),
         destination = C.R, 
         adherent = "hors OP") %>%
  select(date, pecheur, poids, destination, adherent, code_zone, nom_zone, identifiant)

  # 4. Fusion des 2 datas ####
tecap <- bind_rows(op1, horsop1)

  # 5. Ajout saison ####
tecap1 <- tecap %>%
  mutate(annee = year(date),                             
         mois = month(date, label = TRUE, abbr = FALSE),
         saison = if_else(
           month(date) >= 11,
           paste0(year(date), "-", year(date) + 1),
           paste0(year(date) - 1, "-", year(date))),
         pecheur = str_replace(pecheur, "^([^ ]+)", ~ str_to_upper(.x))) %>%
  filter(!saison%in%c("2013-2014", "2025-2026")) %>%
  select(annee, saison, mois, date, pecheur, poids, destination, adherent, code_zone, nom_zone, identifiant)

unique(tecap1$mois)
unique(tecap1$saison)

novembre_mai <- tecap1 %>%
  filter(mois%in% c("novembre", "mai"))

nb_pecheur_24_25 <- tecap1 %>% filter(saison == "2024-2025")
length(unique(nb_pecheur_24_25$pecheur))

  # 6. Exportation ####
write.csv(tecap1, file.path(dir_output, "tecap_2014-2025.csv") )

  # 7. Ajout nom navires ####
    # Importation des licences CMEA
licence_navire <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/licence_navire_combo.csv")
    
# Mise en Forme télécapêche
t1 <- tecap1 %>%
  mutate(pecheur = case_when(
    pecheur == "DAUBIN Shean" & saison%in% c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", 
                                             "2018-2019", "2019-2020", 
                                             "2020-2021") ~ "DAUBIN Sébastien",
    pecheur == "SEPTIER Steven" ~ "SEPTIER Stevens",
    pecheur == "BAIZEAU Jean-claude" ~ "BAIZEAU Jean-Claude",
    pecheur == "RONDINEAU  Julien" ~ "RONDINEAU Julien",
    pecheur == "MARAINDAZ Éric" ~ "MARAINDAZ Eric",
    pecheur == "ROUSSEAU  Jérémy " ~ "ROUSSEAU Jérémy",
    pecheur == "BLANDIN Regis" ~ "BLANDIN Régis",
    pecheur == "BRIERE Patrice / Ronan" ~ "BRIERE Ronan",
    pecheur == "BRIERE Patrice et Ronan" ~ "BRIERE Ronan",
    pecheur == "DAUBIN Shean" & saison%in% c("2021-2022", "2022-2023", "2023-2024") ~ "DAUBIN Jennifer",
    pecheur == "POUVREAU Franck" & saison%in% c("2021-2022", "2022-2023", "2023-2024") ~ "POUVREAU Pascale",
    TRUE ~ pecheur)) 

    # Ajout du navire au data
t2 <- t1 %>%
  left_join(licence_navire %>% 
              select(saison, immat_final, nom_navire, pecheur),
            by = c("saison", "pecheur"))

t2_2025 <- t2 %>% filter(saison == "2024-2025")
length(unique(t2_2025$pecheur))

t3 <- t2 %>% filter(is.na(immat_final))
t4 <- t2 %>% filter(!is.na(immat_final))

t3_2025 <- t3 %>% filter(saison == "2024-2025")
length(unique(t3_2025$pecheur))

t4_2025 <- t4 %>% filter(saison == "2024-2025")
length(unique(t4_2025$pecheur))


    # Exportation
write.csv(t4, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/telecapeche_2014-2025_navire.csv")

### FIN ####