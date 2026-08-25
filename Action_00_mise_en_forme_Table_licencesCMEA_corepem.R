# Script Action 00 : Mise en forme de la Table des licences CMEA (source : COREPEM) :

# Auteur : Lola
# Date : 06/05/2026

# Packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Importation des licences
# Fonction pour lire un sheet selon les années
import_licence <- function(file_path, annee_debut, annee_fin) {
  sheet_name <- paste0(annee_debut, "-", annee_fin)
  name_data <- paste0("licenceCMEA_", annee_debut, "_", annee_fin)
  name_data <- read_excel(file_path, sheet = sheet_name)
}

# Chemin du fichier
file_path <- "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/Table_licenceCMEA_COREPEM_2008-2025.xlsx"
licenceCMEA_2014_2015 <- import_licence(file_path, 2014, 2015)
licenceCMEA_2015_2016 <- import_licence(file_path, 2015, 2016)
licenceCMEA_2016_2017 <- import_licence(file_path, 2016, 2017)
licenceCMEA_2017_2018 <- import_licence(file_path, 2017, 2018)
licenceCMEA_2018_2019 <- import_licence(file_path, 2018, 2019)
licenceCMEA_2019_2020 <- import_licence(file_path, 2019, 2020)
licenceCMEA_2020_2021 <- import_licence(file_path, 2020, 2021)
licenceCMEA_2021_2022 <- import_licence(file_path, 2021, 2022)
licenceCMEA_2022_2023 <- import_licence(file_path, 2022, 2023)
licenceCMEA_2023_2024 <- import_licence(file_path, 2023, 2024)
licenceCMEA_2024_2025 <- import_licence(file_path, 2024, 2025)

licences_all_immat1 <- bind_rows(
  licenceCMEA_2014_2015 %>% 
    mutate(saison = "2014-2015", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2015_2016 %>% 
    mutate(saison = "2015-2016", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2016_2017 %>% 
    mutate(saison = "2016-2017", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2017_2018 %>% 
    mutate(saison = "2017-2018", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2018_2019 %>% 
    mutate(saison = "2018-2019", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2019_2020 %>% 
    mutate(saison = "2019-2020", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2020_2021 %>% 
    mutate(saison = "2020-2021", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2021_2022 %>% 
    mutate(saison = "2021-2022", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2022_2023 %>% 
    mutate(saison = "2022-2023", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2023_2024 %>% 
    mutate(saison = "2023-2024", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2024_2025 %>% 
    mutate(saison = "2024-2025", immat = as.character(`Immat 1`), 
           nom_navire = `Nom Navire 1`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`))

licences_all_immat1 <- licences_all_immat1 %>%
  mutate(immat = str_replace_all(immat, " ", "")) %>%  
  mutate(immat = as.integer(immat))

licences_all_immat2 <- bind_rows(
  licenceCMEA_2016_2017 %>% 
    mutate(saison = "2016-2017", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2017_2018 %>% 
    mutate(saison = "2017-2018", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2018_2019 %>% 
    mutate(saison = "2018-2019", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2019_2020 %>% 
    mutate(saison = "2019-2020", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2020_2021 %>% 
    mutate(saison = "2020-2021", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2021_2022 %>% 
    mutate(saison = "2021-2022", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2022_2023 %>% 
    mutate(saison = "2022-2023", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2023_2024 %>% 
    mutate(saison = "2023-2024", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`),
  
  licenceCMEA_2024_2025 %>% 
    mutate(saison = "2024-2025", immat = as.character(`Immat 2`), 
           nom_navire = `Nom Navire 2`) %>% 
    select(saison, immat, nom_navire, NOM, `Prénom`))

licences_all_immat2 <- licences_all_immat2 %>%
  mutate(immat = str_replace_all(immat, " ", "")) %>%  
  mutate(immat = as.integer(immat)) %>%
  filter(!is.na(immat))

licences_all <- bind_rows(licences_all_immat1,
                          licences_all_immat2)

licences_all <- licences_all %>%
  mutate(immat = as.integer(immat)) %>%
  arrange(NOM, saison)

licences_corepem <- licences_all %>%
  mutate(immat_licence = immat,
         pecheur = paste0(NOM," ",Prénom))


# Exportation
write.csv(licences_corepem, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/Table_licenceCMEA_COREPEM_2014-2025.csv")
