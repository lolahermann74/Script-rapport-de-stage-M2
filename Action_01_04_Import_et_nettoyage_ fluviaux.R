# Script Action 01_04 : Regroupement datas fluviaux et harmonisation : 
  # 1. Importation des fluviaux
  # 2. Exportation du data


# Auteur : Lola 
# Date : 06/05/2026
# Dernière date de modif : 06/05/2026

# Packages
library(readr) ; library(dplyr) ; library(readxl) ; library(ggplot2) ; library(lubridate)
library(tidyr) ; library(stringr) ; library(writexl) ; library(gridExtra) ; 
library(plotly) ; library(ggridges) ; library(patchwork) ; library(ggpubr)
library(gridExtra) ; library(ggtext)

  # 1. Importation des fluviaux ####
    # Fonction pour lire un sheet selon les années
import_fluviaux <- function(file_path, annee_debut, annee_fin) {
  sheet_name <- paste0("Saison ", annee_debut, "-", annee_fin)
  name_data <- paste0("fluviaux_", annee_debut, "_", annee_fin)
  name_data <- read_excel(file_path, sheet = sheet_name)
}

    # Chemin du fichier
file_path <- "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/Captures civelles PPED44 2013 à 2026.xlsx"

fluviaux_2013_2014 <- import_fluviaux(file_path, 2013, 2014)
fluviaux_2014_2015 <- import_fluviaux(file_path, 2014, 2015)
fluviaux_2015_2016 <- import_fluviaux(file_path, 2015, 2016)
fluviaux_2016_2017 <- import_fluviaux(file_path, 2016, 2017)
fluviaux_2017_2018 <- import_fluviaux(file_path, 2017, 2018)
fluviaux_2018_2019 <- import_fluviaux(file_path, 2018, 2019)
fluviaux_2019_2020 <- import_fluviaux(file_path, 2019, 2020)
fluviaux_2020_2021 <- import_fluviaux(file_path, 2020, 2021)
fluviaux_2021_2022 <- import_fluviaux(file_path, 2021, 2022)
fluviaux_2022_2023 <- import_fluviaux(file_path, 2022, 2023)
fluviaux_2023_2024 <- import_fluviaux(file_path, 2023, 2024)
fluviaux_2024_2025 <- import_fluviaux(file_path, 2024, 2025)


fluviaux_all <- bind_rows(fluviaux_2013_2014,
                          fluviaux_2014_2015,
                          fluviaux_2015_2016,
                          fluviaux_2016_2017,
                          fluviaux_2017_2018,
                          fluviaux_2018_2019,
                          fluviaux_2019_2020,
                          fluviaux_2020_2021,
                          fluviaux_2021_2022,
                          fluviaux_2022_2023,
                          fluviaux_2023_2024,
                          fluviaux_2024_2025)


fluviaux_all$destination[fluviaux_all$destination == "Repeuplement"] <- "R"
fluviaux_all$destination[fluviaux_all$destination == "Consommation"] <- "C"
unique(fluviaux_all$destination)


length(unique(fluviaux_2024_2025$identifiant))
length(unique(fluviaux_all$identifiant))

fluviaux_all <- fluviaux_all %>%
  mutate(date = as.Date(date),
         annee = year(date),                             
         mois = lubridate::month(date, label = TRUE, abbr = FALSE),
         saison = if_else(
           month(date) >= 11,
           paste0(year(date), "-", year(date) + 1),
           paste0(year(date) - 1, "-", year(date))),
         nom_zone = `nom zone`) %>%
  select(annee, saison, mois, date, identifiant, poids, destination, nom_zone)

  # 2. Exportation du data ####
write.csv(fluviaux_all, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/fluviaux_2013-2025.csv")

  # 3. Identification de 3 pêcheurs fluviaux connus
    # Importation des données des 3 pêcheurs connus dans le jeu de données : Echantillon préliminaire
OD_pecheurs <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/CPUE/R/data/BD_Civelle.csv", sep = ";")

OD_pecheurs <- OD_pecheurs %>%
  mutate(pecheur = recode(PECHEUR,
                          "Gaëtan BONNET" = "BONNET Gaetan",
                          "Martial BARAUD" = "BARAUD Martial",
                          "Alexis BAUMAL" = "BAUMAL Alexis",
                          "Louis BLANCHARD" = "BLANCHARD Louis",
                          "Yann TAILLANDIER" = "TAILLANDIER Yann",
                          "Fabrice BATARD" = "BATARD Fabrice",
                          "Michel LERAY" = "LERAY Michel",
                          "Ronan BRIERE" = "BRIERE Ronan",
                          "Franck JOSNIN" = "JOSNIN Franck"),
         poids = gsub(",", ".", POIDS),
         poids = as.numeric(poids),
         saison = gsub("/", "-", SAISON))

unique(OD_pecheurs$pecheur)

pf <- OD_pecheurs %>%
filter(pecheur %in% c("BARAUD Martial", "BATARD Fabrice", "BONNET Gaetan"))

  # Comparaison total de capture par saison
pf_saison <- pf %>%
  group_by(pecheur, saison) %>%
  summarise(poids_total = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(source = "PF")

fluviaux_saison <- fluviaux_all %>%
  mutate(pecheur = as.character(identifiant)) %>%
  group_by(pecheur, saison) %>%
  summarise(poids_total = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(source = "Fluviaux")

comparaison_saison <- bind_rows(pf_saison, fluviaux_saison)

gg_saison <- ggplot(comparaison_saison, aes(x = saison, y = poids_total, 
                                            group = pecheur,
                                            colour = pecheur,
                                            text = paste0("Source : ", source,
                                                          "<br>Saison : ", saison,
                                                          "<br>Poids : ", round(poids_total, 1), " kg"))) +
  geom_line() +
  geom_point() +
  labs(x = "Saison",
       y = "Poids total (kg)",
       colour = "Source") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gg_saison, tooltip = "text")


  # Comparaison total de capture/mois/pêcheur/saison
ordre_mois <- c("novembre", "décembre", "janvier", "février", "mars", "avril")

pf_mois <- pf %>%
  mutate(date = as.Date(DATE, format = "%d/%m/%Y"),
         mois = c("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre")[as.numeric(format(date, "%m"))]) %>%
  group_by(pecheur, saison, mois) %>%
  summarise(poids_total = sum(poids, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "PF")

fluviaux_mois <- fluviaux_all %>%
  mutate(pecheur = as.character(identifiant),
         mois = as.character(mois)) %>%
  group_by(pecheur, saison, mois) %>%
  summarise(poids_total = sum(poids, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Fluviaux")

comparaison_mois <- bind_rows(pf_mois, fluviaux_mois) %>%
  mutate(mois = factor(mois, levels = ordre_mois, ordered = TRUE))


gg_mois <- ggplot(comparaison_mois, aes(x = mois, y = poids_total, group = pecheur,
                                        colour = pecheur,
                                        text = paste0("Source : ", source,
                                                      "<br>Pêcheur / ID : ", pecheur,
                                                      "<br>Saison : ", saison,
                                                      "<br>Mois : ", mois,
                                                      "<br>Poids : ", round(poids_total, 1), " kg"))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ saison) +
  labs(x = "Mois",
       y = "Poids total (kg)",
       colour = "Pêcheur / ID") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gg_mois, tooltip = "text")


# Après observation graphique les identifiant correspondant au pêcheur sont :
    # - BARAUD Martial : 145
    # - BATARD Fabrice : 153
    # - BONNET Gaetan : 143

pf_final <- fluviaux_all %>%
  filter(identifiant %in% c("143", "153", "145")) %>%
  mutate(pecheur = recode(identifiant,
                          "145" = "BARAUD Martial",
                          "153" = "BATARD Fabrice",
                          "143" = "BONNET Gaetan"))

write.csv(pf_final, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/3pecheurs_fluviaux_2013-2025.csv")
### FIN ####