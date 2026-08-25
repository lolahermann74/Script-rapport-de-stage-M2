# Script Action 02_02 : Exploration des données télécapêche

  # - 1. Importation des données
  # - 2. Comptage pecheur de loire
  # - 3. Moyenne par saison
  # - 4. Captures par marée
  # - 6. Calendrier déclaration
  # - 7. Comptage des déclarations
  # - 8. Calendrier des plus de 4
  # - 9. Conso et repeuplement


# Auteur : Lola 
# Date : 06/05/2025

# Packages
library(readr) ; library(readxl) ; library(dplyr) ;library(ggplot2)
library(stringr) ; library(plotly) ; library(ggridges) ; library(patchwork)
library(ggpubr) ; library(gridExtra) ; library(ggtext) ; library(tidyr)

  # 1. Importation des données ####
tecap <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/CPUE/R/output/1_METHODE/télécapêche/telecapeche_2014-2025_navire.csv")

tecap <- tecap %>%
  select(!X) %>%
  filter(!(saison %in% c("2014-2015", "2015-2016")))

  # 2. Comptage pecheur de loire ####
unique(tecap$nom_zone)
loire <- tecap %>%
  filter(nom_zone %in% c("Loire", "Loire Lot n°13", "Loire Lot n°14", "Loire Lot n°15"))

toutes_saisons <- c("2016-2017", "2017-2018",
                    "2018-2019","2019-2020","2020-2021","2021-2022",
                    "2022-2023","2023-2024","2024-2025")

pecheurs_saison_loire <- loire %>%
  group_by(saison) %>%
  summarise(nb_pecheurs = n_distinct(pecheur)) %>%
  complete(saison = toutes_saisons) %>% 
  mutate(saison = factor(saison, levels = toutes_saisons))

  # 3. Moyenne capture tot pecheurs par saison ####
pecheurs_saison <- tecap %>%
  group_by(saison) %>%
  summarise(nb_pecheurs = n_distinct(pecheur)) %>%
  complete(saison = toutes_saisons) %>% 
  mutate(saison = factor(saison, levels = toutes_saisons))

    # graphique
nb_pech <- ggplot(pecheurs_saison, aes(x = saison, y = nb_pecheurs, group = 1)) +
  geom_hline(yintercept = 135, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 140, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 145, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 150, color = "darkgrey", linewidth = 0.5) +
  geom_line(color = "blue", linewidth = 0.8, na.rm = FALSE) +
  geom_point(size = 1, color = "blue", na.rm = TRUE) +
  scale_y_continuous(breaks = seq(135, 150, by = 10), limits = c(135,150)) +
  labs(title = "Evolution du total moyen de capture des pêcheurs maritimes\nappartenant au Pays-de-la-Loire (2016-2025)",
       x = "Saison",
       y = "Nombre de\npêcheurs") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, hjust = 0.5, color = "red", face = "bold"),
        axis.text.y = element_text(size = 13, color = "blue"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14, color = "blue"),
        axis.title.x = element_blank())

nb_pech


tecap1 <- tecap %>%
  group_by(saison, pecheur) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(saison) %>%
  summarise(moy_tot = mean(poids_tot, na.rm = TRUE),
            ecart_type = sd(poids_tot, na.rm = TRUE),
            n = n(),
            erreur_standard = ecart_type / sqrt(n),
            .groups = "drop") %>%
  complete(saison = toutes_saisons) %>% 
  mutate(saison = factor(saison, levels = toutes_saisons))


capture_moy <- ggplot(tecap1, aes(x= saison, y = moy_tot, group = 1)) + 
  geom_hline(yintercept = 125, color = "darkgrey", size = 0.5) +
  geom_hline(yintercept = 150, color = "darkgrey", size = 0.5) +
  geom_hline(yintercept = 100, color = "darkgrey", size = 0.5) +
  geom_hline(yintercept = 75, color = "darkgrey", size = 0.5) +
  #annotate("text", x = "2011-2012", y = 194, label = "190 kg", color = "darkgrey")+
  geom_line(color = "red", linewidth = 0.9) +
  geom_errorbar(aes(ymin = moy_tot - erreur_standard,
                    ymax = moy_tot + erreur_standard),
                width = 0.1,
                color = "black") +
  geom_point(color = "red") +
  labs(x = "Saison",
       y = "Captures totales (en kg)")+
  theme_minimal() +
  theme(axis.title.y = element_text(size = 13, color = "red", face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"))

capture_moy

nav_and_capture <- nb_pech + plot_spacer() + capture_moy +
  plot_layout(ncol = 2, widths = c(5, 1), heights = c(1, 5)) ; nav_and_capture

  # 4. Captures par marée ####
stats_saison <- tecap %>%
  group_by(saison) %>%
  summarise(
    n_pecheurs = n_distinct(identifiant),
    mediane_poids = median(poids, na.rm = TRUE),
    .groups = "drop")

ggplot(tecap, aes(x = saison, y = poids)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red") +
  geom_text(data = stats_saison, aes(x = saison, y = mediane_poids +0.9, 
                                     label = paste0("n=", n_pecheurs)), size = 3, color = "white") +
  labs(title = "Distribution des captures (Kg) par marée en \nfonction des saisons (2013-2025)",
       x = "Saison",
       y = "Captures (Kg)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, color = "darkblue", hjust = 0.5),
    axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"))

ggplot(tecap, aes(x = saison, y = poids)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.shape = NA) +
  geom_text(data = stats_saison, aes(x = saison, y = mediane_poids + 0.9,
                                     label = paste0("n=", n_pecheurs)),
            size = 4, color = "white") +
  coord_cartesian(ylim = c(0, 25)) +
  labs(title = "Distribution des captures (Kg) par marée en \nfonction des saisons (2014-2025)",
       x = "Saison",
       y = "Captures (Kg)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, color = "darkblue", hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"))


  # 5. recherche ####
tecap %>% filter(poids>50)

 
  # 6. Comptage des déclarations ####
compte_decl <- tecap_full %>%
  group_by(saison, pecheur) %>%
  summarise(n_decl = n(), .groups = "drop")

  # 7.Calendrier déclaration ####
tecap2 <- tecap %>%
  mutate(presence = 1,
         date = as.Date(date))

tecap3 <- tecap2 %>%
  group_by(saison, pecheur, date) %>%
  summarise(presence = 1, .groups = "drop")

tecap_full <- tecap3 %>%
  group_by(saison, pecheur) %>%
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(presence = 0)) %>%
  ungroup()

matrice <- tecap3 %>%
  pivot_wider(names_from = date,
              values_from = presence,
              values_fill = 0)

saison_choisie <- "2016-2017"

# numéro du groupe
groupe <- 1   # 1 = pêcheurs 1 à 10
# 2 = pêcheurs 11 à 20
# etc.

data_saison <- tecap_full %>%
  filter(saison == saison_choisie)

# liste des pêcheurs
liste_pecheurs <- unique(data_saison$pecheur)

# sélectionner 10 pêcheurs
pecheurs_selection <- liste_pecheurs[((groupe - 1) * 20 + 1):(groupe * 20)]

# filtrer les données
data_plot <- data_saison %>%
  filter(pecheur %in% pecheurs_selection)

x <- ggplot(data_plot,
            aes(x = date,
                y = pecheur,
                fill = factor(presence))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey90",
                               "1" = "steelblue")) +
  labs(title = paste0("Saison : ", saison_choisie,
                      " — Groupe ", groupe),
       fill = "Présence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(x)

compte_decl <- compte_decl %>%
  mutate(categorie = ifelse(n_decl >= 15,
                            ">= 15 déclarations",
                            "< 15 déclarations"))

tecap_classe <- tecap %>%
  left_join(compte_decl, by = c("saison", "pecheur"))

# pêcheurs actifs / réguliers par saison
plus_de_10 <- tecap_classe %>%
  filter(categorie == ">= 15 déclarations")

# pêcheurs occasionnels ou peu déclarants (aggrégation des déclarations)
moins_de_10 <- tecap_classe %>%
  filter(categorie == "< 15 déclarations")


intersect(moins_de_10$pecheur, plus_de_10$pecheur)


# Nombre de déclarations par saison et pêcheur
compte_decl <- tecap %>%
  group_by(saison, mois, pecheur) %>%
  summarise(n_decl = n(), .groups = "drop")

# Catégorie UNIQUE par pêcheur
categorie_pecheur <- compte_decl %>%
  group_by(pecheur) %>%
  summarise(categorie = ifelse(
    any(n_decl < 4),
    "< 4 déclarations",
    ">= 4 déclarations"),
    .groups = "drop")

# Jointure sur le pêcheur uniquement
tecap_classe <- tecap %>%
  left_join(categorie_pecheur, by = "pecheur")

# Séparation des groupes
plus_de_4 <- tecap_classe %>%
  filter(categorie == ">= 4 déclarations")

moins_de_4 <- tecap_classe %>%
  filter(categorie == "< 4 déclarations")


length(unique(plus_de_4$pecheur))

  # 8. Calendrier des plus de 4 ####
pech11 <- plus_de_4 %>%
  mutate(presence = 1,
         date = as.Date(date))

pech11_2 <- pech11 %>%
  group_by(saison, pecheur, date) %>%
  summarise(presence = 1, .groups = "drop")

pech11_full <- pech11_2 %>%
  group_by(saison, pecheur) %>%
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(presence = 0)) %>%
  ungroup()

matrice <- pech11_2 %>%
  pivot_wider(names_from = date,
              values_from = presence,
              values_fill = 0)

saison_choisie <- "2016-2017"

data_saison <- pech11_full %>%
  filter(saison == saison_choisie)

x <- ggplot(data_saison,
            aes(x = date,
                y = pecheur,
                fill = factor(presence))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey90",
                               "1" = "steelblue")) +
  labs(title = paste0("Saison : ", saison_choisie,
                      " — Groupe ", groupe),
       fill = "Présence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(x)

  # 9. Conso et repeuplement ####
tecap_year <- tecap %>%
  filter(saison == "2020-2021",
         !poids==0) %>%
  mutate(date = as.Date(date))

plot1 <- ggplot(tecap_year, aes(x = date, y = poids, color = destination)) +
  geom_point() +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%d/%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(plot1)
