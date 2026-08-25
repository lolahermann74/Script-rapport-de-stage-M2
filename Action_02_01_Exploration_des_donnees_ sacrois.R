# Script Action 02_01 : Exploration des données SACROIS

  # - 1. Importation des données
  # - 2. Moyenne par saison
  # - 3. Nombre de saison par pêcheur
  # - 4. Captures par marée
  # - 5. comparaison titulaire licence


# Auteur : Lola
# Date : 07/05/2026

# Packages ####
library(readr) ; library(readxl) ; library(dplyr) ;library(ggplot2)
library(stringr) ; library(plotly) ; library(ggridges) ; library(patchwork)
library(ggpubr) ; library(gridExtra)

# SACROIS ####
  # 1. Importation des données ####
sacrois <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/sacrois_2016-2025_licence_combo_ready.csv")

sacrois <- sacrois %>%
  select(!X)

  # 2. Moyenne par saison ####
toutes_saisons <- c("2016-2017", "2017-2018", "2018-2019","2019-2020",
                     "2020-2021","2021-2022", "2022-2023","2023-2024",
                     "2024-2025")


navires_saison <- sacrois %>%
  group_by(saison) %>%
  summarise(nb_navires = n_distinct(immat)) %>%
  mutate(saison = factor(saison, levels = toutes_saisons))

    # graphique
nb_nav <- ggplot(navires_saison, aes(x = saison, y = nb_navires, group = 1)) +
  geom_hline(yintercept = 145, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 140, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 135, color = "darkgrey", linewidth = 0.5) +
  geom_line(color = "blue", linewidth = 0.8, na.rm = FALSE) +
  geom_point(size = 1, color = "blue", na.rm = TRUE) +
  scale_y_continuous(breaks = seq(135, 150, by = 10), limits = c(135,150)) +
  labs(title = "Evolution du total moyen des pêcheurs maritimes faisant\npartie des Pays-de-la-Loire (2016 à 2025)",
       x = "Saison",
       y = "Nombre de\npêcheurs") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, hjust = 0.5, color = "red", face = "bold"),
        axis.text.y = element_text(size = 13, color = "blue"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14, color = "blue"),
        axis.title.x = element_blank()) ; nb_nav


sacrois1 <- sacrois %>%
  group_by(saison, nom_navire) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(saison) %>%
  summarise(moy_tot = mean(poids_tot, na.rm = TRUE),
            ecart_type = sd(poids_tot, na.rm = TRUE),
            n = n(),
            erreur_standard = ecart_type / sqrt(n),
            .groups = "drop") %>%
  mutate(saison = factor(saison, levels = toutes_saisons))


capture_moy <- ggplot(sacrois1, aes(x= saison, y = moy_tot, group = 1)) + 
  geom_hline(yintercept = 125, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 150, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 100, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 75, color = "darkgrey", linewidth = 0.5) +
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
        axis.text.y = element_text(size = 13, color = "black")) ; capture_moy

nav_and_capture <- nb_nav + plot_spacer() + capture_moy +
  plot_layout(ncol = 2, widths = c(5, 1), heights = c(1, 5)) ; nav_and_capture


  # 3. Nombre de saison par pêcheur ####
nb_saison_licence <- sacrois %>%
  group_by(pecheur) %>%
  summarise(nb_saison = n_distinct(saison))

  # 4. Captures par marée ####
stats_saison <- sacrois %>%
  group_by(saison) %>%
  summarise(
    n_pecheurs = n_distinct(immat),
    mediane_poids = median(poids, na.rm = TRUE),
    .groups = "drop")

ggplot(sacrois, aes(x = saison, y = poids)) +
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

ggplot(sacrois, aes(x = saison, y = poids)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.shape = NA) +
  geom_text(data = stats_saison, aes(x = saison, y = mediane_poids + 0.9,
                                     label = paste0("n=", n_pecheurs)),
            size = 4, color = "white") +
  coord_cartesian(ylim = c(0, 25)) +
  labs(title = "Distribution des captures (Kg) par marée en \nfonction des saisons (2016-2025) ~ SACROIS",
       x = "Saison",
       y = "Captures (Kg)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, color = "darkblue", hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"))

  # 5. comparaison titulaire licence ####
    # Importation des licences CMEA
licences_corepem <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/Table_licenceCMEA_COREPEM_2014-2025.csv")

licences_saison <- licences_corepem %>%
  filter(!(saison %in% c("2014-2015", "2015-2016"))) %>%
  group_by(saison) %>%
  summarise(nb_licenceCMEA_navire = n_distinct(pecheur)) %>%
  mutate(saison = factor(saison, levels = toutes_saisons))

navires_saison <- sacrois %>%
  group_by(saison) %>%
  summarise(nb_navires = n_distinct(immat_combo),
            nb_licenceCMEA = n_distinct(immat)) %>%
  mutate(saison = factor(saison, levels = toutes_saisons))


ggplot(navires_saison, aes(x = saison)) +
  geom_line(aes(y = nb_navires, color = "Total", group = 1), linewidth = 1) +
  geom_point(aes(y = nb_navires), size = 1, color = "darkblue") +
  geom_line(aes(y = nb_licenceCMEA, color = "Licence CMEA dans le fichier SACROIS", group = 1), linewidth = 1) +
  geom_point(aes(y = nb_licenceCMEA), size = 1, color = "darkorange3") +
  geom_line(data = licences_saison,
            aes(y = nb_licenceCMEA_navire, color = "Licence fichier corepem", group = 1),
            linewidth = 1) +
  geom_point(data = licences_saison,
             aes(y = nb_licenceCMEA_navire), color = "darkgreen",
             size = 1) +
  scale_color_manual(
    values = c(
      "Total" = "steelblue",
      "Licence CMEA dans le fichier SACROIS" = "orange",
      "Licence fichier corepem" = "#76BD6A")) +
  labs(title = "Comparaison entre le nombre de navire ayant déclaré dans la base \nSACROIS et le nombre de titulaire d'une licence CMEA sur\nla période 2008-2025",
       x = "Saison",
       y = "Nombre de navires",
       color = "Type de navires") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, color = "darkblue", hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "top")
