# Script Action 02_03 : Exploration des données Fluviaux

  # - 1. Importation des données
  # - 2. moy capture tot pecheurs par saison
  # - 3. Captures par marée


# Auteur : Lola 
# Date : 06/05/2025

# Packages
library(readr) ; library(readxl) ; library(dplyr) ;library(ggplot2)
library(stringr) ; library(plotly) ; library(ggridges) ; library(patchwork)
library(ggpubr) ; library(gridExtra) ; library(ggtext)

# 1. Importation des données ####
fluv <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/fluviaux_2013-2025.csv")

fluv <- fluv %>%
  select(!X) %>%
  filter(!saison == "2013-2014")

# 2. moy capture tot pecheurs par saison ####
toutes_saisons <- c("2014-2015","2015-2016","2016-2017", "2017-2018",
                    "2018-2019","2019-2020","2020-2021","2021-2022",
                    "2022-2023","2023-2024","2024-2025")

pecheurs_saison <- fluv %>%
  group_by(saison) %>%
  summarise(nb_pecheurs = n_distinct(identifiant)) %>%
  mutate(saison = factor(saison, levels = toutes_saisons))

# graphique
nb_pech <- ggplot(pecheurs_saison, aes(x = saison, y = nb_pecheurs, group = 1)) +
  geom_hline(yintercept = 19, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 17, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 18, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 16, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 20, color = "darkgrey", linewidth = 0.5) +
  geom_line(color = "blue", linewidth = 0.8, na.rm = FALSE) +
  geom_point(size = 1, color = "blue", na.rm = TRUE) +
  scale_y_continuous(breaks = seq(15, 21, by = 2), limits = c(15,21)) +
  labs(title = "Evolution du total moyen des pêcheurs au cours des saisons - 2014-2025",
       x = "Saison",
       y = "Nombre de\npêcheurs") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, hjust = 0.5, color = "cornflowerblue", face = "bold"),
        axis.text.y = element_text(size = 13, color = "blue"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14, color = "blue"),
        axis.title.x = element_blank()) ; nb_pech


fluv1 <- fluv %>%
  group_by(saison, identifiant) %>%
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


capture_moy <- ggplot(fluv1, aes(x= saison, y = moy_tot, group = 1)) + 
  geom_hline(yintercept = 125, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 150, color = "darkgrey", linewidth = 0.5) +
  geom_hline(yintercept = 175, color = "darkgrey", linewidth = 0.5) +
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

nav_and_capture <- nb_pech + plot_spacer() + capture_moy +
  plot_layout(ncol = 2, widths = c(5, 1), heights = c(1, 5)) ; nav_and_capture

# 3. Captures par marée ####
stats_saison <- fluv %>%
  group_by(saison) %>%
  summarise(
    n_pecheurs = n_distinct(identifiant),
    mediane_poids = median(poids, na.rm = TRUE),
    .groups = "drop")

ggplot(fluv, aes(x = saison, y = poids)) +
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

ggplot(fluv, aes(x = saison, y = poids)) +
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

