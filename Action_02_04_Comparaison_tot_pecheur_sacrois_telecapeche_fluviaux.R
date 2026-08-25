# Action_02_04 : Comparaison du total par pêcheur entre 3 jeux de données
#     SACROIS
#     Télécapêche
#     Fluviaux

# Auteur : Lola 
# Date : 11/05/2025

# Packages
library(readr) ; library(readxl) ; library(dplyr) ;library(ggplot2)
library(stringr) ; library(plotly) ; library(ggridges) ; library(patchwork)
library(ggpubr) ; library(gridExtra) ; library(ggtext)

# 1. Importation des données ####
tecap <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/telecapeche_2014-2025_navire.csv")

tecap <- tecap %>%
  select(!X)


fluv <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/fluviaux_2013-2025.csv")

fluv <- fluv %>%
  select(!X) %>%
  filter(!saison == "2013-2014")

sacrois <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/sacrois_2016-2025_licence_combo_ready.csv")

sacrois <- sacrois %>%
  select(!X) %>%
  filter(!quartier%in% c("VA", "LR"))

# 2. regroupement moyenne ####
toutes_saisons <- c("2014-2015","2015-2016","2016-2017", "2017-2018",
                    "2018-2019","2019-2020","2020-2021","2021-2022",
                    "2022-2023","2023-2024","2024-2025")


s1 <- sacrois %>%
  group_by(saison, nom_navire) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(saison) %>%
  summarise(moy_tot = mean(poids_tot, na.rm = TRUE),
            ecart_type = sd(poids_tot, na.rm = TRUE),
            n = n(),
            erreur_standard = ecart_type / sqrt(n),
            .groups = "drop") %>%
  complete(saison = toutes_saisons) %>%
  mutate(saison = factor(saison, levels = toutes_saisons),
         source = "SACROIS")

t1 <- tecap %>%
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
  mutate(saison = factor(saison, levels = toutes_saisons),
         source = "Télécapêche")

f1 <- fluv %>%
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
  mutate(saison = factor(saison, levels = toutes_saisons),
         source = "Fluviaux")

data_finale <- bind_rows(s1, t1, f1)

# 3. graphique ####
capture_moy <- ggplot(data_finale, aes(x = saison, y = moy_tot, group = source,
                                       color = source, fill = source)) + 
  geom_hline(yintercept = c(75, 100, 125, 150),
             color = "darkgrey",
             linewidth = 0.5) +
  geom_ribbon(aes(ymin = moy_tot - erreur_standard,
                  ymax = moy_tot + erreur_standard),
              alpha = 0.2,
              color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "SACROIS" = "darkorchid2",
    "Télécapêche" = "red",
    "Fluviaux" = "cornflowerblue")) +
  scale_fill_manual(values = c(
    "SACROIS" = "darkorchid2",
    "Télécapêche" = "red",
    "Fluviaux" = "cornflowerblue")) +
  labs(title = "Comparaison des moyennes des captures totales des pêcheurs pour\nles saison de 2014 à 2025 selon différentes sources",
       x = "Saison",
       y = "Captures totales moyennes (kg)",
       color = "Source",
       fill = "Source") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"),
        legend.title = element_text(face = "bold")) ; capture_moy

# 4. total ####
s2 <- sacrois %>%
  group_by(saison) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  complete(saison = toutes_saisons) %>%
  mutate(saison = factor(saison, levels = toutes_saisons),
         source = "SACROIS")

t2 <- tecap %>%
  group_by(saison) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(saison) %>%
  mutate(saison = factor(saison, levels = toutes_saisons),
         source = "Télécapêche")

f2 <- fluv %>%
  group_by(saison) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(saison = factor(saison, levels = toutes_saisons),
         source = "Fluviaux")

data_finale2 <- bind_rows(s2, t2, f2)

# 5. graphique tot ####
capture_tot <- ggplot(data_finale2, aes(x = saison, y = poids_tot, group = source,
                                       color = source)) + 
  geom_hline(yintercept = c(2000, 10000, 20000, 25000),
             color = "darkgrey",
             linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "SACROIS" = "darkorchid2",
    "Télécapêche" = "red",
    "Fluviaux" = "cornflowerblue")) +
  labs(title = "Comparaison des captures totales pour les saison\nde 2014 à 2025 selon différentes sources",
       x = "Saison",
       y = "Captures totales (kg)",
       color = "Source",
       fill = "Source") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"),
        legend.title = element_text(face = "bold")) ; capture_tot

# 6. Ajout visiomer ####
visiomer <- read_excel("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/Prod_RIC_20_24_pop_ref.xlsx", 
                       col_types = c("text", "text", "numeric", 
                                     "text", "text", "numeric", "numeric", 
                                     "numeric", "numeric"))

v1 <- visiomer %>%
  mutate(year = as.numeric(paste0("20", year)),
         saison = if_else(month >= 11,
                          paste0(year, "-", year + 1),
                          paste0(year - 1, "-", year)),
         mois = factor(month,
                       levels = 1:12,
                       labels = c("janvier", "février", "mars", "avril",
                                  "mai", "juin", "juillet", "août",
                                  "septembre", "octobre", "novembre", "décembre")),
         immat = as.integer(code_bat)) %>%
  filter(espece == "ELE",
         mois%in% c("décembre", "janvier", "février", "mars", "avril"),
         !saison == "2024-2025")

v2 <- v1 %>%
  group_by(saison) %>%
  summarise(poids_tot = sum(qte, na.rm = TRUE),
            .groups = "drop") %>%
  complete(saison = toutes_saisons) %>%
  mutate(saison = factor(saison, levels = toutes_saisons),
         source = "Visiomer")

data_finale3 <- bind_rows(s2, t2, f2, v2)


capture_tot2 <- ggplot(data_finale3, aes(x = saison, y = poids_tot, group = source,
                                        color = source)) + 
  geom_hline(yintercept = c(2000, 10000, 20000, 25000),
             color = "darkgrey",
             linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "SACROIS" = "darkorchid2",
    "Télécapêche" = "red",
    "Fluviaux" = "cornflowerblue",
    "Visiomer" = "darkorange")) +
  labs(title = "Comparaison des captures totales pour les saison\nde 2014 à 2025 selon différentes sources",
       x = "Saison",
       y = "Captures totales (kg)",
       color = "Source",
       fill = "Source") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"),
        legend.title = element_text(face = "bold")) ; capture_tot2
