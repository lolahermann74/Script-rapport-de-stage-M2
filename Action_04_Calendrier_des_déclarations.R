# Action_05 : Calendrier des déclarations des pêcheurs de l'échantillon "Groupe Loire" 

  # - 1. Importation des données
  # - 2. Calendrier des déclarations par pêcheur
  # - 3. Nombre de décla par saison
  # - 4. Calendrier tous les les pecheurs

# Auteur : Lola
# date : 16/06/2026

# Packages 
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)

  # 1. Importation des données ####
ech <- read_excel("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/Groupe_Loire_2016-2025.xlsx", 
                  sheet = "Prototype fini", col_types = c("text", 
                                                          "date", "date", "text", "numeric", 
                                                          "text", "text", "text"))


ech <- ech %>%
  mutate(date_la_plus_tot = as.Date(date_la_plus_tot)) ; head(ech)

  # 2. Calendrier des déclarations par pêcheur ####
unique(ech$pecheur)

pecheur_cible <- "BARAUD Martial"
pecheur_cible <- "BATARD Fabrice"
pecheur_cible <- "BAUMAL Alexis"
pecheur_cible <- "BAUMAL Franck"
pecheur_cible <- "BLANCHARD Louis"
pecheur_cible <- "BLANDIN Kévin"
pecheur_cible <- "BONNET Gaetan"
pecheur_cible <- "BRIERE Ronan"
pecheur_cible <- "JOSNIN Franck"
pecheur_cible <- "LERAY Michel"
pecheur_cible <- "LORCY Frédéric"
pecheur_cible <- "TAILLANDIER Yann"

data_p <- ech %>%
  filter(pecheur == pecheur_cible) %>%
  mutate(mois_jour = format(date_la_plus_tot, "%m-%d"),
         presence = 1) %>%
  distinct(saison, mois_jour, presence)

      # Grille complète (TOUTES les cases jour × saison)
grille <- expand.grid(saison = unique(ech$saison),
  mois_jour = unique(format(ech$date_la_plus_tot, "%m-%d")))

cal <- grille %>%
  left_join(data_p, by = c("saison", "mois_jour")) %>%
  mutate(presence = ifelse(is.na(presence), 0, 1))

      # CRÉATION D'UN ORDRE CHRONOLOGIQUE CORRECT (Déc → Nov)
cal <- cal %>%
  mutate(mois = as.integer(substr(mois_jour, 1, 2)),
         jour = as.integer(substr(mois_jour, 4, 5)),
         mois_tri = ifelse(mois == 12, 0, mois),
         ordre = mois_tri * 100 + jour)

      # factor ordonné correctement
cal$mois_jour <- factor(
  cal$mois_jour,
  levels = cal %>%
    arrange(ordre) %>%
    pull(mois_jour) %>%
    unique())

      # labels : seulement 1 et 15
labels_x <- ifelse(substr(levels(cal$mois_jour), 4, 5) %in% c("01", "15"),
                   levels(cal$mois_jour), "")

      # PLOT FINAL
ggplot(cal, aes(x = mois_jour, y = saison, fill = factor(presence))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey95", "1" = "steelblue")) +
  scale_x_discrete(labels = labels_x) +
  labs(x = "Jour - Mois",
       y = "Saison",
       title = paste0("Calendrier des déclarations - ", pecheur_cible)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        axis.text.y = element_text(size = 10))


  # 3. Nombre de décla par saison ####
decla <- ech %>%
  group_by(saison, pecheur) %>%
  summarise(nb_decla = n_distinct(date_la_plus_tot),
            .groups = "drop")


ggplot(decla, aes(x= saison, y = nb_decla, colour = pecheur, group = pecheur)) +
  geom_point() +
  geom_line(linewidth = 1.2) + 
  labs(title = "Evolution du nombre de jour de pêche en fonction des saisons",
       x = "Saison",
       y = "Nombre de déclarations",
       color = "Pêcheur") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"))

  # 4. Calendrier tous les les pecheurs ####
ech2 <- ech %>%
  select(saison, date_la_plus_tot, pecheur, poids) %>%
  group_by(saison, date_la_plus_tot, pecheur) %>%
  summarise(poids_tot = sum(poids, na.rm = FALSE), 
            .groups = "drop")
      # On compte le nombre de déclarations pour chaque jour et chaque saison
data_p <- ech2 %>%
  mutate(mois_jour = format(date_la_plus_tot, "%m-%d")) %>%
  group_by(saison, mois_jour) %>%
  summarise(nb_decla = n(), .groups = "drop")


      # Grille complète (TOUTES les cases jour × saison)
grille <- expand.grid(saison = unique(ech$saison),
  mois_jour = unique(format(ech$date_la_plus_tot, "%m-%d")))

cal <- grille %>%
  left_join(data_p, by = c("saison", "mois_jour")) %>%
  mutate(nb_decla = ifelse(is.na(nb_decla), 0, nb_decla))

      # Ordre chronologique (Décembre → Novembre)
cal <- cal %>%
  mutate(mois = as.integer(substr(mois_jour, 1, 2)),
         jour = as.integer(substr(mois_jour, 4, 5)),
         mois_tri = ifelse(mois == 12, 0, mois),
         ordre = mois_tri * 100 + jour)

      # Facteur ordonné
cal$mois_jour <- factor(cal$mois_jour,
                        levels = cal %>%
    arrange(ordre) %>%
    pull(mois_jour) %>%
    unique())

      # Labels seulement le 1 et le 15
labels_x <- ifelse(substr(levels(cal$mois_jour),4,5) %in% c("01","15"),
                   levels(cal$mois_jour), "")

      # Heatmap finale avec intensité selon nombre de déclarations

ggplot(cal, aes(x = mois_jour, y = saison, fill = nb_decla)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c("gray95", "gold", "orange", "red", "purple", "darkblue"),
    values = scales::rescale(c(0, 2, 5, 8, 11, 15)),
    limits = c(0,15),
    breaks = c(0,2,5,8,11,15),
    name = "Nombre\n de déclarations")+
  scale_x_discrete(labels = labels_x) +
  labs(x = "Jour - Mois",
       y = "Saison",
       title = "Calendrier des déclarations - Tous pêcheurs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        axis.text.y = element_text(size = 10))
