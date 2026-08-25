# Script Action 03 : Application des critères de sélection de l'échantillon
# Critères présentées dans le rapport :

  # - 1. Importation des données
  # - 2. Moyenne par saison
  # - 3. Nombre de saison par pêcheur
  # - 4. Captures par marée
  # - 5. comparaison titulaire licence

# Auteur : Lola
# date : 02/06/2026

# Packages 
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)

  # 1. Importation des données ####
        # Données pêcheurs
sacrois <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/sacrois_2016-2025_licence_combo_ready.csv")
tecap <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/telecapeche_2014-2025_navire.csv")
fluv <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/3pecheurs_fluviaux_2013-2025.csv")
fluv <- fluv %>% select(!X)
        # LIC
lic_maritime <- read_excel("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/Données RNN_Baie Aiguillon_LIC.xlsx",
                  sheet = "LIC2")
lic_fluv <- read_excel("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/LIC Fluviaux 44 - 2013-2026.xlsx")

        # Adhérent centre de stockage
centre_stockage <- read_excel("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/2026 Adhérents SAS.xlsx", sheet = "Feuil1")

# Tous les pêcheurs présent dans les données détiennent forcément une licence CMEA

  # Critère 1 : Adhérent au centre de stockage ####
adherent_stockage <- unique(centre_stockage$pecheur)

ech1 <- sacrois %>%
  select(!X) %>%
  filter(pecheur %in% adherent_stockage |
           pecheur %in% c("JOSNIN Franck", "LERAY Michel"))

# Pas besoin de filtrer pour les fluviaux car il s'agit déjà de 3 pêcheurs adhérent
# au centrre de stockage

  # Critère 2 : Activité menée sur la Loire ####
tec1 <- tecap %>%
  select(!X) %>%
  filter(pecheur %in% ech1$pecheur)
# regarder s'il pêche bien tous en Loire
unique(tec1$nom_zone)
# "Loire"          "Loire Lot n°15" "Loire Lot n°13" "Loire Lot n°14"
# C'est bon 

unique(fluv$nom_zone)
# "Zone maritime"       "Loire Lot n°15"      "Loire Lot n°13"     
# "Loire Lot n°14"      "Lots Sèvre Nantaise" "44_L13"             
# "44_L14"              "44_L15"              "44_S6"              
# "44_S67"

  # Critère 3 : 5 Saisons Minimum ####
ech_saison <- ech1 %>%
  group_by(pecheur) %>%
  summarise(nb_saison = n_distinct(saison),
            .groups = "drop") %>%
  filter(nb_saison >=5)

fluv_saison <- fluv %>%
  group_by(pecheur) %>%
  summarise(nb_saison = n_distinct(saison),
            .groups = "drop") %>%
  filter(nb_saison >=5) #Les 3 pêcheurs ont au moins 12 saisons à leur actif

ech2 <- ech1 %>%
  filter(pecheur %in% ech_saison$pecheur) %>%
  filter(!(pecheur == "VALLEE Mickaël" & saison == "2018-2019" & poids >= 20))

  # Nombre de marée minimum <- a titre informatif ####
ech3 <- ech2 %>%
  group_by(pecheur, saison) %>%
  summarise(nb_maree = n_distinct(date),
            .groups = "drop")

tec3 <- tec1 %>%
  group_by(pecheur, saison) %>%
  summarise(nb_maree = n_distinct(date),
            .groups = "drop")

fluv3 <- fluv %>%
  group_by(pecheur, saison) %>%
  summarise(nb_maree = n_distinct(date),
            .groups = "drop")

  # Critère 4 : Comparaison entre sacrois et télécapêche ####
adherent_saison <- tec1 %>%
  select(saison, pecheur, adherent) %>%
  distinct()

adherent_saison %>%
  count(saison, pecheur) %>%
  filter(n > 1)

ech2 <- ech2 %>%
  left_join(adherent_saison, by = c("saison", "pecheur"))

ech_somme <- ech2 %>%
  group_by(pecheur, saison, adherent) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(lic_maritime %>%
              select(saison = `Saison de pêche`, LIC_horsOP_total, LIC_OPEstuaires_total) %>%
              distinct(),
            by = "saison") %>%
  mutate(source = "sacrois")

tec_somme <- tec1 %>%
  group_by(pecheur, saison, adherent) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(lic_maritime %>%
              select(saison = `Saison de pêche`, LIC_horsOP_total, LIC_OPEstuaires_total) %>%
              distinct(),
            by = "saison") %>%
  mutate(source = "télécapêche")

somme_tot <- bind_rows(ech_somme, tec_somme)

g1 <- ggplot(somme_tot, aes(x = saison, y = poids_tot, colour = pecheur,
                            linetype = source,
                            group = interaction(pecheur, source))) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = LIC_horsOP_total, group = 1), color = "red", linewidth = 0.8) +
  geom_line(aes(y = LIC_OPEstuaires_total, group = 1), color = "green", linewidth = 0.8) +
  scale_linetype_manual(values = c("sacrois" = "solid",
                                   "télécapêche" = "dashed")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(g1)

  # 7. Comparaison au LIC total ####
ech4 <- ech2 %>%
  group_by(saison, pecheur, adherent) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE), 
            .groups = "drop") %>%
  left_join(lic_maritime %>%
              select(saison = `Saison de pêche`, LIC_horsOP_total, LIC_OPEstuaires_total) %>%
              distinct(),
            by = "saison") %>%
  mutate(diff_lic = case_when(
      adherent == "OP" ~ poids_tot - LIC_OPEstuaires_total,
      adherent == "hors OP" ~ poids_tot - LIC_horsOP_total,
      TRUE ~ NA_real_))
head(ech4)

g2 <- ggplot(ech4, aes(x = saison, y = poids_tot, group = pecheur)) +
  geom_point(aes(color = adherent)) +
  scale_color_manual(values = c("OP" = "blue",
                                "hors OP" = "red")) +
  #geom_line(aes(color = pecheur), alpha = 0.3) +
  geom_line(
    aes(y = LIC_horsOP_total, group = 1),
    color = "red",
    linewidth = 1.2) +
  geom_line(aes(y = LIC_OPEstuaires_total, group = 1),
            color = "skyblue",
            linewidth = 1.2) +
  geom_line(aes(y = LIC_OPEstuaires_total + 30, group = 1),
            color = "green",
            linewidth = 1.2) +
  labs(title = "Sacrois") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tec4 <- tec1 %>%
  filter(pecheur %in% ech2$pecheur,
         saison %in% ech2$saison) %>%
  group_by(saison, pecheur, adherent) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE), 
            .groups = "drop") %>%
  left_join(lic_maritime %>%
              select(saison = `Saison de pêche`, LIC_horsOP_total, LIC_OPEstuaires_total) %>%
              distinct(),
            by = "saison") %>%
  mutate(diff_lic = round(case_when(
    adherent == "OP" ~ poids_tot - LIC_OPEstuaires_total,
    adherent == "hors OP" ~ poids_tot - LIC_horsOP_total,
    TRUE ~ NA_real_),2))


g3 <- ggplot(tec4, aes(x = saison, y = poids_tot, group = pecheur)) +
  geom_point(aes(color = adherent)) +
  scale_color_manual(values = c("OP" = "blue",
                                "hors OP" = "red")) +
  #geom_line(aes(color = pecheur), alpha = 0.3) +
  geom_line(
    aes(y = LIC_horsOP_total, group = 1),
    color = "red",
    linewidth = 1.2) +
  geom_line(aes(y = LIC_OPEstuaires_total, group = 1),
            color = "skyblue",
            linewidth = 1.2) +
  labs(title = "Télécapêche") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(g2, g3)
# depassement lic important
weird <- ech4 %>%
  filter(diff_lic > 50)

        # Pour les fluviaux 
fluv4 <- fluv %>%
  group_by(saison, pecheur) %>%
  summarise(poids_tot = sum(poids, na.rm = TRUE), 
            .groups = "drop")

lic_fluv_tot <- lic_fluv %>%
  mutate(lic_tot_op = (OP_conso + OP_repeup),
         lic_tot_hop = (HOP_conso + HOP_repeup)) %>%
              select(saison, lic_tot_op, lic_tot_hop)

g4 <- ggplot(fluv4, aes(x = saison, y = poids_tot, group = 1, color = pecheur)) +
  geom_point() +
  geom_line() +
  geom_line(data = lic_fluv_tot, aes(x = saison, y = lic_tot_op), color = "skyblue", linewidth = 1.2) +
  geom_line(data = lic_fluv_tot, aes(x = saison, y = lic_tot_hop), color = "red", linewidth = 1.2) +
  labs(title = "Fluviaux") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(g4)

  # 8. 80% du LIC de consommation - Télécapêche ####
tec2 <- tec1 %>%
  filter(pecheur %in% ech2$pecheur,
         saison %in% ech2$saison,
         destination == "C") %>%
  group_by(saison, pecheur, adherent) %>%
  summarise(poids_conso = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(lic_maritime %>%
              select(saison = `Saison de pêche`, LIC_horsOP_Conso = LIC_horsOP_Consommation, LIC_OP_Conso = `LIC_OPEstuaires-Consommation`) %>%
              distinct(),
            by = "saison") %>%
  mutate(conso_80 = case_when(
    adherent == "OP" ~ LIC_OP_Conso * 0.8,
    adherent == "hors OP" ~ LIC_horsOP_Conso * 0.8,
    TRUE ~ NA_real_),
    diff_conso_80 = poids_conso - conso_80,
    diff_lic = round(case_when(
    adherent == "OP" ~ poids_conso - LIC_OP_Conso,
    adherent == "hors OP" ~ poids_conso - LIC_horsOP_Conso,
    TRUE ~ NA_real_),2))

conso_80_pas_atteint <- tec2 %>%
  filter(diff_conso_80 < 0)

  # 9. 80% du LIC de consommation - Fluviaux ####
fluv5 <- fluv %>%
  filter(destination == "C") %>%
  group_by(saison, pecheur) %>%
  summarise(poids_conso = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(lic_fluv %>%
              select(saison, OP_conso , HOP_conso) %>%
              distinct(), by = "saison") %>%
  mutate(conso_80_op = OP_conso * 0.8,
         conso_80_hop = HOP_conso * 0.8,
         diff_conso_80_op = poids_conso - conso_80_op,
         diff_conso_80_hop = poids_conso - conso_80_hop) %>%
  select(saison, pecheur, poids_conso, OP_conso, conso_80_op, diff_conso_80_op, HOP_conso, conso_80_hop, diff_conso_80_hop)

conso_80_pas_atteint_fluv <- fluv5 %>%
  filter(diff_conso_80_op < 0 | diff_conso_80_hop < 0)

  # 10. 50% du LIC de repeuplement - Télécapêche ####
tec5 <- tec1 %>%
  filter(pecheur %in% ech2$pecheur,
         saison %in% ech2$saison,
         destination == "R") %>%
  group_by(saison, pecheur, adherent) %>%
  summarise(poids_rep = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(lic_maritime %>%
              select(saison = `Saison de pêche`, LIC_horsOP_Repeup = LIC_horsOP_Repeuplement, LIC_OP_Repeup = LIC_OPEstuaires_Repeuplement) %>%
              distinct(),
            by = "saison") %>%
  mutate(rep_50 = case_when(
    adherent == "OP" ~ LIC_OP_Repeup * 0.5,
    adherent == "hors OP" ~ LIC_horsOP_Repeup * 0.5,
    TRUE ~ NA_real_),
    diff_rep_50 = poids_rep - rep_50,
    diff_lic = round(case_when(
    adherent == "OP" ~ poids_rep - LIC_OP_Repeup,
    adherent == "hors OP" ~ poids_rep - LIC_horsOP_Repeup,
    TRUE ~ NA_real_),2))

repeup_50_pas_atteint <- tec5 %>%
  filter(diff_rep_50 < 0)

  # 11. 50% du LIC de repeuplement - Fluviaux ####
fluv6 <- fluv %>%
  filter(destination == "R") %>%
  group_by(saison, pecheur) %>%
  summarise(poids_repeup = sum(poids, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(lic_fluv %>%
              select(saison, OP_repeup , HOP_repeup) %>%
              distinct(), by = "saison") %>%
  mutate(repeup_50_op = OP_repeup * 0.5,
         repeup_50_hop = HOP_repeup * 0.5,
         diff_repeup_50_op = poids_repeup - repeup_50_op,
         diff_repeup_50_hop = poids_repeup - repeup_50_hop) %>%
  select(saison, pecheur, poids_repeup, OP_repeup, repeup_50_op, diff_repeup_50_op, HOP_repeup, repeup_50_hop, diff_repeup_50_hop)

repeup_50_pas_atteint_fluv <- fluv6 %>%
  filter(diff_repeup_50_op < 0 | diff_repeup_50_hop < 0)

#writexl::write_xlsx(ech2, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/Echantillon_5saisons_sacrois2.xlsx")
#writexl::write_xlsx(tec1, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/Echantillon/Echantillon_5saisons_tecap2.xlsx")

### Fin ###

# Les pêcheurs répondant aux critère sont selectionnée grâce à ce script
# cependant, la vrai mise en forme des données de ces pêcheurs ne sont pas automatisée
# et est faite manuellement sur excel. 