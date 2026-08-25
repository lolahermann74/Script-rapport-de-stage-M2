# Script Action 01_01 : Prétraitement des données (Nettoyage) SACROIS : 
  # - 1. Conversion des formats
  # - 2. Gestion des valeurs manquantes 
  # - 3. Sélection du stade civelle
  # - 4. Ajout de la colonne saison
  # - 5. Sélection des saisons d'intérêt
  # - 6. Suppression des navires en doubles du jeu de données SACROIS
  #     --> Lorsque un même immat à différent nom de navire soit à cause d'erreur 
  #         de frappe, soit car les données ne sont pas mis à jour lorsque le navire
  #         change de nom au cours du temps etc
  # - 7. Remettre les bons quartiers maritime
  # - 8. Supression des lignes doublons
  # - 9. SACROIS version final : Exportation
  # - 10. Ajout licence

# Auteur : Lola 
# Date : 24/03/2026
# Dernière modif : 04/05/2026

# Packages
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(readxl)
library(writexl)
library(ggplot2)

# SACROIS ####
  # Importation des données 
sacrois <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/data/Data_SACROIS_Anguille_COREPEM_2008_2025.csv", fileEncoding = "UTF-8")

  # 1. Conversion des formats ####
sacrois <- sacrois %>%
  mutate(date = as.Date(date),
         annee = an_ref,
         poids = as.numeric(debarquement_Kg),
         prix = as.numeric(debarquement_euros))

  # 2. Gestion des valeurs manquantes ####
    # Cellule vide remplacée par NA
sacrois <- sacrois %>%
  mutate(across(where(is.character), ~ na_if(., "")))

    # Stade espèce non spécifié
      # Ajout de la variable prix au kilo
sacrois <- sacrois %>%
  mutate(prix_au_kg = prix / poids)

      # Ajout variable saison dans un data test
S2 <- sacrois %>%
  mutate(saison = if_else(
    month(date) >= 11,
    paste0(year(date), "-", year(date) + 1),
    paste0(year(date) - 1, "-", year(date))))

        # Graphique 1 : Prix au kilo par saison de S2
ggplot(S2, aes(x = saison, y = prix_au_kg, color = stade_anguille_cod)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = 80, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = "2011-2012", y = 80, label = "80 €/kg", color = "red", hjust = 1.1, vjust = -0.5) +
  scale_color_manual(values = c("AJA" = "yellow",
                                "AAR" = "green",
                                "ACI" = "blue",
                                "NA"  = "#D3D3D34D")) +
  labs(title = "Nuage de points : Prix au kilo par saison",
       x = "Saison",
       y = "Prix au kilo (€)",
       color = "Stade anguille") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"))

        # Premier test avec une limite à 80€ le kilo
S3 <- S2 %>%
  mutate(stade_anguille_cod = case_when(
    is.na(stade_anguille_cod) & prix_au_kg >= 80 ~ "ACI",
    TRUE ~ stade_anguille_cod),
    stade_anguille_lib = case_when(
      is.na(stade_anguille_lib) & prix_au_kg >= 80 ~ "Civelle",
      TRUE ~ stade_anguille_lib))

        # Graphique 2 : Prix au kilo par saison avec nouvelle limite
ggplot(S3, aes(x = saison, y = prix_au_kg, color = stade_anguille_cod)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = 80, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = "2011-2012", y = 80, label = "80 €/kg", color = "red", hjust = 1.1, vjust = -0.5) +
  scale_color_manual(values = c("AJA" = "yellow",
                                "AAR" = "green",
                                "ACI" = "blue",
                                "NA"  = "#D3D3D34D")) +
  labs(title = "Nuage de points : Prix au kilo par saison",
       x = "Saison",
       y = "Prix au kilo (€)",
       color = "Stade anguille") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"))

        # Regarder les individus entre 40 et 80€ le kilo qui peuvent poser des doutes
S4 <- S3 %>%
  filter(prix_au_kg >= 40 & prix_au_kg <= 80) %>%
  mutate(mois = month(date, label = TRUE, abbr = FALSE))

        # Après vérification des déclaration qui posaient encore un doute,
        # la limite est baissé à 60€ le kilo 
sacrois <- sacrois %>%
  mutate(stade_anguille_cod = case_when(
    is.na(stade_anguille_cod) & prix_au_kg >= 60 ~ "ACI",
    TRUE ~ stade_anguille_cod),
    stade_anguille_lib = case_when(
      is.na(stade_anguille_lib) & prix_au_kg >= 60 ~ "Civelle",
      TRUE ~ stade_anguille_lib))

  # 3. Sélection du stade civelle ####
sacrois <- sacrois %>%
  filter(stade_anguille_cod == "ACI",
         stade_anguille_lib == "Civelle")

    # Vérification des mois --> la pêcherie de la civelle est autorisé de novembre à fin avril
sacrois <- sacrois %>%
  mutate(mois = month(date, label = TRUE, abbr = FALSE))

unique(sacrois$mois)

    # Filtration des mois qui ne devrait pas être présent puis regarder les années
S5 <- sacrois %>%
  filter(mois %in% c("septembre", "octobre", "juin", "juillet", "août")) %>%
  select(nom_navire, poids, prix, date, mois, an_ref, stade_anguille_cod, prix_au_kg)
unique(S5$an_ref)

    # Regarder pour le mois de mai qui était autorisé au début puis interdit par suite (à partir de 2023)
S6 <- sacrois %>%
  filter(mois == "mai") %>%
  select(nom_navire, poids, prix, date, mois, an_ref, stade_anguille_cod, prix_au_kg)
unique(S6$an_ref)

    # Supprimer les déclarations de ces mois là dans le jeu de données SACROIS. 
sacrois <- sacrois %>%
  filter(!mois %in% c("septembre", "octobre", "novembre", "mai", "juin", "juillet", "août"))


  # 4. Ajout de la colonne saison ####
sacrois <- sacrois %>%
  mutate(saison = if_else(
    month(date) >= 12,
    paste0(year(date), "-", year(date) + 1),
    paste0(year(date) - 1, "-", year(date))))

  # 5. Sélection des saisons d'intérêt ####
unique(sacrois$saison)

sacrois <- sacrois %>%
  filter(!saison %in% c("2007-2008", "2008-2009", "2009-2010", "2015-2016", "2025-2026"))

unique(sacrois$saison)

  # 6. Supression des navires doublons ####
    # Ajout de la colonne immat
sacrois <- sacrois %>%
  mutate(immat = str_sub(cfr_cod, -6, -1),
         quartier = trimws(quartier)) %>%
  select(annee, mois, saison, date, quartier, immat, nom_navire, poids, stade_anguille_cod)

    # Regarder les noms de navire qui sont en double
navire_doublon <- sacrois %>%
  group_by(immat) %>%
  summarise(nb_navires = n_distinct(nom_navire),
            noms = paste(unique(nom_navire), collapse = ", ")) %>%
  filter(nb_navires > 1) %>%
  mutate(immat = iconv(immat, from = "", to = "UTF-8", sub = ""),
         noms = iconv(noms, from = "", to = "UTF-8", sub = ""))
    # Chercher dans les base de données mirage et fleet register pour attribuer 
    # le bon nom de bateau. Sinon garder seulement un des deux noms et si besoin
    # enlever les lignes dupliquer.

    # Filtrer les navires qui présente des doublons
SACROIS_doublons <- sacrois %>%
  filter(immat %in% navire_doublon$immat)

    # Faire la sommes des captures totales par saison
df_sum <- SACROIS_doublons %>%
  group_by(saison, immat, nom_navire) %>%
  summarise(poids_total = sum(poids, na.rm = TRUE), .groups = "drop") %>%
  mutate(saison = factor(saison, levels = unique(saison)))

check <- df_sum %>%
  group_by(immat, saison) %>%
  summarise(n_poids = n_distinct(poids_total), .groups = "drop")

    # Séparer les navires en 2 catégories : 
      # - Ceux dont le total de capture par saison est le même 
      # - Ceux dont le total de capture par saison est différent (souvent le double)
immat_ok <- check %>%
  group_by(immat) %>%
  summarise(all_same = all(n_poids == 1)) %>%
  filter(all_same) %>%
  pull(immat)


immat_diff <- check %>%
  group_by(immat) %>%
  summarise(any_diff = any(n_poids > 1)) %>%
  filter(any_diff) %>%
  pull(immat)

df_same <- df_sum %>%
  filter(immat %in% immat_ok)

df_diff <- df_sum %>%
  filter(immat %in% immat_diff)

    # Vérifier qu'un immat ne se retrouve pas dans les 2 tableaux (doit retourner character(0))
intersect(df_same$immat, df_diff$immat)

unique(df_same$immat)
unique(df_diff$immat)


    # savoir quel est le facteur de différence entre les navires de même immat qui
    # n'ont pas le même total de capture par saison donc rajouter un colonne facteur. 
    # On s'attends à ce que ce soit 2 pour la plupart.
df_diff2 <- df_diff %>%
  group_by(immat, saison) %>%
  mutate(poids_ref = min(poids_total, na.rm = TRUE),
         facteur = poids_total / poids_ref) %>%
  ungroup()


df_x2 <- df_diff2 %>%
  filter(facteur == 2)
unique(df_x2$nom_navire)

    # Une fois tous les navires lister regarder dans mirage et dans fleet register 
    # le bon nom de bateau (voir tableau excel vrai_navire.xlsx)
    # Puis enlever les bâteaux de SACROIS qui sont "faux"

SACROIS_clean <- sacrois %>%
  filter(!(immat == "198107" & nom_navire == "CARRE D'AS"),
         
         !(immat == "222427" & nom_navire == "ANAIS-TIFFANY"),
         !(immat == "226043" & nom_navire == "LE FORBAN II"),
         !(immat == "288029" & nom_navire == "BOSCO"),
         !(immat == "292551" & nom_navire == "MICHEL COLETTE"),
         !(immat == "293421" & nom_navire == "WENDY-SHEAN"),
         
         !(immat == "329186" & nom_navire%in% c("JUNIOR II", "JUNIOR ")),
         !(immat == "329195" & nom_navire == "LA TOISON D'OR"),
         !(immat == "329320" & nom_navire == "BENJY YOMI"),
         !(immat == "343451" & nom_navire%in% c("CREAC", "LE P'TIT NANA")),
         
         !(immat == "429123" & nom_navire == "MANU-REVA"),
         !(immat == "476284" & nom_navire == "C'REPARTI"),
         !(immat == "490147" & nom_navire == "CELIA"),
         !(immat == "492966" & nom_navire%in% c("LE PTIT VIAUD", 
                                                "LE P'TIT VIAUD", "HELINE")),
         
         !(immat == "510765" & nom_navire == "BILOU II"),
         !(immat == "511517" & nom_navire == "LE PETREL"),
         !(immat == "517999" & nom_navire%in% c("LE LOUP BLANC 2", "LE LOUP BLANC II")),
         !(immat == "522609" & nom_navire == "CAM BOT"),
         !(immat == "536336" & nom_navire == "BICHNOUCK"),
         !(immat == "536354" & nom_navire == "P'TIT BISON"),
         !(immat == "546973" & nom_navire == "VENUS"),
         !(immat == "560227" & nom_navire == "P'TIT THOMAS"),
         !(immat == "561260" & nom_navire == "L'ABRI C TIER"),
         !(immat == "571469" & nom_navire == "ANATIFE"),
         !(immat == "577920" & nom_navire == "AR DIWALL"),
         !(immat == "593846" & nom_navire%in% c("EXOTIC", "ENFANTS DES FLOTS")),
         
         !(immat == "618660" & nom_navire == "FRANCK STENN"),
         !(immat == "635662" & nom_navire == "BRISES LAMES"),
         !(immat == "635716" & nom_navire == "DEBORAH II"),
         !(immat == "638931" & nom_navire == "PASSE-TEMPS"),
         !(immat == "639186" & nom_navire == "ETOILE FILANTE II"),
         !(immat == "639735" & nom_navire == "VILLE DE ST BREVIN"),
         !(immat == "648730" & nom_navire%in% c("DAVY", "DEN HELIGA")),
         !(immat == "661265" & nom_navire == "LA SANTA MARIA"),
         !(immat == "661455" & nom_navire == "C REPARTI"),
         !(immat == "671994" & nom_navire == "L'ESTRELLITA"),
         !(immat == "684262" & nom_navire == "LE FORBAN"),
         !(immat == "686349" & nom_navire == "TROIS MA"),
         !(immat == "687728" & nom_navire == "TZIGANE"),
         !(immat == "689866" & nom_navire == "LE REBELLE"),
         !(immat == "690814" & nom_navire == "BALBUZARD"),     
         !(immat == "691653" & nom_navire == "IRIS "),
         !(immat == "698058" & nom_navire%in% c("LA CHOCHOTE II", "LA CHOCHOTTE II")),
         
         !(immat == "707261" & nom_navire%in% c("LAURALEX", "P'TIT PALUCHE")),
         !(immat == "707398" & nom_navire%in% c("CHRISTAL II", "L'ODYSEE")),
         !(immat == "707485" & nom_navire == "ROULE TA BILLE"),
         !(immat == "721070" & nom_navire == "LA  FURIEUSE"),
         !(immat == "721350" & nom_navire%in% c("NON NON", "NONNON")),
         !(immat == "726675" & nom_navire == "FOLLY FOOT"),
         !(immat == "726954" & nom_navire == "CARRE D'AS"),
         !(immat == "750731" & nom_navire == "AR MEN"),
         !(immat == "759920" & nom_navire == "VIRGO-SALUTARIS II"),
         
         !(immat == "847352" & nom_navire == "ETRILLE"),
         !(immat == "848804" & nom_navire == "BORD LOIN"),
         !(immat == "867725" & nom_navire == "CA DEROUALLE II"),
         !(immat == "869916" & nom_navire%in% c("L'OISEAU BLEU", "CARRE D'AS")),
         !(immat == "886574" & nom_navire == "DESTIN"),
         !(immat == "886580" & nom_navire == "FEE MORGANE"),
         
         !(immat == "907812" & nom_navire == "NAIADE"),
         !(immat == "916001" & nom_navire%in% c("LEA FLORA", "LEA FLORE")),
         !(immat == "916006" & nom_navire == "L'OISEAU DES ISLES II"),
         !(immat == "916067" & nom_navire == "MUSTANG II"),
         !(immat == "916068" & nom_navire == "AR PEOCH II"),
         !(immat == "918284" & nom_navire == "LE KOALA II"),
         !(immat == "918524" & nom_navire == "GALEJEUR"),
         !(immat == "918536" & nom_navire == "LA POLONAISE II"),
         !(immat == "920411" & nom_navire == "LA BROCHE 1"),
         !(immat == "922177" & nom_navire == "LE SAGITTAIRE"),
         !(immat == "922225" & nom_navire == "LA RASCASSE"),
         !(immat == "922241" & nom_navire == "BALBAYA"),
         !(immat == "922248" & nom_navire == "LE CORSAIRE"),
         !(immat == "922384" & nom_navire == "MARIA LUCAS"),
         !(immat == "925369" & nom_navire == "ISALIA"),
         !(immat == "925408" & nom_navire == "ROYAL"),
         !(immat == "926777" & nom_navire == "ELDORADO PIERO"),
         !(immat == "930478" & nom_navire == "DEN HELIGA  II")) %>%
  
  mutate(nom_navire = case_when(
    immat == "522609" & nom_navire == "CAM'BOT" ~ "CA M'BOT",
    immat == "671994" & nom_navire == "R VE DE GOSSE" ~ "REVE DE GOSSE",
    immat == "510796" & nom_navire == "L'AMI DU P CHEUR" ~ "L'AMI DU PECHEUR",
    immat == "571241" & nom_navire == "R VE D'ADO" ~ "REVE D'ADO",
    TRUE ~ nom_navire))

uniquenom <- SACROIS_clean %>%
  distinct(immat, nom_navire)

    # Vérifier comme précédemment s'il n'y a plus de doublon dans les navires
navire_doublon2 <- SACROIS_clean %>%
  group_by(immat) %>%
  summarise(nb_navires = n_distinct(nom_navire),
            noms = paste(unique(nom_navire), collapse = ", ")) %>%
  filter(nb_navires > 1)

    # test 1 navire
cotier <- SACROIS_clean %>%
  filter(immat == "561260",
         saison =="2016-2017") %>%
  group_by(nom_navire) %>%
  summarise(tot = sum(poids, na.rm = TRUE),
            .groups = "drop")

  # 7. Remettre les bons quartiers maritime après vérification dans la base de données interne au COREPEM ####
SACROIS_clean2 <- SACROIS_clean %>%
  mutate(quartier = case_when(
    immat %in% c("292551", "925083", "547407", "839511", "689360", "650742",
                 "285047", "925411", "713326", "606994", "565103", "723419") ~ "NA",
    immat == "198107" & saison %in% c("2016-2017", "2017-2018", "2018-2019",
                                      "2019-2020")~ "NO",
    immat == "198107" & saison %in% c("2020-2021", "2021-2022", "2022-2023", 
                                      "2023-2024", "2024-2025")~ "NA",
    immat%in% c("473574", "274034", "650436")~ "SN",
    immat == "886580" & saison %in% c("2016-2017", "2017-2018", "2018-2019",
                                      "2019-2020","2020-2021", "2021-2022")~ "NO",
    immat == "886580" & saison %in% c("2022-2023", "2023-2024", "2024-2025")~ "SN",
    immat == "639186" ~ "LS",
    TRUE ~ quartier)) %>%
  filter(!quartier%in% c("VA", "LR")) # Quartier maritime hors Pays-de-la-Loire

  # 8. Supression des lignes doublons ####
SACROIS_clean3 <- unique(SACROIS_clean2)

  # 8.2. Vérification si déclaration en double
S7 <- SACROIS_clean3 %>% filter(is.na(quartier))

nav_captures_double <- unique(S7$immat)

S8 <- SACROIS_clean3 %>% filter(immat%in% nav_captures_double)

quartier_double <- SACROIS_clean3 %>%
  group_by(saison, immat) %>%
  summarise(n_quartiers = n_distinct(quartier),
            quartiers = paste(sort(unique(quartier)), collapse = ", "),
    .groups = "drop") %>%
  filter(n_quartiers > 1)

  # 9. SACROIS version final ####
SACROIS <- SACROIS_clean3 %>%
  select(annee, mois, saison, date, quartier, immat, nom_navire, poids, stade_anguille_cod)
      
        # Exportation
write.csv(SACROIS, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/SACROIS_Civelle_2016_2025.csv")

  # 10. Ajout licence et pecheur ####
    # Importation des licences CMEA
licences_corepem <- read.csv("C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/Table_licenceCMEA_COREPEM_2014-2025.csv")

licences_corepem <- licences_corepem %>%
  select(!X) %>%
  mutate(immat = as.character(immat)) %>%
  filter(!(saison%in%c("2014-2015", "2015-2016")))

    # Ajout pecheur à sacrois avec licence_corepem
sac_licences <- SACROIS %>%
  left_join(licences_corepem %>% 
              select(saison, immat, immat_licence, pecheur),
            by = c("saison", "immat")) %>%
  mutate(licence = ifelse(!is.na(immat_licence), "yes", "no"))

navires <- sac_licences %>%
  distinct(saison, immat, nom_navire)

licence_navires <- licences_corepem %>%
  left_join(navires, by = c("saison", "immat", "nom_navire")) %>%
  group_by(saison, pecheur) %>%
  summarise(immat_final = paste(unique(immat), collapse = "_"),
            nom_navire = paste(unique(na.omit(nom_navire)), collapse = "_"),
            .groups = "drop")

licences_final <- sac_licences %>%
  left_join(licence_navires,
            by = c("saison", "pecheur")) %>%
  mutate(navire_combo = nom_navire.y,
         nom_navire = nom_navire.x,
         immat_combo = immat_final) %>%
  select(annee, mois, saison, date, immat_combo, navire_combo, pecheur, poids, immat, nom_navire, quartier, stade_anguille_cod)

write.csv(licences_final, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/sacrois_2016-2025_licence_combo.csv")
write.csv(licence_navires, "C:/Users/Stagiaire/Desktop/LOLA/STAGE/R/output/licence_navire_combo.csv")


### FIN ####