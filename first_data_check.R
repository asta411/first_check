
rm(list = ls())
library(MASS)
library(tidyverse)
library(readxl)
library(readr)
library(gtsummary)
library(gt)
library(ggnetwork)
library(data.table)
library(ggpubr)
library(pscl)
library(stringr)
library(gitcreds)
library(ResourceSelection)
library(ggplot2)
library(lubridate)
library(dplyr)

colonisation <- read_delim("COLONISATION.txt", delim = "#", col_names = TRUE)
infection <- read_delim("INFECTION.txt", delim = "#", col_names = TRUE)
sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)


resultat <- read_excel("resultat.xlsx", col_names = TRUE)
resultat_autre <- read_excel("resultat_autre.xlsx", col_names = TRUE)
campagne <- read_excel("campagne.xlsx", col_names = TRUE)
chambre <- read_excel("chambre.xlsx", col_names = TRUE)

rea_data <- read_excel("TraPRea-Data_service-Fin.xlsx", col_names=TRUE)
summary(sejour)




hist(sejour$PATAGE, 
     main = "Distribution of Patient Ages", 
     xlab = "Age of Patients", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black")


png("Patient_Age_Distribution.png")
hist(sejour$PATAGE, main = "Distribution of Patient Ages", xlab = "Age of Patients", col = "skyblue", border = "black")
dev.off()


# Transforming campagne into long format to have jsut column of ID and column of encensement and the appareil that is considered in the study
#prise en compte de la colomne encensement seulement pas la premiere
campagne_long <- campagne %>%
  # Reshape ENV* columns into APP groups
  pivot_longer(
    cols = matches("^ENV"), 
    names_to = c("APP", ".value"),
    names_pattern = "ENV(PEC|PRA|PEL|PLE)(C|E)"
  ) %>%
  # Rename columns and format APP
  rename(CODE = C, ENC = E) %>%
  mutate(
    APP = case_when(
      APP == "PEC" ~ 1L,
      APP == "PRA" ~ 2L,
      APP == "PEL" ~ 3L,
      APP == "PLE" ~ 4L,
      TRUE ~ NA_integer_
    )
  ) %>%
  # Select and order final columns
  select(CAMID, CAMNUM, CAMDAT, CAMSEC, APP, CODE, ENC)

# Remove rows with missing CODE/ENC if needed
campagne_long <- campagne_long %>% filter(!is.na(APP))

#transforming chambre into long format

chambre_long <- chambre %>%
mutate(across(matches("ENV.*E"), as.character) #Non fait converti en NA et pas gardé tel quel 
  ) %>%
  pivot_longer(
    cols = matches("^ENV"),
    names_to = c("APP", ".value"),
    names_pattern="ENV(PPO|PSC|PRE|PPA|PAD|PSI|PFA)(C|E)"
  ) %>%
  rename(CODE = C, ENC = E) %>%
  mutate(
    #ENC = na_if(ENC, ""), 
    APP = case_when(
      APP == "PPO" ~ 5L,
      APP == "PSC" ~ 6L,
      APP == "PRE" ~ 7L,
      APP == "PPA" ~ 8L,
      APP == "PAD" ~ 9L,
      APP == "PSI" ~ 10L,
      APP == "PFA" ~ 11L,
      TRUE ~ NA_integer_
    )
  ) %>%
  # Select and order final columns
  select(CHAID, CHANUM, CAMID, APP, CODE, ENC)
  
chambre_long <- chambre_long %>% filter(!is.na(APP))


#Graphe de la durée de séjour de chaque patient de l'étude à l'hopital

sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)

sejour <- sejour %>%
  mutate(
    REAENT = dmy(REAENT),
    FINDAT = dmy(FINDAT),
    Duree = as.numeric(FINDAT - REAENT)
  )


ggplot(
  data = subset(sejour, !is.na(REAENT) & !is.na(FINDAT)),  # Exclusion graphique uniquement
  aes(y = reorder(SUBJID, REAENT))
) +
  geom_linerange(
    aes(xmin = REAENT, xmax = FINDAT, color = Duree),
    linewidth = 1.5
  ) +
  scale_x_date(
    date_breaks = "1 week", 
    date_labels = "%d %b",
    limits = c(
      min(sejour$REAENT, na.rm = TRUE),  # Min avec gestion des NA
      max(sejour$FINDAT, na.rm = TRUE)    # Max avec gestion des NA
    )
  ) +
  scale_color_gradient(
    low = "#FF6F61", 
    high = "#6B5B95",
    name = "Durée (jours)"
  ) +
    theme_minimal(base_size = 12)
#reprise du code:graphe secteur Flamboyant avec prise ou non d'antibiotiques 

# Filtrer les patients du secteur Flamboyant
sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)
sejour_flamboyant <- sejour %>%
  filter(REASEC1 == "Flamboyant") %>%
  mutate(
    REAENT = dmy(REAENT),
    FINDAT = dmy(FINDAT),
    Duree = as.numeric(FINDAT - REAENT)
  )

# Calcul de la date maximale pour positionner les indicateurs antibiotiques
max_date <- max(sejour_flamboyant$FINDAT, na.rm = TRUE) + days(7)

# Création du graphique
ggplot(sejour_flamboyant, aes(y = reorder(SUBJID, REAENT))) +
  # Lignes de durée de séjour
  geom_linerange(
    aes(xmin = REAENT, xmax = FINDAT, color = Duree),
    linewidth = 1.5
  ) +
  
  # Indicateurs antibiotiques à droite
  geom_point(
    aes(
      x = max_date,
      shape = ifelse(ATB == "Oui", "Antibiotiques", "Pas d'antibiotiques")
    ),
    size = 3
  ) +
  
  # Échelle de l'axe X avec espace pour les indicateurs antibiotiques
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d %b",
    limits = c(min(sejour_flamboyant$REAENT, na.rm = TRUE), max_date + days(2)),
    expand = expansion(add = c(0, 5))
  ) +
  
  # Échelle des couleurs pour la durée
  scale_color_gradient(
    low = "#FF6F61", 
    high = "#6B5B95",
    name = "Durée (jours)"
  ) +
  
  # Échelle des formes pour les antibiotiques
  scale_shape_manual(
    name = "Antibiotiques",
    values = c("Antibiotiques" = 16, "Pas d'antibiotiques" = 1),
    labels = c("Oui", "Non")
  ) +
  
  # Labels et thème
  labs(
    title = "Durée de séjour des patients du secteur Flamboyant",
    x = "Date",
    y = "Patients (triés par date d'entrée)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8),
    plot.title.position = "plot"
  )
  
#Ajout données de colonisation (seulement pour le secteur flamboyant)

# Filtrer les prélèvements rectaux positifs pour les patients de Flamboyant
colonisation_flamboyant <- colonisation %>%
  filter(
    SUBJID %in% sejour_flamboyant$SUBJID,
    COLRES == "Positif"
  ) %>%
  mutate(COLDAT = dmy(COLDAT)) %>%
  left_join(sejour_flamboyant %>% select(SUBJID, REAENT, FINDAT), by = "SUBJID") %>%
  filter(COLDAT >= REAENT & COLDAT <= FINDAT)

# Filtrer les prélèvements bactériologiques positifs pour les patients de Flamboyant
infection <- read_delim("INFECTION.txt", delim = "#", col_names = TRUE)
infection_flamboyant <- infection %>%
  filter(
    SUBJID %in% sejour_flamboyant$SUBJID,
    INFIBR == "Positif"
  ) %>%
  mutate(INFDAT = dmy(INFDAT)) %>%
  left_join(sejour_flamboyant %>% select(SUBJID, REAENT, FINDAT), by = "SUBJID") %>%
  filter(INFDAT >= REAENT & INFDAT <= FINDAT)


# Calcul de la date maximale APRÈS le filtrage
max_date <- max(sejour_flamboyant$FINDAT, na.rm = TRUE) + days(7)

ggplot(sejour_flamboyant, aes(y = reorder(SUBJID, REAENT))) +
  # Lignes de durée de séjour
  geom_linerange(
    aes(xmin = REAENT, xmax = FINDAT, color = Duree),
    linewidth = 1.5
  ) +
  
  # Prélèvements rectaux positifs
  geom_point(
    data = colonisation_flamboyant,
    aes(x = COLDAT, shape = "Prélèvement rectal positif"),
    color = "darkgreen", size = 3
  ) +
  
  # Infections bactériologiques positives
  geom_point(
    data = infection_flamboyant,
    aes(x = INFDAT, shape = "Infection bactériologique"),
    color = "red", size = 3
  ) +
  
  # Antibiotiques
  geom_point(
    aes(
      x = max_date,
      shape = ifelse(ATB == "Oui", "Antibiotiques", "Pas d'antibiotiques")
    ),
    size = 3
  ) +
  
  # Échelle de dates
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d %b",
    limits = c(min(sejour_flamboyant$REAENT, na.rm = TRUE), max_date + days(10)),
    expand = expansion(add = c(1, 12))
  ) +
  
  # Échelle de couleurs
  scale_color_gradient(
    low = "#FF6F61", 
    high = "#6B5B95",
    name = "Durée (jours)"
  ) +
  
  # Échelle de formes unifiée
  scale_shape_manual(
    name = "Événements",
    values = c(
      "Antibiotiques" = 16,
      "Pas d'antibiotiques" = 1,
      "Prélèvement rectal positif" = 18,
      "Infection bactériologique" = 4
    ),
    labels = c("Antibiotiques", 
               "Infection bactérienne +", 
               "Pas d'antibiotiques",
               "Colonisation rectale +")
  ) +
  
  # Labels et thème
  labs(
    title = "Durée de séjour - Secteur Flamboyant",
    subtitle = "Avec événements cliniques et traitements antibiotiques",
    x = "Date",
    y = "Patients (triés par date d'entrée)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 10)
  )
+
  theme(
    axis.text.x = element_text(
      angle = 45,                   # Rotation à 45 degrés
      hjust = 1,                    # Alignement horizontal
      vjust = 1                     # Alignement vertical
    )
  ) 
