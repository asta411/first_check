#Visualization of the data: sejour,colonisation,infection,campagne,chambre,resultat,resultat_autre,TraPRea-Data_Service-Fin


rm(list = ls())
#Libraries--------------
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



# Load data---------------------------------------------------------------------
colonisation <- read.table("COLONISATION.txt", sep = "#", header = TRUE, encoding = "latin1", comment.char = "")
infection <- read.table("INFECTION.txt", sep = "#", header = TRUE, encoding = "latin1", comment.char = "")
sejour <- read.table("SEJOUR.txt", sep = "#", header = TRUE, encoding = "latin1", comment.char = "")


resultat <- read_excel("resultat.xlsx", col_names = TRUE)
resultat_autre <- read_excel("resultat_autre.xlsx", col_names = TRUE)
campagne <- read_excel("campagne.xlsx", col_names = TRUE)
chambre <- read_excel("chambre.xlsx", col_names = TRUE)

rea_data <- read_excel("TraPRea-Data_service-Fin.xlsx", col_names=TRUE)
summary(sejour)




# Distribution of patient ages----------------------------------------------------------
hist(sejour$PATAGE, 
     main = "Distribution of Patient Ages", 
     xlab = "Age of Patients", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black")


png("Patient_Age_Distribution.png")
hist(sejour$PATAGE, main = "Distribution of Patient Ages", xlab = "Age of Patients", col = "skyblue", border = "black")
dev.off()



# Transforming campagne into long format-----------------------------------------------------
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
      APP == "PEC" ~ "Echographe",
      APP == "PRA" ~ "",
      APP == "PEL" ~ "",
      APP == "PLE" ~ "",
      .default = NA
    )
  ) %>%
  # Select and order final columns
  select(CAMID, CAMNUM, CAMDAT, CAMSEC, APP, CODE, ENC)

# Remove rows with missing CODE/ENC if needed
campagne_long <- campagne_long %>% filter(!is.na(APP))

#transforming chambre into long format--------------------------------------

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



#Durée de séjour de chaque patient de l'étude à l'hopital-----------------------------------------

sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)

sejour <- sejour %>%
  mutate(
    REAENT = dmy(REAENT),
    FINDAT = dmy(FINDAT),
    HOSENT=dmy(HOSENT),
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
#Graphe secteur Flamboyant avec prise ou non d'antibiotiques MARCHE ---------------

# Filtrer les patients du secteur Flamboyant
sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)
sejour_flamboyant <- sejour %>%
  filter(REASEC1 == "Flamboyant") %>%
  mutate(
    REAENT = dmy(REAENT),
    FINDAT = dmy(FINDAT),
    HOSENT =dmy(HOSENT),
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
   
#Graphe avec les données de colonisations fécales et d'infections bactériologiques (seulement les résultats positifs et pour le secteur flamboyant)-------------
#MARCHE
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
    legend.text = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,                   # Rotation à 45 degrés
      #hjust = 1,                    # Alignement horizontal
      vjust = 1                     # Alignement vertical
    )
  ) 






#Tableau résumé des bactéries productrices de blse trouvées chez les patients lors de l'investigation de l'infection bactériologique----------------

infection_flamboyant <- infection_flamboyant %>%
  mutate(INFIBR_IC = ifelse(INFIBR_IC == "INFECTION", "Infection", INFIBR_IC),
         group_var = paste(INFNAT, INFIBR_IC, sep = " - ")
         )

infection_flamboyant_filtered <- infection_flamboyant %>%
  select(
    SUBJID,          # Identifiant patient
    INFNAT,          #Nature de l'investigation
    INFIBR_IC,       # Infection vs Colonisation
    INFKP,           # Klebsiella pneumoniae
    INFECC,          # Enterobacter cloacae
    INFECO           # Escherichia coli
  )

#we use tbl_summary from gt_summary :
tbl_infection <- infection_flamboyant_filtered %>%
  tbl_summary(
    by = INFIBR_IC,  # Comparer Infection vs Colonisation
    include = c(INFKP, INFECC, INFECO),
    label = list(
      INFKP ~ "Klebsiella pneumoniae",
      INFECC ~ "Enterobacter cloacae",
      INFECO ~ "Escherichia coli"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p() %>%  # Ajouter les tests statistiques
  modify_header(label ~ "**Bactérie**") %>%
  bold_labels()
tbl_infection

gt::gtsave(as_gt(tbl_infection), "tab1.png")



tbl_infection_nat <- infection_flamboyant_filtered %>%
  tbl_summary(
    by = INFNAT,  # Groupement par nature de l'investigation
    include = c(INFIBR_IC, INFKP, INFECC, INFECO),
    label = list(
      INFIBR_IC ~ "Type (Infection/Colonisation)",
      INFKP ~ "Klebsiella pneumoniae",
      INFECC ~ "Enterobacter cloacae",
      INFECO ~ "Escherichia coli"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_categorical() ~ "fisher.test",
      all_dichotomous() ~ "fisher.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Nature de l'investigation**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_infection_nat
gt::gtsave(as_gt(tbl_infection_nat), "tab4.png")


#commentaire : 
# Le résumé statistiques de tbl_infection ne concerne que les epèces productrices de blse 
# retrouvées chez les patients lors de l'investigation bactériologiques sur l'ensemble des résultats
# positifs lors de cette investigation sur chaque patient, ne sont pas considérés les autres infections 
# bactériologiques, les infections virologiques et les infections parasito-mycologiques.


#Tableau résumé des souches de Klebsiella pneumoniae retrouvées lors de l'investigation aux infections bactériologiques------------------
tbl_strains_kp <- infection_flamboyant %>%
  tbl_summary(
    by = INFIBR_IC,  # Comparer Strains de Infection vs Colonisation
    include = c(INFKPST),
    label = list(
      INFKPST ~ "Strains Klebsiella pneumoniae"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany"
  ) %>%
  modify_header(label ~ "**Souches**") %>%
  bold_labels()

tbl_strains_kp
gt::gtsave(as_gt(tbl_strains_kp), "tab2.png")
#Tableau résumé des souches d'Enterobacter cloacae retrouvées lors de l'investigation aux infections bactériologiques------------------

tbl_strains_entero <- infection_flamboyant %>%
  tbl_summary(
    by = INFIBR_IC,  # Comparer Strains de Infection vs Colonisation
    include = c(INFECCST),
    label = list(
      INFECCST ~ "Enterobacter cloacae"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany"
  ) %>%
  #modify_header(label ~ "**Souches**") %>%
  bold_labels()

tbl_strains_entero
gt::gtsave(as_gt(tbl_strains_entero), "tab3.png")

#Tableau résumé des souches de Escherichia coli retrouvées lors de l'investigation aux infections bactériologiques------------------
tbl_strains_ec <- infection_flamboyant %>%
  tbl_summary(
    by = INFIBR_IC,  # Comparer Strains de Infection vs Colonisation
    include = c(INFECOST),
    label = list(
      INFECOST ~ "Escherichia Coli"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany"
  ) %>%
  #modify_header(label ~ "**Souches**") %>%
  bold_labels()

tbl_strains_ec



#Tableau résumé statistique des données de colonisation (secteur flamboyant):--------------------

colonisation_flamboyant_filtered <- colonisation_flamboyant %>%
  select(
    SUBJID,          # Identifiant patient
    COLRES,          # Résultat test colonisation
    COLKP,           # Klebsiella pneumoniae
    COLECC,          # Enterobacter cloacae
    COLECO           # Escherichia coli
  )

tbl_colonisation <- colonisation_flamboyant_filtered %>%
  tbl_summary(
    by = COLRES, 
    include = c(COLKP, COLECC, COLECO),
    label = list(
      COLKP ~ "Klebsiella pneumoniae",
      COLECC ~ "Enterobacter cloacae",
      COLECO ~ "Escherichia coli"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  #add_p() %>%  # Ajouter les tests statistiques
  #modify_header(label ~ "**Bactérie**") %>%
  bold_labels()
tbl_colonisation
gt::gtsave(as_gt(tbl_colonisation), "tabcol1.png")
#commentaire :
#85% des tests donnent une colonisation du patient par Klebsiella pneumoniae.


#Tableau différentes souches de Escherichia coli trouvées lors de l'investigation colonisation fécale----------------
tbl_col_ec <- colonisation_flamboyant %>%
  tbl_summary(
    by = COLRES,  # Comparer Strains de Infection vs Colonisation
    include = c(COLECOST),
    label = list(
      COLECOST ~ "Escherichia Coli"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany"
  ) %>%
  #modify_header(label ~ "**Souches**") %>%
  bold_labels()

tbl_col_ec
gt::gtsave(as_gt(tbl_col_ec), "tabcol4.png")

#Tableau différentes souches de Klebsiella pneumoniae trouvées lors de l'investigation colonisation fécale----------------
tbl_col_kp <- colonisation_flamboyant %>%
  tbl_summary(
    by = COLRES,  # Comparer Strains de Infection vs Colonisation
    include = c(COLKPST),
    label = list(
      COLKPST ~ "Strains Klebsiella pneumoniae"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany"
  ) %>%
  modify_header(label ~ "**Souches**") %>%
  bold_labels()

tbl_col_kp
gt::gtsave(as_gt(tbl_colonisation), "tabcol2.png")

#Tableau différentes souches de Enterobacter cloacae trouvées lors de l'investigation colonisation fécale----------------
tbl_col_entero <- colonisation_flamboyant %>%
  tbl_summary(
    by = COLRES,  # Comparer Strains de Infection vs Colonisation
    include = c(COLECCST),
    label = list(
      COLECCST ~ "Enterobacter cloacae"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany"
  ) %>%
  #modify_header(label ~ "**Souches**") %>%
  bold_labels()

tbl_col_entero
gt::gtsave(as_gt(tbl_col_entero), "tabcol3.png")


#Checking the number of blse beds each day in the sector Flamboyant(méthode incorrecte à revoir)--------------

rea_data_flamboyant <- rea_data %>%
  filter(secteur=="FLAMBOYANT")

colonisation_events <- colonisation_flamboyant %>%
  mutate(COLDAT = as.Date(COLDAT)) %>%
  select(COLDAT) %>%
  rename(event_date = COLDAT) %>%
  mutate(event_type = "colonisation")

infection_events <- infection_flamboyant %>%
  mutate(INFDAT = as.Date(INFDAT)) %>%
  select(INFDAT) %>%
  rename(event_date = INFDAT) %>%
  mutate(event_type = "infection")

all_events <- bind_rows(colonisation_events, infection_events) %>%
  arrange(event_date)

event_counts <- all_events %>%
  group_by(event_date) %>%
  summarise(
    total_events = n(),
    nb_colonisations = sum(event_type == "colonisation"),
    nb_infections = sum(event_type == "infection")
  )

# 4. Jointure avec les données de lits
validation_df <- rea_data_flamboyant %>%
  mutate(date = as.Date(date)) %>%
  left_join(event_counts, by = c("date" = "event_date")) %>%
  mutate(
    total_events = replace_na(total_events, 0),
    beds_match = nb_lit_blse == total_events,
    discrepancy = nb_lit_blse - total_events
  )

# 5.Résultats concordance: seul 12 jour ou ça correspond c bizarre
cat("Jours avec discordance :", sum(!validation_df$beds_match), "/", nrow(validation_df), "\n")
cat("Erreur moyenne :", mean(abs(validation_df$discrepancy)), "lits/jour\n")

#Liste des dates ou une infection positive ou plus a été relevée:
infection_events_unique <- infection_events%>%
  distinct(event_date) %>%
  arrange(event_date)




#Graphe avec les données d'infections bactériologiques : 







#(Sexe du patient,durée de séjour du patient, prise d'antibiotiques, résultats positifs ou négatifs, )-------------------
#commentaire: focus plus tard sur une espèce Klebsiella Pneumoniae.

# Filtrer les prélèvements rectaux positifs pour les patients de Flamboyant
#commentaire : col_flamb_neg : résultats négatifs de l'investigation colonisation fécale pour tous les patients du secteur Flamboyant

col_flamb_neg <- colonisation %>%
  filter(
    SUBJID %in% sejour_flamboyant$SUBJID,
    COLRES=="Négatif"
  ) %>%
  mutate(COLDAT = dmy(COLDAT)) %>%
  left_join(sejour_flamboyant %>% select(SUBJID, REAENT, FINDAT), by = "SUBJID") %>%
  filter(COLDAT >= REAENT & COLDAT <= FINDAT)


# Filtrer les prélèvements bactériologiques positifs pour les patients de Flamboyant

infection <- read.table("INFECTION.txt", sep = "#", header = TRUE, encoding = "latin1", comment.char = "")
infection <- infection %>%
  mutate(
    INFIBR_IC=ifelse(INFIBR_IC=="INFECTION","Infection",INFIBR_IC)
  )

sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)
sejour_flamboyant <- sejour %>%
  filter(REASEC1 == "Flamboyant") %>%
  mutate(
    REAENT = dmy(REAENT),
    FINDAT = dmy(FINDAT),
    HOSENT =dmy(HOSENT),
    Duree = as.numeric(FINDAT - REAENT)
  )

inf_flamb_neg <- infection %>%
  filter(
    SUBJID %in% sejour_flamboyant$SUBJID,
    INFIBR=="Négatif"
  ) %>%
  mutate(INFDAT = dmy(INFDAT)) %>%
  left_join(sejour_flamboyant %>% select(SUBJID, REAENT, FINDAT), by = "SUBJID") %>%
  filter(INFDAT >= REAENT & INFDAT <= FINDAT)
#commentaire : inf_flamb : résultats négatifs de l'investigation infection bactériologiques pour tous les patients du secteur Flamboyant

# Filtrer les prélèvements bactériologiques positifs pour les patients de Flamboyant
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

#graphe sur le secteur flamboyant----------------
#probleme d'affichage des legendes a corriger +


graph1 <- ggplot(sejour_flamboyant, aes(y = reorder(SUBJID, REAENT))) +
  # Lignes de durée de séjour
  geom_linerange(
    aes(xmin = REAENT, xmax = FINDAT, color = PATSEX),
    linewidth = 1.5
  ) +
  # #Marqueur d'entrée en service de réanimation
  # geom_point(
  #   aes(x=REAENT,shape="Date d'entrée en service de réanimation"),
  #   color="darkgreen",
  #   size=3
  # )+
  # Infections bactériologiques
  geom_point(
    data = inf_flamb_neg,
    aes(x = INFDAT, shape = "Infection bactériologique négative"),
    color = "red", 
    size = 3
  ) +
  geom_point(
    data = infection_flamboyant,
    aes(x = INFDAT, shape = "Infection bactériologique positive"),
    color = "black", 
    size = 3
  ) +
  
  #Barre antibiotiques à droite
  geom_tile(
    aes(
      x = max_date,  # Position ajustée
      width = 3,               # Largeur augmentée
      fill = ifelse(ATB == "Oui", "Antibiotiques", "Pas d'antibiotiques")
    ),
    height = 0.8,              # Hauteur ajustée
    show.legend = TRUE
  ) +
  
  # Échelles
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d %b",
    limits = c(
      min(sejour_flamboyant$REAENT, na.rm = TRUE) - days(5),
      max_date + days(15)  # Espace supplémentaire à droite
    ),
    expand = expansion(add = c(0, 3))  # Réduction de l'expansion
  ) +
  scale_color_manual(
    name = "Sexe",
    values = c("Masculin" = "orange", "Feminin" = "darkturquoise"),
    labels=c("Masculin","Feminin")
  ) +
  scale_fill_manual(
    name = "Antibiotiques",
    values = c("Antibiotiques" = "#1F77B4", "Pas d'antibiotiques" = "#FF7F0E"),
    labels = c("Oui", "Non")
  ) +
  scale_shape_manual(
    name = "Evenements",
    values = c(
      #"Date d'entrée en service de réanimation"=3,
      "Infection bactériologique négative" = 4,
      "Infection bactériologique positive" = 20
    ),
    labels=c("Infection bactériologique négative",
             "Infection bactériologique positive")
  ) +
  
  # Thème
  labs(
    title = "Suivi des patients - Secteur Flamboyant",
    subtitle = "Durée de séjour avec événements cliniques et traitement antibiotique",
    x = "Date",
    y = "Patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(r = 50, l = 50),
    legend.position = "right",
    legend.box.just = "left"
  )



#graphe sur le secteur mahogany----------------
infection <- read.table("INFECTION.txt", sep = "#", header = TRUE, encoding = "latin1", comment.char = "")
infection <- infection %>%
  mutate(
    INFIBR_IC=ifelse(INFIBR_IC=="INFECTION","Infection",INFIBR_IC)
  )
sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)
sejour_mahogany <- sejour %>%
  filter(REASEC1 == "Mahogany") %>%
  mutate(
    REAENT = dmy(REAENT),
    FINDAT = dmy(FINDAT),
    HOSENT =dmy(HOSENT),
    Duree = as.numeric(FINDAT - REAENT)
  )
inf_ma_neg <- infection %>%
  filter(
    SUBJID %in% sejour_mahogany$SUBJID,
    INFIBR=="Négatif"
  ) %>%
  mutate(INFDAT = dmy(INFDAT)) %>%
  left_join(sejour_mahogany %>% select(SUBJID, REAENT, FINDAT), by = "SUBJID") %>%
  filter(INFDAT >= REAENT & INFDAT <= FINDAT)

infection_mahogany <- infection %>%
  filter(
    SUBJID %in% sejour_mahogany$SUBJID,
    INFIBR == "Positif"
  ) %>%
  mutate(INFDAT = dmy(INFDAT)) %>%
  left_join(sejour_mahogany %>% select(SUBJID, REAENT, FINDAT), by = "SUBJID") %>%
  filter(INFDAT >= REAENT & INFDAT <= FINDAT)

# Calcul de la date maximale APRÈS le filtrage
max_date_ma <- max(sejour_mahogany$FINDAT, na.rm = TRUE) + days(7)

graph2 <- ggplot(sejour_mahogany, aes(y = reorder(SUBJID, REAENT))) +
  # Lignes de durée de séjour
  geom_linerange(
    aes(xmin = REAENT, xmax = FINDAT, color = PATSEX),
    linewidth = 1.5
  ) +
  # #Marqueur d'entrée en service de réanimation
  # geom_point(
  #   aes(x=REAENT,shape="Date d'entrée en service de réanimation"),
  #   color="darkgreen",
  #   size=3
  # )+
  # Infections bactériologiques
  geom_point(
    data = inf_ma_neg,
    aes(x = INFDAT, shape = "Infection bactériologique négative"),
    color = "red", 
    size = 3
  ) +
  geom_point(
    data = infection_mahogany,
    aes(x = INFDAT, shape = "Infection bactériologique positive"),
    color = "black", 
    size = 3
  ) +
  
  #Barre antibiotiques à droite
  geom_tile(
    aes(
      x = max_date_ma,  # Position ajustée
      width = 3,               # Largeur augmentée
      fill = ifelse(ATB == "Oui", "Antibiotiques", "Pas d'antibiotiques")
    ),
    height = 0.8,              # Hauteur ajustée
    show.legend = TRUE
  ) +
  
  # Échelles
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d %b",
    limits = c(
      min(sejour_mahogany$REAENT, na.rm = TRUE) - days(5),
      max_date + days(15)  # Espace supplémentaire à droite
    ),
    expand = expansion(add = c(0, 3))  # Réduction de l'expansion
  ) +
  scale_color_manual(
    name = "Sexe",
    values = c("Masculin" = "orange", "Feminin" = "darkturquoise"),
    labels=c("Masculin","Feminin")
  ) +
  scale_fill_manual(
    name = "Antibiotiques",
    values = c("Antibiotiques" = "#1F77B4", "Pas d'antibiotiques" = "#FF7F0E"),
    labels = c("Oui", "Non")
  ) +
  scale_shape_manual(
    name = "Evenements",
    values = c(
      "Infection bactériologique négative" = 4,
      "Infection bactériologique positive" = 20
    ),
    labels=c("Infection bactériologique négative",
             "Infection bactériologique positive")
  ) +
  
  # Thème
  labs(
    title = "Suivi des patients - Secteur Mahogany",
    subtitle = "Durée de séjour avec événements cliniques et traitement antibiotique",
    x = "Date",
    y = "Patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(r = 50, l = 50),
    legend.position = "right",
    legend.box.just = "left"
  )

#graphe sur le secteur colibri----------------
infection <- read.table("INFECTION.txt", sep = "#", header = TRUE, encoding = "latin1", comment.char = "")
infection <- infection %>%
  mutate(
    INFIBR_IC=ifelse(INFIBR_IC=="INFECTION","Infection",INFIBR_IC)
  )
sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)
sejour_colibri <- sejour %>%
  filter(REASEC1 == "Colibri") %>%
  mutate(
    REAENT = dmy(REAENT),
    FINDAT = dmy(FINDAT),
    HOSENT =dmy(HOSENT),
    Duree = as.numeric(FINDAT - REAENT)
  )
inf_col_neg <- infection %>%
  filter(
    SUBJID %in% sejour_colibri$SUBJID,
    INFIBR=="Négatif"
  ) %>%
  mutate(INFDAT = dmy(INFDAT)) %>%
  left_join(sejour_colibri %>% select(SUBJID, REAENT, FINDAT), by = "SUBJID") %>%
  filter(INFDAT >= REAENT & INFDAT <= FINDAT)

infection_colibri <- infection %>%
  filter(
    SUBJID %in% sejour_colibri$SUBJID,
    INFIBR == "Positif"
  ) %>%
  mutate(INFDAT = dmy(INFDAT)) %>%
  left_join(sejour_colibri %>% select(SUBJID, REAENT, FINDAT), by = "SUBJID") %>%
  filter(INFDAT >= REAENT & INFDAT <= FINDAT)

# Calcul de la date maximale APRÈS le filtrage
max_date_col <- max(sejour_colibri$FINDAT, na.rm = TRUE) + days(7)

graph3 <- ggplot(sejour_colibri, aes(y = reorder(SUBJID, REAENT))) +
  # Lignes de durée de séjour
  geom_linerange(
    aes(xmin = REAENT, xmax = FINDAT, color = PATSEX),
    linewidth = 1.5
  ) +
  # #Marqueur d'entrée en service de réanimation
  # geom_point(
  #   aes(x=REAENT,shape="Date d'entrée en service de réanimation"),
  #   color="darkgreen",
  #   size=3
  # )+
  # Infections bactériologiques
  geom_point(
    data = inf_col_neg,
    aes(x = INFDAT, shape = "Infection bactériologique négative"),
    color = "red", 
    size = 3
  ) +
  geom_point(
    data = infection_colibri,
    aes(x = INFDAT, shape = "Infection bactériologique positive"),
    color = "black", 
    size = 3
  ) +
  
  #Barre antibiotiques à droite
  geom_tile(
    aes(
      x = max_date_col,  # Position ajustée
      width = 3,               # Largeur augmentée
      fill = ifelse(ATB == "Oui", "Antibiotiques", "Pas d'antibiotiques")
    ),
    height = 0.8,              # Hauteur ajustée
    show.legend = TRUE
  ) +
  
  # Échelles
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d %b",
    limits = c(
      min(sejour_colibri$REAENT, na.rm = TRUE) - days(5),
      max_date + days(15)  # Espace supplémentaire à droite
    ),
    expand = expansion(add = c(0, 3))  # Réduction de l'expansion
  ) +
  scale_color_manual(
    name = "Sexe",
    values = c("Masculin" = "orange", "Feminin" = "darkturquoise"),
    labels=c("Masculin","Feminin")
  ) +
  scale_fill_manual(
    name = "Antibiotiques",
    values = c("Antibiotiques" = "#1F77B4", "Pas d'antibiotiques" = "#FF7F0E"),
    labels = c("Oui", "Non")
  ) +
  scale_shape_manual(
    name = "Evenements",
    values = c(
      "Infection bactériologique négative" = 4,
      "Infection bactériologique positive" = 20
    ),
    labels=c("Infection bactériologique négative",
             "Infection bactériologique positive")
  ) +
  
  # Thème
  labs(
    title = "Suivi des patients - Secteur Colibri",
    subtitle = "Durée de séjour avec événements cliniques et traitement antibiotique",
    x = "Date",
    y = "Patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(r = 50, l = 50),
    legend.position = "right",
    legend.box.just = "left"
  )






# Créer un dataframe unifié pour tous les événements---------
all_events <- bind_rows(
  infection_flamboyant %>% mutate(INFIBR = "Positif"),
  inf_flamb_neg %>% mutate(INFIBR = "Négatif")
) %>% 
  mutate(
    Type_evenement = case_when(
      INFIBR_IC == "Infection" & INFIBR == "Positif" ~ "Infection positive",
      INFIBR_IC == "Colonisation" & INFIBR == "Positif" ~ "Colonisation positive",
      INFIBR_IC == "Infection" & INFIBR == "Négatif" ~ "Infection négative",
      INFIBR_IC == "Colonisation" & INFIBR == "Négatif" ~ "Colonisation négative"
    )
  )

#Gros tableau résumé statistiques sur les données de sejour et de colonisation et d'infection par Entero, Ec et K-P par secteur---------------- 

sejour <- sejour %>%
  mutate(Duree=as.numeric(sejour$FINDAT - sejour$HOSENT))

tableau <- sejour %>%
  select(REASEC1, PATAGE, PATSEX, Duree, ATB) %>%
  tbl_summary(
    by = REASEC1,
    statistic = list(
      all_continuous() ~ "{mean} ({median})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "oneway.test",
      all_categorical() ~ "fisher.test"
    ),
    test.args = all_continuous() ~ list(var.equal = TRUE)
  ) %>%
  modify_caption("**Tableau 1. Analyse par secteur**")
tableau

library(dplyr)
library(gtsummary)
library(lubridate)

#marche PAAASSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
# Étape 1 : Fusion des données
sejour_col_inf <- sejour %>%
  # Jointure avec colonisation
  left_join(
    colonisation %>%
      group_by(REASEC1, SUBJID) %>%
      summarise(
        COLKP = sum(COLKP == "Positif", na.rm = TRUE),
        COLECC = sum(COLECC == "Positif", na.rm = TRUE),
        COLECO = sum(COLECO == "Positif", na.rm = TRUE),
        .groups = 'drop'
      ),
    by = c("REASEC1", "SUBJID")
  ) %>%
  # Jointure avec infection
  left_join(
    infection %>%
      group_by(RECSEC1, SUBJID) %>%
      summarise(
        INFKP = sum(INFKP == "Positif" & INFIBR_IC == "Infection", na.rm = TRUE),
        INFECC = sum(INFECC == "Positif" & INFIBR_IC == "Infection", na.rm = TRUE),
        INFECO = sum(INFECO == "Positif" & INFIBR_IC == "Infection", na.rm = TRUE),
        .groups = 'drop'
      ),
    by = c("REASEC1", "SUBJID")
  ) %>%
  # Remplacement des NA par 0
  mutate(across(c(COLKP, COLECC, COLECO, INFKP, INFECC, INFECO), ~ replace_na(.x, 0)))

# Étape 2 : Création du tableau récapitulatif
tableau <- sejour_col_inf %>%
  select(RECSEC1, PATAGE, PATSEX, Duree, ATB, 
         COLKP, COLECC, COLECO, INFKP, INFECC, INFECO) %>%
  tbl_summary(
    by = RECSEC1,
    statistic = list(
      all_continuous() ~ "{mean} ({median})",
      all_categorical() ~ "{n} ({p}%)",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    label = list(
      COLKP ~ "Colonisations à Klebsiella pneumoniae",
      COLECC ~ "Colonisations à Enterobacter cloacae",
      COLECO ~ "Colonisations à Escherichia coli",
      INFKP ~ "Infections à Klebsiella pneumoniae",
      INFECC ~ "Infections à Enterobacter cloacae", 
      INFECO ~ "Infections à Escherichia coli"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "oneway.test",
      all_categorical() ~ "fisher.test"
    ),
    test.args = all_continuous() ~ list(var.equal = TRUE)
  ) %>%
  modify_caption("**Tableau 1. Analyse par secteur avec données de colonisation et d'infection**") %>%
  bold_labels() %>%
  italicize_levels()

