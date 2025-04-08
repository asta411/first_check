
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
colonisation <- read_delim("COLONISATION.txt", delim = "#", col_names = TRUE)
infection <- read_delim("INFECTION.txt", delim = "#", col_names = TRUE)
sejour <- read_delim("SEJOUR.txt", delim = "#", col_names = TRUE)


resultat<- read_excel("resultat.xlsx", col_names = TRUE)
resultat_autre<- read_excel("resultat_autre.xlsx", col_names = TRUE)
campagne<- read_excel("campagne.xlsx", col_names = TRUE)
chambre<- read_excel("chambre.xlsx", col_names = TRUE)

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

