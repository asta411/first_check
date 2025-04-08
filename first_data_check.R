
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
head(sejour)



hist(sejour$PATAGE, 
     main = "Distribution of Patient Ages", 
     xlab = "Age of Patients", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black")


png("Patient_Age_Distribution.png")
hist(sejour$PATAGE, main = "Distribution of Patient Ages", xlab = "Age of Patients", col = "skyblue", border = "black")
dev.off()

# Replace FAUX with 0 and VRAI with 1
result_pos <- resultat %>%
  mutate(across(c(RESKP, RESECC, RESECO, RESPSA, RESACB), ~ ifelse(. == "VRAI", 1, 0)))

# Sum all the positive cases for each bacteria
positive_sums <- colSums(result_pos[, c("RESKP", "RESECC", "RESECO", "RESPSA", "RESACB")])

# Create a histogram of the summed positive cases
barplot(
  positive_sums,
  main = "Positive Cases for Each Bacteria",
  xlab = "Bacteria",
  ylab = "Number of Positive Cases",
  col = "skyblue",
  border = "black"
)

#pb tout est à zero mais c pas forcement pertinent ce que je fais donc changer 
