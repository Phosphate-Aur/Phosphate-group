## IMPORTATION OF THE DATA
data <- read.table("C:/Users/auria/Documents/Supagro 2023-2024/JRLT/project/CopieEXP.csv",
                   header = FALSE, sep = ",", dec = ",", na.strings = "")

## QUESTION 1
# Quick look of the data
library(dplyr)
library(ggplot2)

colnames(data)[1] <- "Strain"
colnames(data)[2] <- "Concentration"
colnames(data)[3] <- "Absorbance"
# Supprimer les lignes avec "Concentration" ou "Absorbance t="
data <- subset(data, !(concentration == "Concentration" | Absorbance == "Absorbance t="))

# Supprimer les lignes avec des valeurs manquantes
data <- na.omit(data)
# Supprimer la première ligne
data <- data[-1, ]

# Afficher un résumé des données
summary(data)
str(data)

# Convertir les colonnes en numérique
data$Concentration <- as.numeric(gsub(",", ".", data$Concentration))
data$Absorbance <- as.numeric(data$Absorbance)


# Tracer le nuage de points avec l'absorbance en fonction de la concentration
plot(data$Concentration, data$Absorbance)

# Assurez-vous que data$Strain est de type factor
data$Strain <- factor(data$Strain)

# Utiliser une palette de couleurs pour les différentes souches
colors <- rainbow(length(levels(data$Strain)))

# Tracer le graphique en utilisant les couleurs définies
plot(data$Concentration, data$Absorbance, col = colors[data$Strain])
# Ajouter une légende
legend("topright", legend = levels(data$Strain), fill = colors, title = "Strain")

# Tracer le nuage de points de l'absorbance en fonction de la souche
plot(data$Strain, data$Absorbance)

<<<<<<< Updated upstream
data <- 
hhhhhhh
=======
>>>>>>> Stashed changes
