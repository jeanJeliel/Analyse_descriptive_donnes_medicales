#le chemin d'accès complet
diabetes <- read.csv("C:/IA School/Cours/M2/Analyse descriptive avec R/diabetes.csv")
# Afficher les premières lignes du fichier pour vérifier
head(diabetes)

#ANALYSE DE LA DESCRIPTION

# Charger ggplot2
library(ggplot2)

# Création de l'histogramme pour la variable Glucose
ggplot(data = diabetes, aes(x = Glucose)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogramme des niveaux de glucose", x = "Niveaux de glucose", y = "Fréquence") +
  theme_minimal()

# Création du boxplot pour la variable BMI en fonction du Outcome
ggplot(data = diabetes, aes(x = factor(Outcome), y = BMI, fill = factor(Outcome))) +
  geom_boxplot() +
  labs(title = "Boxplot de l'IMC en fonction du résultat du diabète", 
       x = "Résultat du diabète (Outcome)", y = "IMC (BMI)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# MANIPULATION DES DONNEES AVEC DPLYR
# Charger la bibliothèque dplyr
library(dplyr)

# Filtrer les patients ayant un taux de glucose supérieur à 120
patients_glucose_120 <- diabetes %>%
  filter(Glucose > 120)
head(patients_glucose_120)

# Créer une nouvelle variable BMI_category avec mutate()
diabetes <- diabetes %>%
  mutate(BMI_category = case_when(
    BMI < 18.5 ~ "faible",
    BMI >= 18.5 & BMI < 25 ~ "normal",
    BMI >= 25 ~ "élevé"
  ))
head(diabetes)

# Calculer la moyenne de l'âge et du taux de glucose pour chaque catégorie de BMI_category
summary_bmi_category <- diabetes %>%
  group_by(BMI_category) %>%
  summarize(
    mean_age = mean(Age, na.rm = TRUE),
    mean_glucose = mean(Glucose, na.rm = TRUE)
  )
print(summary_bmi_category)

# Trier le dataset par Glucose décroissant et afficher les 10 premières lignes
diabetes_sorted <- diabetes %>%
  arrange(desc(Glucose)) %>%
  head(10)
# Afficher les résultats
print(diabetes_sorted)

# ANALYSE DE LA CORRELATION
library(ggplot2)
# Créer un scatter plot pour visualiser la relation entre Glucose et BMI
ggplot(data = diabetes, aes(x = BMI, y = Glucose)) +
  geom_point(alpha = 0.6, color = "blue") +  # Ajouter des points
  labs(title = "Relation entre le taux de glucose et l'indice de masse corporelle",
       x = "Indice de masse corporelle (BMI)", y = "Niveau de glucose") +
  theme_minimal()

# Calculer la corrélation entre Glucose et BMI
correlation <- cor(diabetes$Glucose, diabetes$BMI, use = "complete.obs")
print(correlation)

# Interprètation de la correlation
# La correlation est proche de 0, cela indique qu'il n'y a 
# pas de relation linéaire significative entre les deux variables.


# MATRICE DE CORRELATION ET HEATMAP
# Sélectionner les variables numériques
numerical_vars <- diabetes %>%
  select(Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin, BMI, DiabetesPedigreeFunction, Age)

# Calculer la matrice de corrélation
correlation_matrix <- cor(numerical_vars, use = "complete.obs")
print(correlation_matrix)


install.packages("ggcorrplot")
library(ggcorrplot)

# Représenter la matrice de corrélation sous forme de heatmap
ggcorrplot(correlation_matrix, 
           method = "circle", 
           type = "lower", 
           title = "Matrice de corrélation des variables numériques", 
           lab = TRUE)

# Interpretation des corrélations les plus significatives 

# Pregnancies et Age (r = 0.544) : Corrélation positive forte 
# indiquant que les femmes plus âgées ont tendance à avoir eu plus de grossesses.

# Glucose et Insulin (r = 0.331): Corrélation positive modérée suggérant que des niveaux plus élevés de 
# glucose sont associés à des niveaux d'insuline plus élevés.

# loodPressure et SkinThickness (r = 0.207): Corrélation positive indiquant que des pressions artérielles plus élevées sont 
# liées à une plus grande épaisseur de la peau.

# BMI et SkinThickness (r = 0.393): Corrélation positive modérée, suggérant que les personnes avec un BMI plus élevé ont 
# tendance à avoir une plus grande épaisseur de la peau.

# Glucose et BMI (r = 0.221): Corrélation positive indiquant que les 
#niveaux de glucose augmentent avec un BMI plus élevé.

# Age et Glucose (r = 0.264): Corrélation positive modérée suggérant que les niveaux de 
# glucose peuvent augmenter avec l'âge.

# CONCLUSION

# L'analyse du dataset diabetes montre des corrélations significatives : une forte relation positive 
# entre le nombre de grossesses et l'âge (r = 0.544), et des corrélations modérées entre le glucose et 
# l'insuline (r = 0.331), ainsi qu'entre le glucose et le BMI (r = 0.221). La pression artérielle est liée 
# à l'épaisseur de la peau (r = 0.207) et au BMI (r = 0.393). De plus, le glucose augmente avec l'âge (r = 0.264). 
# Ces résultats soulignent l'importance de surveiller ces facteurs pour la gestion du diabète et suggèrent des 
# pistes pour des interventions ciblées.


