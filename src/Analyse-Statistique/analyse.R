# ===================================================================================================
# Author : Ousmane Tom BECHIR
# Project : Projet du cours De la fouille de Données à l'Auto-ML. Part1
# ===================================================================================================

# Installation et importation des library. ----
library(lubridate)
library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(here)

getwd()
data <- read_csv(here("data", "player_data_2025.csv"))

# 1. Création d'un groupe d'âge ----
age <- interval(start = ymd(data$dob), end = ymd(data$version)) / years(1)

GA <- cut(
  age,
  breaks = c(-Inf, 20, 25, 30, 35, Inf),
  labels = c("<20", "20-25", "25-30", "30-35", ">35"),
  right = FALSE
)

# 2. Création de l'attribut BMI(IMC: indice de masse corporelle) ----
IMC <- data$weight_kg / (data$height_cm / 100)^2

# 3. Modification de la granulité des positions des joueurs ----
position_mapping <- c(
  "GK"  = "GK",
  "RB"  = "DEF", "LB" = "DEF", "RCB" = "DEF", "LCB" = "DEF", "CB" = "DEF",
  "RM"  = "MID", "LM" = "MID", "RCM" = "MID", "LCM" = "MID", "CDM" = "MID", "CM" = "MID",
  "RDM" = "MID", "LDM" = "MID",
  "CAM" = "MID", "RAM" = "MID", "LAM" = "MID",
  "RW"  = "FWD", "LW" = "FWD", "ST" = "FWD", "RS" = "FWD", "LS" = "FWD",
  "RES" = "SUB", "SUB" = "SUB"
)

club_position_cat <- sapply(data$club_position, function(x) position_mapping[x])
country_position_cat <- sapply(data$country_position, function(x) position_mapping[x])

# 4 Suppression des symboles "+" dans les colonnes ----
data_without_plus <- as.data.frame(lapply(data, function(col) {
  sub("\\+.*", "", col)
}))

# 5. Formatage de la colonne wage ----
clean_wage <- data_without_plus$wage %>%
  sub("€", "", .) %>%
  sub("K", "", .) %>%
  as.numeric()

# 6. Formatage et discrétisation de la colonne value ---- 
discretes_values_num <- sapply(data_without_plus$value, function(d) {
  num <- sub("€", "", d)
  suffix <- sub("[0-9.]+", "", num)
  num <- as.numeric(sub("[A-Z]+", "", num))
  
  if (suffix == "K") {
    return(num * 1e3)
  } else if (suffix == "M") {
    return(num * 1e6)
  } else {
    return(num)
  }
})

discretes_values <- cut(
  discretes_values_num,
  breaks = quantile(discretes_values_num, probs = seq(0, 1, length.out = 6)),
  labels = c("very low", "low", "medium", "high", "elite"),
  include.lowest = TRUE
)

# 7. Discrétisation de la colonne wage ----
discretes_wage <- cut(
  clean_wage,
  breaks = quantile(clean_wage, probs = seq(0, 1, length.out = 6)),
  labels = c("very low", "low", "medium", "high", "elite"),
  include.lowest = TRUE
)

# 8. Suppression des attributs unitils ----
data_without_plus$club_logo <- NULL
data_without_plus$image <- NULL
data_without_plus$full_name <- NULL
data_without_plus$description <- NULL
data_without_plus$country_flag <- NULL

# 9. Création de la variable durée de contrat restante ----
numeric_end_contract_date <- sub("[a-zA-Z]+", "", sub("[a-zA-Z0-9 ]+,+ ", "", data_without_plus$club_contract_valid_until)) %>% 
  as.numeric()

duree_contrat_restant <- numeric_end_contract_date - year(data_without_plus$version)  

# 10. Calcule de scores par grand familles de competences ----
# Nous constatons qu'il manque beaucoup de données dans les colonnes des attributs techniques.

# Nous allons compléter notre les données manquante par les valeurs recupérées dans la dataset "legacy"
data_legacy = read_csv(here("data", "male_players (legacy).csv"))

col_a_completer = c("crossing", "finishing", "heading_accuracy", "short_passing", "volleys", "dribbling", "curve", "work_rate", 
                    "fk_accuracy", "long_passing", "ball_control", "acceleration", "sprint_speed", "agility", "reactions")

# Créer un index pour accès rapide aux données legacy par player_id
legacy_by_id <- setNames(split(data_legacy, data_legacy$player_id), 
                         unique(data_legacy$player_id))

for (i in 1:nrow(data_without_plus)) {
  player_id <- data_without_plus$player_id[i]
  
  # Vérifier si le joueur exist dans legacy
  if (as.character(player_id) %in% names(legacy_by_id)) {
    legacy_row <- legacy_by_id[[as.character(player_id)]]
    
    # Compléter toutes les colonnes manquantes pour ce joueur
    for (col in col_a_completer) {
      if (col %in% names(data_without_plus) && col %in% names(legacy_row)) {
        current_value <- data_without_plus[[col]][i]
        
        # Si la valeur est manquante ou vide
        if (is.na(current_value) || current_value == "" || current_value == "NA") {
          legacy_value <- legacy_row[[col]][1]
          if (!is.na(legacy_value) && legacy_value != "") {
            data_without_plus[[col]][i] <- legacy_value
          }
        }
      }
    }
  }
}

cols_to_numeric <- c(
  "finishing", "shot_power", "positioning", "penalties", "volleys", "long_shots",
  "short_passing", "long_passing", "vision", "crossing", "fk_accuracy", "curve",
  "dribbling", "agility", "balance", "ball_control", "reactions",
  "interceptions", "standing_tackle", "sliding_tackle", "defensive_awareness", "aggression",
  "strength", "stamina", "jumping",
  "overall_rating", "potential"
)

data_without_plus[cols_to_numeric] <- lapply(data_without_plus[cols_to_numeric], as.numeric)

# Encore toutes les colonnes n'existent pas dans legacy. Nous proposons de les remplacer par leur variante, comme passing
# shooting, defending, physic, attacking_crossing, attacking_finishing, attacking_heading_accuracy existent dans legacy

new_col <- c("passing", "shooting", "defending", "physic", "attacking_crossing", "attacking_finishing", "attacking_heading_accuracy")
data_without_plus[new_col] <- data_legacy[new_col]

data_without_plus <- data_without_plus %>%
  mutate(
    Attaque = rowMeans(select(., finishing, shot_power, positioning, penalties, volleys, long_shots, shooting, attacking_crossing, attacking_finishing, attacking_heading_accuracy), na.rm = TRUE),
    Passe = rowMeans(select(., passing, short_passing, long_passing, vision, crossing, fk_accuracy, curve), na.rm = TRUE),
    Dribble = rowMeans(select(.,dribbling, agility, balance, ball_control, reactions), na.rm = TRUE),
    Defense = rowMeans(select(., defending, interceptions, standing_tackle, sliding_tackle, defensive_awareness, aggression), na.rm = TRUE),
    Physique = rowMeans(select(., physic, strength, stamina, jumping), na.rm = TRUE)
  )


# Ajout des nouvelles variables au dataset
data_final <- data_without_plus %>%
  mutate(
    age = age,
    groupe_age = GA,
    IMC = IMC,
    club_position_cat = club_position_cat,
    country_position_cat = country_position_cat,
    wage_clean = clean_wage,
    wage_cat = discretes_wage,
    value_num = discretes_values_num,
    value_cat = discretes_values,
    duree_contrat = duree_contrat_restant
  )

# 1.2 Analyse descriptive ----
# A. Attributs et statistiques descriptives ----

# a. Présentation des attributs
cat("=== PRÉSENTATION DES ATTRIBUTS ===\n\n")
str(data_final)

# b. Statistiques de base
cat("\n=== STATISTIQUES DESCRIPTIVES ===\n\n")

vars_numeriques <- c("age", "IMC", "height_cm", "weight_kg", "overall_rating", 
                     "potential", "value_num", "wage_clean","Attaque", "Passe", 
                     "Dribble", "Defense", "Physique")

for (col in vars_numeriques) {
  data_final[[col]] <- as.numeric(data_final[[col]])
}

# Structure des données
summary(data_final)

# Statistiques descriptives sur l'âge
cat("\n=== STATISTIQUES SUR L'ÂGE ===\n")
print(summary(age))

# Distribution de l'IMC
cat("\n=== STATISTIQUES SUR L'IMC ===\n")
print(summary(data_final$IMC))

# Répartition par groupe d'âge
cat("\n=== RÉPARTITION PAR GROUPE D'ÂGE ===\n")
print(table(GA))

# Répartition par position catégorisée (club)
cat("\n=== RÉPARTITION PAR POSITION (CLUB) ===\n")
print(table(club_position_cat))

# Statistiques sur les scores de compétences
cat("\n=== STATISTIQUES SUR LES SCORES DE COMPÉTENCES ===\n")
cat("Score Attaque:\n")
print(summary(data_final$Attaque))

cat("\nScore Physique:\n")
print(summary(data_final$Physique))
cat("\nScore Défense:\n")
print(summary(data_final$Defense))

# c. Visualisations
pdf("visualisations_fifa.pdf", width = 12, height = 8)

par(mfrow = c(2, 3))
hist(data_final$age, main = "Distribution de l'âge", xlab = "Âge", col = "lightblue", breaks = 30)
hist(data_final$overall_rating, main = "Distribution Overall Rating", xlab = "Rating", col = "lightgreen", breaks = 30)
hist(data_final$IMC, main = "Distribution IMC", xlab = "IMC", col = "coral", breaks = 30)
barplot(table(data_final$groupe_age), main = "Joueurs par groupe d'âge", col = "steelblue")
barplot(table(data_final$club_position_cat), main = "Joueurs par position", col = "orange")
barplot(table(data_final$value_cat), main = "Joueurs par catégorie de valeur", col = "lightgreen")

par(mfrow = c(2, 2))
boxplot(overall_rating ~ groupe_age, data = data_final, main = "Rating par groupe d'âge", 
        xlab = "Groupe d'âge", ylab = "Overall Rating", col = "lightblue")
boxplot(value_num ~ club_position_cat, data = data_final, main = "Valeur par position", 
        xlab = "Position", ylab = "Valeur (€)", col = "lightgreen")
boxplot(wage_clean ~ value_cat, data = data_final, main = "Salaire par catégorie de valeur", 
        xlab = "Catégorie de valeur", ylab = "Salaire (K€)", col = "coral")
boxplot(IMC ~ club_position_cat, data = data_final, main = "IMC par position", 
        xlab = "Position", ylab = "IMC", col = "steelblue")

dev.off()

# B. Analyse de corrélation ----
cat("\n=== ANALYSE DE CORRÉLATION ===\n\n")

vars_correlation <- c("overall_rating", "potential", "value_num", "wage_clean", 
                      "Attaque", "Passe", "Dribble", "Defense", "Physique", "age")

cor_matrix <- cor(data_final[vars_correlation], use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.6, tl.cex = 0.8,
         title = "Heatmap des corrélations", mar = c(0,0,1,0))
dev.off()

cat("Corrélation entre Overall Rating et Value:", cor(data_final$overall_rating, data_final$value_num, use = "complete.obs"), "\n")
cat("Corrélation entre Wage et Potential:", cor(data_final$wage_clean, data_final$potential, use = "complete.obs"), "\n")

# C. Prise en main des données ----

# 1. Créez l'équipe la plus chère (value)
data_equipe <- data_final %>%
  filter(!is.na(club_position_cat), club_position_cat %in% c("GK", "DEF", "MID", "FWD"))

equipe_chere <- data_equipe %>%
  arrange(desc(value_num)) %>%
  group_by(club_position_cat) %>%
  mutate(
    max_joueurs = case_when(
      club_position_cat == "GK" ~ 1,
      club_position_cat == "DEF" ~ 4,
      club_position_cat == "MID" ~ 4,
      club_position_cat == "FWD" ~ 2,
      TRUE ~ 0
    ),
    rang = row_number()
  ) %>%
  filter(rang <= max_joueurs) %>%
  select(-max_joueurs, -rang) %>%
  ungroup()

cat("\n=== ÉQUIPE LA PLUS CHÈRE ===\n")
print(equipe_chere %>% select(name, club_position_cat, overall_rating, value_num))
cat("Valeur totale:", sum(equipe_chere$value_num, na.rm = TRUE), "€\n")

# 2. Créez l'équipe la plus forte (overall_rating)
equipe_forte <- data_equipe %>%
  arrange(desc(overall_rating)) %>%
  group_by(club_position_cat) %>%
  mutate(
    max_joueurs = case_when(
      club_position_cat == "GK" ~ 1,
      club_position_cat == "DEF" ~ 4,
      club_position_cat == "MID" ~ 4,
      club_position_cat == "FWD" ~ 2,
      TRUE ~ 0
    ),
    rang = row_number()
  ) %>%
  filter(rang <= max_joueurs) %>%
  select(-max_joueurs, -rang) %>%
  ungroup()

cat("\n=== ÉQUIPE LA PLUS FORTE ===\n")
print(equipe_forte %>% select(name, club_position_cat, overall_rating, value_num))
cat("Rating moyen:", mean(equipe_forte$overall_rating, na.rm = TRUE), "\n")

# 3. Comparaison
cat("\n=== COMPARAISON DES ÉQUIPES ===\n")
joueurs_communs <- intersect(equipe_chere$name, equipe_forte$name)
cat("Joueurs en commun:", length(joueurs_communs), "sur 11\n")
if(length(joueurs_communs) > 0) {
  cat("Noms:", paste(joueurs_communs, collapse = ", "), "\n")
} else {
  cat("Aucun joueur en commun\n")
}

# 4. Créer une équipe par ligue ou pays
equipe_premier_league <- data_equipe %>%
  filter(club_league_name == "Premier League") %>%
  arrange(desc(overall_rating)) %>%
  group_by(club_position_cat) %>%
  mutate(
    max_joueurs = case_when(
      club_position_cat == "GK" ~ 1,
      club_position_cat == "DEF" ~ 4,
      club_position_cat == "MID" ~ 4,
      club_position_cat == "FWD" ~ 2,
      TRUE ~ 0
    ),
    rang = row_number()
  ) %>%
  filter(rang <= max_joueurs) %>%
  select(-max_joueurs, -rang) %>%
  ungroup()

cat("\n=== ÉQUIPE PREMIER LEAGUE ===\n")
print(equipe_premier_league %>% select(name, club_position_cat, overall_rating, club_name))

equipe_france <- data_equipe %>%
  filter(country_name == "France") %>%
  arrange(desc(overall_rating)) %>%
  group_by(club_position_cat) %>%
  mutate(
    max_joueurs = case_when(
      club_position_cat == "GK" ~ 1,
      club_position_cat == "DEF" ~ 4,
      club_position_cat == "MID" ~ 4,
      club_position_cat == "FWD" ~ 2,
      TRUE ~ 0
    ),
    rang = row_number()
  ) %>%
  filter(rang <= max_joueurs) %>%
  select(-max_joueurs, -rang) %>%
  ungroup()

cat("\n=== ÉQUIPE FRANCE ===\n")
print(equipe_france %>% select(name, club_position_cat, overall_rating, club_name))

# 5. Créer une équipe prometteuse (jeunes à fort potentiel)
equipe_prometteuse <- data_equipe %>%
  filter(age < 23) %>%
  mutate(marge_progression = potential - overall_rating) %>%
  arrange(desc(marge_progression)) %>%
  group_by(club_position_cat) %>%
  mutate(
    max_joueurs = case_when(
      club_position_cat == "GK" ~ 1,
      club_position_cat == "DEF" ~ 4,
      club_position_cat == "MID" ~ 4,
      club_position_cat == "FWD" ~ 2,
      TRUE ~ 0
    ),
    rang = row_number()
  ) %>%
  filter(rang <= max_joueurs) %>%
  select(-max_joueurs, -rang) %>%
  ungroup()

cat("\n=== ÉQUIPE PROMETTEUSE (Jeunes talents) ===\n")
print(equipe_prometteuse %>% 
        select(name, club_position_cat, age, overall_rating, potential, marge_progression))
cat("Âge moyen:", round(mean(equipe_prometteuse$age, na.rm = TRUE), 2), "ans\n")
cat("Marge de progression moyenne:", round(mean(equipe_prometteuse$marge_progression, na.rm = TRUE), 2), "\n")

# 6. Visualisation comparative des équipes
equipes_comparison <- bind_rows(
  equipe_chere %>% mutate(Type = "Plus Chère"),
  equipe_forte %>% mutate(Type = "Plus Forte"),
  equipe_prometteuse %>% mutate(Type = "Prometteuse")
)

ggplot(equipes_comparison, aes(x = Type, y = overall_rating, fill = Type)) +
  # geom_boxplot() +
  labs(title = "Comparaison des ratings par type d'équipe",
       x = "Type d'équipe", y = "Overall Rating") +
  theme_minimal() +
  theme(legend.position = "none")

# Comparaison des valeurs
ggplot(equipes_comparison %>% filter(!is.na(value_num)), 
       aes(x = Type, y = value_num/1e6, fill = Type)) +
  geom_boxplot() +
  labs(title = "Comparaison des valeurs par type d'équipe",
       x = "Type d'équipe", y = "Valeur (millions €)") +
  theme_minimal() +
  theme(legend.position = "none")
