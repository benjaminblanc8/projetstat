#création moyenne_g

data_ocde$moyenne_g <- rowMeans(data_ocde[, c("moyenne_maths", "moyenne_sciences", "moyenne_lecture")], na.rm = TRUE)


#carte selon réussite scolaire
install.packages(c("ggplot2", "dplyr", "maps", "countrycode"))
library(ggplot2)
library(dplyr)
library(maps)
library(countrycode)

# ── 1. Agréger : une ligne par pays ──────────────────────────────────────────
data_pays <- data_ocde %>%
  group_by(CNT) %>%
  summarise(moyenne_g = mean(moyenne_g, na.rm = TRUE)) %>%
  ungroup()

# ── 2. Convertir CNT en nom de pays ──────────────────────────────────────────
data_pays <- data_pays %>%
  mutate(region = countrycode(CNT, origin = "iso3c", destination = "country.name"))

# ── 3. Fond de carte ──────────────────────────────────────────────────────────
world_map <- map_data("world")

# ── 4. Jointure (beaucoup plus légère maintenant) ────────────────────────────
map_data_merged <- world_map %>%
  left_join(data_pays, by = "region")

# ── 5. Carte ──────────────────────────────────────────────────────────────────
library(ggplot2)
library(grid) # pour unit()

ggplot(map_data_merged, aes(x = long, y = lat, group = group, fill = moyenne_g)) +
  geom_polygon(color = "white", linewidth = 0.2) +  # contours des pays
  scale_fill_gradient(
    low      = "#cfe2f3",
    high     = "#08306b",
    na.value = "white",
    name     = "Moyenne générale"
  ) +
  coord_map("mollweide") +
  labs(
    title    = "Scores moyens des élèves par pays",
    subtitle = "Moyenne des scores en maths, sciences et lecture",
    caption  = "Source : data_ocde"
  ) +
  theme_void() +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5, color = "grey40"),
    legend.position   = "bottom",
    legend.key.width  = unit(2, "cm"),
    legend.title      = element_text(size = 10),
    plot.caption      = element_text(size = 8, color = "grey60")
  )
  -----------------
    
   
  
#graphique de densité conjointe 

# ── Option 1 : Courbes de densité 2D (contours) ──────────────────────────────
ggplot(data_ocde, aes(x = ESCS, y = moyenne_g)) +
  geom_density_2d_filled(alpha = 0.8) +
  geom_density_2d(color = "white", linewidth = 0.3) +
  labs(
    title = "Densité conjointe : ESCS × Moyenne générale",
    x     = "Indice socio-économique (ESCS)",
    y     = "Moyenne générale",
    fill  = "Densité"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# ── Option 2 : Carte de chaleur par bins (geom_bin2d) ────────────────────────
ggplot(data_ocde, aes(x = ESCS, y = moyenne_g)) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(
    low  = "#cfe2f3",
    high = "#08306b",
    name = "Nombre\nd'élèves"
  ) +
  labs(
    title = "Distribution conjointe : ESCS × Moyenne générale",
    x     = "Indice socio-économique (ESCS)",
    y     = "Moyenne générale"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


#graphique réussite scolaire et sexe 
# ── 1. Créer la variable genre ────────────────────────────────────────────────
data_ocde <- data_ocde %>%
  mutate(genre = factor(ST004D01T, 
                        levels = c(1, 2), 
                        labels = c("Fille", "Garçon")))

# ── 2. Graphique en barres ────────────────────────────────────────────────────
data_ocde %>%
  filter(!is.na(genre)) %>%
  group_by(genre) %>%
  summarise(moyenne = mean(moyenne_g, na.rm = TRUE)) %>%
  ggplot(aes(x = genre, y = moyenne, fill = genre)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = round(moyenne, 1)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Fille" = "#f4a582", "Garçon" = "#92c5de")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Moyenne générale par genre",
    x     = "Genre",
    y     = "Moyenne générale"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none"
  )



data_ocde %>%
  filter(!is.na(genre)) %>%
  group_by(genre) %>%
  summarise(moyenne = mean(moyenne_lecture, na.rm = TRUE)) %>%
  ggplot(aes(x = genre, y = moyenne, fill = genre)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = round(moyenne, 1)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Fille" = "#f4a582", "Garçon" = "#92c5de")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Moyenne en lecture par genre",
    x     = "Genre",
    y     = "Moyenne générale"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none"
  )


data_ocde %>%
  filter(!is.na(genre)) %>%
  group_by(genre) %>%
  summarise(moyenne = mean(moyenne_maths, na.rm = TRUE)) %>%
  ggplot(aes(x = genre, y = moyenne, fill = genre)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = round(moyenne, 1)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Fille" = "#f4a582", "Garçon" = "#92c5de")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Moyenne en maths par genre",
    x     = "Genre",
    y     = "Moyenne générale"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none"
  )


data_ocde %>%
  filter(!is.na(genre)) %>%
  group_by(genre) %>%
  summarise(moyenne = mean(moyenne_sciences, na.rm = TRUE)) %>%
  ggplot(aes(x = genre, y = moyenne, fill = genre)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = round(moyenne, 1)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Fille" = "#f4a582", "Garçon" = "#92c5de")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Moyenne en sciences par genre",
    x     = "Genre",
    y     = "Moyenne générale"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none"
  )



#tableau des corrélations propre entre toutes les variables ( format graphique)

# Installer si besoin
install.packages(c("tidyverse", "corrplot", "gt"))

# Charger
library(tidyverse)
library(corrplot)
library(gt)

#sélection des variables
vars <- data_ocde %>%
  select(moyenne_g, PQSCHOOL, SCHSUST, ST059Q01TA, ST059Q02JA,
         EXPO21ST, ESCS, HISEI, PA042Q01TA,
         LEARRES, HOMEPOS, PARINVOL, ST003D03T)

#calcule la matrice des corrélations
cor_matrix <- cor(vars, use = "complete.obs")

#"extrait les corrélations avec moyenne_g"
cor_moyenne <- cor_matrix["moyenne_g", ] %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable")

colnames(cor_moyenne)[2] <- "Correlation"

#tableau propre pour le rapport 
tableau_propre <- cor_moyenne %>%
  arrange(desc(abs(Correlation))) %>%
  gt() %>%
  tab_header(
    title = "Corrélation avec la variable moyenne_g"
  ) %>%
  fmt_number(
    columns = "Correlation",
    decimals = 3
  )

tableau_propre

#heatmap

corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.7, number.cex = 0.6)

#tableau des corrélations propres 
library(dplyr)
library(tibble)
library(gt)

cor_moyenne <- data_ocde %>%
  select(moyenne_g, PQSCHOOL, SCHSUST, ST059Q01TA, ST059Q02JA,
         EXPO21ST, ESCS, HISEI, PA042Q01TA,
         LEARRES, HOMEPOS, PARINVOL, ST003D03T) %>%
  summarise(across(-moyenne_g,
                   ~ cor(.x, moyenne_g, use = "complete.obs"))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Correlation")

#le tableau pour le rapport 
tableau <- cor_moyenne %>%
  arrange(desc(abs(Correlation))) %>%
  gt() %>%
  tab_header(title = "Corrélation des variables avec moyenne_g") %>%
  fmt_number(columns = "Correlation", decimals = 3)

tableau



# pour la partie 2
#creation de niveau_pays la base de donnée qui regroupe les résultats des élèves en fonction de leur pays 
library(dplyr)

niveau_pays <- data_ocde %>%
  group_by(CNT) %>%
  summarise(
    nb_eleve = n(),
    moyenne_min = min(moyenne_g, na.rm = TRUE),
    moyenne_max = max(moyenne_g, na.rm = TRUE),
    moyenne_moyenne = mean(moyenne_g, na.rm = TRUE),
    ecart_type = sd(moyenne_g, na.rm = TRUE)
  ) %>%
  ungroup()

#trier les variables 
niveau_pays <- niveau_pays %>%
  arrange(desc(moyenne_moyenne))


#tableau propre pour le rapport

library(gt)

niveau_pays %>%
  gt() %>%
  tab_header(title = "Statistiques des scores par pays") %>%
  fmt_number(columns = -CNT, decimals = 2)


#graphique des 5 meilleurs pays des 5 pires et de la france 

library(dplyr)
library(ggplot2)


# 1. Classement global
niveau_pays <- niveau_pays %>%
  arrange(desc(moyenne_moyenne)) %>%
  mutate(rang = row_number())

# 2. Sélections
top5 <- niveau_pays %>% slice(1:5)
bottom5 <- niveau_pays %>% slice((n()-4):n())
france <- niveau_pays %>% filter(CNT == "FRA")

# 3. Fusion
selection <- bind_rows(top5, bottom5, france) %>%
  distinct()

# 4. Ajouter type de groupe
selection <- selection %>%
  mutate(
    groupe = case_when(
      CNT == "FRA" ~ "France",
      CNT %in% top5$CNT ~ "Top",
      CNT %in% bottom5$CNT ~ "Bottom"
    )
  )

# 5. Graphique
ggplot(selection, aes(x = reorder(CNT, moyenne_moyenne),
                      y = moyenne_moyenne,
                      fill = moyenne_moyenne)) +
  
  geom_col() +
  
  # Dégradés différents selon groupe
  scale_fill_gradientn(
    colours = c("purple", "grey", "green"),
    guide = "none"
  ) +
  
  # France en rouge par-dessus
  geom_col(data = subset(selection, CNT == "FRA"),
           fill = "red") +
  
  # Label score
  geom_text(aes(label = round(moyenne_moyenne, 1)),
            hjust = -0.1, size = 3) +
  
  # Label rang France
  geom_text(data = subset(selection, CNT == "FRA"),
            aes(label = paste0("Rang France: ", rang)),
            hjust = 1.5, color = "white", fontface = "bold") +
  
  coord_flip() +
  
  labs(
    title = "Classement des pays (Top 5, Bottom 5 et France)",
    x = "Pays",
    y = "Score moyen"
  ) +
  
  theme_minimal()


#création de groupe de pays par niveau 
library(dplyr)

niveau_pays <- niveau_pays %>%
  mutate(
    groupe = case_when(
      moyenne_moyenne >= quantile(moyenne_moyenne, 2/3, na.rm = TRUE) ~ "Fort",
      moyenne_moyenne <= quantile(moyenne_moyenne, 1/3, na.rm = TRUE) ~ "Faible",
      TRUE ~ "Moyen"
    )
  )

#on l'ajoute dans la base de donnée 

data_ocde <- data_ocde %>%
  left_join(niveau_pays %>% select(CNT, groupe), by = "CNT")

#les variables à analyser 
vars <- c("PQSCHOOL", "SCHSUST", "ST059Q01TA", "ST059Q02JA",
          "EXPO21ST", "ESCS", "HISEI", "PA042Q01TA", "LEARRES", "HOMEPOS", "PARINVOL", "ST003D03T")

#identifier les variables similaires dans chaque groupe ( on considère homogèen si son écart type est faible )

resultats <- data_ocde %>%
  group_by(groupe) %>%
  summarise(across(all_of(vars),
                   list(
                     moyenne = ~ mean(.x, na.rm = TRUE),
                     ecart_type = ~ sd(.x, na.rm = TRUE)
                   ),
                   .names = "{.col}_{.fn}"))


#on transforme pour lire plus facilement les variables homogènes 

library(tidyr)
homogeneite <- resultats %>%
  pivot_longer(
    -groupe,
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(moyenne|ecart_type)"
  ) %>%
  arrange(groupe, ecart_type)

library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)

# --- Variables les plus stables ---
stables <- homogeneite %>%
  group_by(groupe) %>%
  slice_min(ecart_type, n = 5) %>%
  mutate(Type = "Stable")
library(dplyr)
library(tidyr)
library(ggplot2)

# Transformation long
homogeneite <- resultats %>%
  pivot_longer(
    -groupe,
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(moyenne|ecart_type)"
  )

# --- Variables les plus stables (écart-type le plus faible intra-groupe)
stables <- homogeneite %>%
  group_by(groupe) %>%
  slice_min(ecart_type, n = 5) %>%
  mutate(Type = "Stable")

# --- Variables qui varient le plus intra-groupe (écart-type le plus élevé)
varient <- homogeneite %>%
  group_by(groupe) %>%
  slice_max(ecart_type, n = 5) %>%
  mutate(Type = "Variable")

# --- Fusion pour graphique
graph_data <- bind_rows(stables, varient)

# --- Graphique
ggplot(graph_data, aes(x = reorder(Variable, ecart_type), 
                       y = ecart_type, 
                       fill = Type)) +
  geom_col() +
  facet_wrap(~groupe, scales = "free_y") +
  coord_flip() +
  labs(title = "Variables les plus stables et les plus variables par groupe de pays",
       x = "Variable",
       y = "Écart-type intra-groupe") +
  scale_fill_manual(values = c("Stable" = "steelblue", "Variable" = "tomato")) +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank())





#tentative d'analyse multivariée

# Packages nécessaires
install.packages(c("FactoMineR", "factoextra", "dplyr", "ggplot2"))

library(FactoMineR)   # ACM + analyses factorielles
library(factoextra)   # Visualisations ggplot2-friendly
library(dplyr)        # Manipulation des données
library(ggplot2)      # Graphiques personnalisés

# Charger votre base (adapter le chemin)
data_ocde <- read.csv("data_ocde.csv", stringsAsFactors = TRUE)

# Sélectionner uniquement les variables catégorielles pertinentes
# Exemples : CSP parents, type établissement, niveau scolaire, etc.
vars_acm <- c("ST127Q01TA", "ST272Q01JA", "PROGN",
              "CNT", "ST255Q01JA", "FL170Q02JA","FL170Q03JA","ST294Q01JA","ST295Q04JA","ST295Q03JA","HISCED","FISCED","MISCED","IMMIG","DURECEC","ST016Q01NA","ST260Q01JA","ST267Q02JA","ST295Q02JA","ST307Q01JA","WB150Q01HA")

data_ocde_acm <- data_ocde[, vars_acm]

# Vérifier qu'il n'y a pas de valeurs manquantes
colSums(is.na(data_ocde_acm))

# Supprimer les lignes avec NA (ou imputer selon vos choix)
data_ocde_acm <- na.omit(data_ocde_acm)

# S'assurer que toutes les variables sont bien en facteur
data_ocde_acm <- mutate(data_ocde_acm, across(everything(), as.factor))


# ACM avec FactoMineR
res_acm <- MCA(data_ocde_acm,
               ncp   = 5,
               graph = FALSE)

# Valeurs propres et % de variance expliquée
get_eigenvalue(res_acm)

# Contributions et cos² des modalités aux axes
res_acm$var$contrib
res_acm$var$cos2

# Coordonnées des individus sur les axes
res_acm$ind$coord


# ---- A. Éboulis des valeurs propres ----
fviz_screeplot(res_acm,
               addlabels = TRUE,
               ylim      = c(0, 50),
               title     = "Variance expliquée par axe")

# ---- B. Nuage des modalités (plan 1-2) ----
fviz_mca_var(res_acm,
             repel       = TRUE,   # évite les chevauchements
             col.var     = "cos2", # couleur = qualité de représentation
             gradient.cols = c("#aaaaaa", "#3B8BD4", "#1D3461"),
             title       = "ACM — nuage des modalités")



#nuage des individus 
# Étape 1 : identifier les lignes complètes sur les variables ACM
vars_acm <- c("ST127Q01TA", "ST272Q01JA", "PROGN",
              "CNT", "ST255Q01JA", "FL170Q02JA", "FL170Q03JA",
              "ST294Q01JA", "ST295Q04JA", "ST295Q03JA", "HISCED",
              "FISCED", "MISCED", "IMMIG", "DURECEC", "ST016Q01NA",
              "ST260Q01JA", "ST267Q02JA", "ST295Q02JA", "ST307Q01JA",
              "WB150Q01HA")

# Alternative avec apply
lignes_completes <- rowSums(is.na(data_ocde[, vars_acm])) == 0
# Étape 2 : récupérer moyenne_g sur ces lignes (depuis data_ocde original)
moyenne_g_filtre <- as.numeric(as.character(data_ocde$moyenne_g[lignes_completes]))

# Étape 3 : discrétiser en 3 niveaux
library(dplyr)

data_ocde <- data_ocde %>%
  mutate(
    niveau = ntile(moyenne_g_filtre, 3),
    niveau = factor(niveau,
                    levels = 1:3,
                    labels = c("Faible", "Moyen", "Fort"))
  )


 #nuage des individus coloré par moyenne_g

library(dplyr)

data_ocde$moyenne_g <- rowMeans(data_ocde[, c("moyenne_maths", "moyenne_sciences", "moyenne_lecture")], na.rm = TRUE)


data_acm <- data_ocde %>%
  select(all_of(vars_acm), moyenne_g) %>%
  drop_na()

library(FactoMineR)

res_acm <- MCA(data_acm[, vars_acm], graph = FALSE)

library(factoextra)

fviz_mca_ind(res_acm,
             label = "none",
             col.ind = data_acm$moyenne_g,
             gradient.cols = c("red", "yellow", "green"),
             alpha.ind = 0.5) +
  labs(color = "Moyenne générale")


# ---- D. Biplot individus + modalités ----
fviz_mca_biplot(res_acm,
                repel     = TRUE,
                ggtheme   = theme_minimal(),
                title     = "Biplot ACM")

# Typologie des individus par CAH sur les axes de l'ACM
res_hcpc <- HCPC(res_acm,
                 nb.clust = -1,
                 graph    = FALSE)


# Top 15 modalités qui contribuent le plus à l'axe 1
fviz_contrib(res_acm,
             choice = "var",
             axes   = 1,
             top    = 15,
             title  = "Contributions à l'axe 1")

# Idem pour l'axe 2
fviz_contrib(res_acm,
             choice = "var",
             axes   = 2,
             top    = 15,
             title  = "Contributions à l'axe 2")