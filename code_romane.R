

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
ggplot(map_data_merged, aes(x = long, y = lat, group = group, fill = moyenne_g)) +
  geom_polygon(color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low      = "#cfe2f3",
    high     = "#08306b",
    na.value = "#d9d9d9",
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