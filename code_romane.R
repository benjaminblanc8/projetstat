

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