#II-A
# II — Les vecteurs d'inégalités scolaires sont-ils les mêmes au sein de tous les pays de l'OCDE

Nous avons identifié à l'échelle individuelle les variables qui affecte la trajectoire scolaire d'un élève étudiant dans un pays de l'OCDE. Désormais, nous allons changer d'échelle.Nous souhaitons vérifier si à l'échelle nationale ce sont les mêmes caractéristiques qui définissent le niveau scolaire d'un pays sur la scène internationale. Ainsi nous avons regrouper les pays en trois catégories : fort, moyen et faible selon les résultats des élèves qui y étudient. 


```{r}
library(dplyr)
library(gt)

niveau_pays <- niveau_pays %>%
  mutate(groupe = case_when(
    score_moyen >= quantile(score_moyen, 0.66) ~ "Fort",
    score_moyen >= quantile(score_moyen, 0.33) ~ "Moyen",
    TRUE ~ "Faible"
  ))

niveau_pays %>%
  gt() %>%
  tab_header(title = "Statistiques des scores par pays") %>%
  fmt_number(columns = c(score_min, score_max, score_moyen, ecart_type), decimals = 2) %>%
  tab_style(
    style = cell_fill(color = "#2ecc71"),
    locations = cells_body(rows = groupe == "Fort")
  ) %>%
  tab_style(
    style = cell_fill(color = "#f39c12"),
    locations = cells_body(rows = groupe == "Moyen")
  ) %>%
  tab_style(
    style = cell_fill(color = "#e74c3c"),
    locations = cells_body(rows = groupe == "Faible")
  ) %>%
  tab_style(
    style = list(
      cell_borders(sides = "all", color = "black", weight = px(2)),
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = CNT == "FRA")
  )
```


```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.width=7, fig.height=5}
library(dplyr)
library(ggplot2)

# Agrégation par pays
niveau_pays <- pisa_oecd_clean %>%
  group_by(CNT) %>%
  summarise(
    nb_eleves       = n(),
    score_moyen     = mean(score_global, na.rm = TRUE),
    score_min       = min(score_global, na.rm = TRUE),
    score_max       = max(score_global, na.rm = TRUE),
    ecart_type      = sd(score_global, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(score_moyen)) %>%
  mutate(rang = row_number())

# Sélection Top5 / Bottom5 / France
top5    <- niveau_pays %>% slice(1:5)
bottom5 <- niveau_pays %>% slice((n()-4):n())
france  <- niveau_pays %>% filter(CNT == "FRA")

selection <- bind_rows(top5, bottom5, france) %>%
  distinct() %>%
  mutate(groupe = case_when(
    CNT == "FRA"          ~ "France",
    CNT %in% top5$CNT    ~ "Top 5",
    CNT %in% bottom5$CNT ~ "Bottom 5"
  ))

# Graphique
ggplot(selection, aes(x = reorder(CNT, score_moyen),
                      y = score_moyen,
                      fill = groupe)) +
  geom_col() +
  geom_col(data = subset(selection, CNT == "FRA"), fill = "#e41a1c") +
  geom_text(aes(label = round(score_moyen, 1)), hjust = -0.1, size = 3) +
  geom_text(data = subset(selection, CNT == "FRA"),
            aes(label = paste0("Rang : ", rang)),
            hjust = 1.2, color = "white", fontface = "bold", size = 3) +
  scale_fill_manual(values = c("Top 5" = "#1a9850",
                               "Bottom 5" = "#d73027",
                               "France" = "#e41a1c")) +
  coord_flip() +
  labs(
    title   = "Classement des pays : Top 5, Bottom 5 et France",
    x       = NULL, y = "Score moyen global",
    fill    = NULL,
    caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : le score global est la moyenne des scores en mathématiques, lecture et sciences."
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 0),
    legend.position = "bottom"
  )
```



## A — Le capital économique affecte-t-il la réussite scolaire uniformément au sein de l'OCDE


Nous avons vu dans la première partie qu'à l'échelle individuelle certaines variables économiques étaient liées à la réussite scolaire d'un élève. Nous voulons maintenant savoir si ces mêmes variables influencent le niveau scolaire d'un pays par rapport aux autres dans l'OCDE. Ainsi, nous avons calculer la moyenne pour chaque groupe de pays des variables significatives à l'échelle individuelles. 
```{r}

#tableau avec les moyennes des variables économiques significatives par niveau de pays 
library(gt)

moyennes_groupe %>%
  gt() %>%
  tab_header(title = "Moyennes des variables par groupe de pays") %>%
  fmt_number(columns = -groupe, decimals = 2) %>%
  tab_style(
    style = cell_fill(color = "#2ecc71"),
    locations = cells_body(rows = groupe == "Fort")
  ) %>%
  tab_style(
    style = cell_fill(color = "#f39c12"),
    locations = cells_body(rows = groupe == "Moyen")
  ) %>%
  tab_style(
    style = cell_fill(color = "#e74c3c"),
    locations = cells_body(rows = groupe == "Faible")
  )


library(ggplot2)
library(tidyr)

# Passage en format long pour ggplot
moyennes_long <- moyennes_groupe %>%
  pivot_longer(cols = -groupe, names_to = "variable", values_to = "moyenne")

ggplot(moyennes_long, aes(x = variable, y = moyenne, fill = groupe)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Fort" = "#2ecc71", "Moyen" = "#f39c12", "Faible" = "#e74c3c")) +
  labs(title = "Moyenne des variables par groupe de pays",
       x = "Variable", y = "Moyenne", fill = "Groupe") +
  theme_minimal()



```

Afin de rendre l'analyse du tableau ci dessus plus claire, il est important de rappeler que ESCS est composé d'HOMEPOS et que ces deux variables ainsi que LEARRES sont des variables standardisées. L'indice ESCS est à l'échelle individuelle fortement lié au niveau scolaire. Il est important de prendre des pincettes lors de l'analyse à l'échelle macroéconomque puisqu'on efface les inégalités au sein d'un même pays. On constate à cette échelle qu'une relation existe bel et bien. Il existe un écart de 0,48 point entre le groupe faible et le groupe fort, ce qui reste modeste pour un indice standardisé. On constate également une asymétrie. L'écart entre le groupe faible et moyen est prés de 4 fois supérieur à celui entre le groupe moyen et fort. On peut en conclure que le statut socio-économique joue surtout par le bas. Un faible niveau ESCS semble davantage pénalisant qu'un indice élevé est bénéfique. Ainsi, la pauvreté des milieux scolaires serait un frein plus puissant que la richesse n'en serait un accélérateur. On pourrait aussi imaginer un effet de seuil. Au-delà d'un certain niveau socio-économique, l'ESCS cesse d'être le facteur déterminant du niveau scolaire d'un pays. 

Du fait de la composition de l'indice, on observe une tendance similaire pour les possesions matérielles, avec un effet légèrement plus amplifié.

De plus, à l'échelle individuelle la possession de livres est lié au niveau scolaire. On retrouve également cette intuition à l'échelle nationale. Comme l'indice ESCS l'écart est plus important entre les pays faibles et moyens qu'entre le groupe moyen et fort. Cependant les écarts ne sont pas comparables entre les deux indices.L'indice ESCS est un indice standardisé alors que ST255Q01JA est une variable ordinale codée numérique. Si on se rapporte au code de l'indice, l'entier dont sont le plus proche trois moyennes est 4. Cela correspont à la présence de 26 à 100 livres au domicile familial. De cette manière, l'écart ne semble plus aussi significatif. 

Quant à l'indice LEARRES, il peut être à la fois interpréter comme résultant de l'environnement scolaire et des ressources que l'école met à disposition de l'élève en dehors du temps scolaire que comme représentatif du niveau économique du ménage. A l'échelle individuelle, on constatait une absence totale de corrélation avec le niveau scolaire. Cependant à l'échelle nationale, le résultat est autre et peut être contre intuitif. Une valeur élevé de cet indice signfie que lors des fermetures d'école l'élève à eu accès a davantage de ressources variées qu'une valeur faible. Une valeur faible peut ainsi traduire à la fois un manque d"équipement ou un manque de suivi de la part de l'école. On constate à l'échelle nationale que dans le groupe faible les élèves ont eu accès à davantage de ressources que dans les pays forts et moyens dont la moyenne est similaire à 0,02 point de pourcentage près. Ce résultat interpèle puisque le niveau de capital socio-économique est lié au niveau scolaire. On pourrait penser que autant les pays avec un indice ESCS élevé que ses habitants disposent de davantage de moyens pour permettre le suivi de l'école à distance et qu'ainsi, l'indice LEARRES serait plus élevé pour les pays forts or c'est le contraire. Cela peut s'expliquer par le fait que les pays du groupe faible ont pu être moins touché par les fermetures d'école ou encore que l'indice LEARRES mesure la quantité des ressources pédagogiques à dispositif et non pas la qualité. Enfin, il es possible que les réponses des élèves soient biaisés. Les élèves des pays faibles pourraient avoir surestimés leurs ressources disponibles puisqu'ils répondant par rapport à ce qu'il connaissent. 
