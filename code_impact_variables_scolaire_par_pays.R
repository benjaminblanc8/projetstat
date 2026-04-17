#box plot de la qualité des établissements scolaires selon pays 
```{r, echo=FALSE}
ggplot(pisa_oecd_clean, aes(x = CNT, y = PQSCHOOL, color = CNT)) +
  geom_boxplot(fill = NA, size = 0.8, outlier.shape = 15) +
  stat_summary(fun = median,
               geom = "point",
               shape = 18,
               size = 3,
               color = "black") +
  theme_minimal() +
  labs(title = "Qualité des établissement scolaire",
       x = "Pays",
       y = "établissement scolaire",
       caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : indice standardisé, moyenne 0 et écart-type 1. Un score positif indique une qualité perçue supérieure à la moyenne OCDE.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0))

```
---------------------------
  #box plot des actions des établissements pour soutenir les apprentissages 
  ```{r, echo=FALSE}
ggplot(pisa_oecd_clean, aes(x = CNT, y = SCHSUST, color = CNT)) +
  geom_boxplot(fill = NA, size = 0.8, outlier.shape = 15) +
  stat_summary(fun = median,
               geom = "point",
               shape = 18,
               size = 3,
               color = "black") +
  theme_minimal() +
  labs(title = "Actions de l’établissement pour soutenir les apprentissages",
       x = "Pays",
       y = "Aide",
       caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : indice standardisé, moyenne 0 et écart-type 1. Un score positif indique un effort institutionnel supérieur à la moyenne OCDE.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0))


-------------------------
  # box plot du nb d'heure de de maths par semaine
  
  ```{r, echo=FALSE}
ggplot(pisa_oecd_clean, aes(x = CNT, y = ST059Q01TA, color = CNT)) +
  geom_boxplot(fill = NA, size = 0.8, outlier.shape = 15) +
  stat_summary(fun = median,
               geom = "point",
               shape = 18,
               size = 3,
               color = "black") +
  theme_minimal() +
  labs(title = "Nombre d’heures de mathématiques par semaine",
       x = "Pays",
       y = "nombre d'heure",
       caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : valeur déclarée par l'élève. Le losange noir indique la médiane par pays.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0))
```

--------------------------
  #box plot du nombre d'heure de cours par semaine selon pays 
  
  ```{r, echo=FALSE}
ggplot(pisa_oecd_clean, aes(x = CNT, y = ST059Q02JA, color = CNT)) +
  geom_boxplot(fill = NA, size = 0.8, outlier.shape = 15) +
  stat_summary(fun = median,
               geom = "point",
               shape = 18,
               size = 3,
               color = "black") +
  theme_minimal() +
  labs(title = "Nombre total d’heures de cours par semaine",
       x = "Pays",
       y = "nombre d'heures",
       caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : valeur déclarée par l'élève. Le losange noir indique la médiane par pays.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0))
```
idem

Pour le nombre total d’heures de cours par semaine, on observe une forte hétérogénéité des résultats, notamment au sein même des pays. Les médianes restent toutefois relativement proches, généralement autour de 30 à 35 heures hebdomadaires, ce qui suggère une certaine convergence des systèmes éducatifs. Certains pays, comme le Costa Rica ou la Turquie, se situent légèrement au-dessus de cette moyenne. En revanche, la dispersion varie fortement : des pays comme la Turquie, la Corée du Sud, la Grèce ou le Costa Rica présentent une grande variabilité des horaires, traduisant des différences importantes entre élèves. À l’inverse, d’autres pays comme l’Estonie, l’Espagne ou la Belgique affichent des distributions plus resserrées, indiquant une organisation plus homogène du temps scolaire.

```{r, echo=FALSE}
ggplot(pisa_oecd_clean, aes(x = CNT, y =EXPO21ST, color = CNT)) +
  geom_boxplot(fill = NA, size = 0.8, outlier.shape = 15) +
  stat_summary(fun = median,
               geom = "point",
               shape = 18,
               size = 3,
               color = "black") +
  theme_minimal() +
  labs(title = "Exposition aux mathématiques du XXIe siècle",
       x = "Pays",
       y = "exposition",
       caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : indice standardisé, moyenne 0 et écart-type 1. Mesure l'exposition à des concepts mathématiques contemporains.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none",
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0))
```

------------------------------
  #box plot de l'exposition aux mathématiques au 21 eme siècle selon les pays'

  ```{r, echo=FALSE}
ggplot(
  data = subset(pisa_oecd_clean, !is.na(ST272Q01JA) & !is.na(CNT)),
  aes(x = reorder(CNT, CNT, length), fill = ST272Q01JA)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_minimal(base_size = 12) +
  labs(title = "Répartition de la qualité perçue de l'enseignement (OCDE)",
       x = "Pays",
       y = "Proportion",
       fill = "Catégorie",   caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : 25% des élève en Islande mettent une note de 9 ou plus à la qualité de leur enseignement.") +
  
  theme(
    axis.text.x = element_text(
      angle = 90,         
      hjust = 1,
      size = 8
    ),
    plot.title = element_text(face = "bold")
  )

-----------------
  # c'est quoi cette merde askip c'est la répartition de la qualité de l'enseignement par pays mais je pense ya un soucis dans le code
  
  ```{r, echo=FALSE}
ggplot(
  data = pisa_oecd_clean, aes(x = CNT, 
                              fill = factor(PROGN %% 10, levels = c("1", "2", "3", "4"))) 
) +
  geom_bar(position = "fill") + # "fill" empile les catégories à 100%
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "1" = "#1b9e77",   # En bas
      "2" = "#d95f02",
      "3" = "#7570b3",
      "4" = "#666666"    # En haut
    ),
    labels = c(
      "1" = "Générale",
      "2" = "Technologique",
      "3" = "Professionnelle",
      "4" = "Autres"
    ),
    na.value = "#e0e0e0" # Les NA seront tout en haut par défaut
  ) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Répartition des filières par pays (OCDE)",
    subtitle = "Catégories ordonnées de 1 (bas) à 4 (haut)",
    x = "Pays",
    y = "Proportion",
    fill = "Type de filière",  caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : Environ 88% des élèves en Australie sont en filière générale.") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

```

---------------------------
  # et alors ça c'est vraiment le sommum répartition des filieres pays pays 
  
  ```{r, echo=FALSE}
ggplot(
  data = pisa_oecd_clean, aes(x = CNT, 
                              fill = factor(PROGN %% 10, levels = c("1", "2", "3", "4"))) 
) +
  geom_bar(position = "fill") + # "fill" empile les catégories à 100%
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "1" = "#1b9e77",   # En bas
      "2" = "#d95f02",
      "3" = "#7570b3",
      "4" = "#666666"    # En haut
    ),
    labels = c(
      "1" = "Générale",
      "2" = "Technologique",
      "3" = "Professionnelle",
      "4" = "Autres"
    ),
    na.value = "#e0e0e0" # Les NA seront tout en haut par défaut
  ) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Répartition des filières par pays (OCDE)",
    subtitle = "Catégories ordonnées de 1 (bas) à 4 (haut)",
    x = "Pays",
    y = "Proportion",
    fill = "Type de filière",  caption = "Source : PISA 2022 — OCDE. Champ : élèves de 15 ans des pays membres de l'OCDE.\nNote de lecture : Environ 88% des élèves en Australie sont en filière générale.") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

```