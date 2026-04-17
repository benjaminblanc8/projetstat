data=read.csv2("data_ocde.csv", sep=",",header=TRUE)
setwd()
data_ocde <- read_csv("projet_stat/data_ocde.csv")
summary(data_ocde$PQSCHOOL)
summary(data_ocde$SCHSUST)
summary(data_ocde$ST059Q01TA)


library(ggplot2)
# Installer ggplot2 si ce n'est pas déjà fait
# install.packages("ggplot2")

library(ggplot2)

# Exemple de données
# Installer ggplot2 si ce n'est pas déjà fait
# install.packages("ggplot2")

library(ggplot2)

# Exemple de données
# Charger ggplot2
library(ggplot2)

# Boxplot : relation entre pv_maths (quantitative) et cnt (qualitative)
ggplot(data_ocde, aes(x =CNT, y = moyenne_maths, fill =CNT)) +
  geom_boxplot() +
  labs(
    title = "Répartition des scores de mathématiques par pays",
    x = "Pays",
    y = "Score en mathématiques"
  ) +
  theme_minimal()


ggplot(data_ocde, aes(x =CNT, y = moyenne_lecture, fill =CNT)) +
  geom_boxplot() +
  labs(
    title = "Répartition des scores de lecture par pays",
    x = "Pays",
    y = "Score en lecture"
  ) +
  theme_minimal()


ggplot(data_ocde, aes(x =CNT, y = moyenne_, fill =CNT)) +
  geom_boxplot() +
  labs(
    title = "Répartition des scores de sciences par pays",
    x = "Pays",
    y = "Score en science"
  ) +
  theme_minimal()

summary(data_ocde$PQSCHOOL)

#lien moyenne et différente variables 

ggplot(data_ocde, aes(x = SCHSUST, y = moyenne_maths)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

library(dplyr)

data_pays <- data_ocde %>%
  group_by(CNT) %>%
  summarise(
    SCHSUST = mean(SCHSUST, na.rm = TRUE),
    moyenne_maths = mean(pv_maths, na.rm = TRUE)
  )

ggplot(data_ocde, aes(x =ST059Q01TA, y = moyenne_maths)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

#pareil amis avec les variables quali 

ggplot(data_ocde, aes(x =ST127Q01TA, y = moyenne_maths, fill =ST127Q01TA)) +
  geom_boxplot() +
  labs(
    title = "",
    x = "ST127Q01TA",
    y = "Moyenne en maths"
  ) +
  theme_minimal()


ggplot(data_ocde, aes(x = ST127Q01TA, fill = ST127Q01TA)) +
  geom_bar() +
  theme_minimal() +
  labs(
    x = "ST127Q01TA",
    y = "Effectif",
    title = "Répartition des modalités de ST127Q01TA"
  )

library(ggplot2)

ggplot(data_ocde, aes(x = factor(ST127Q01TA), y = moyenne_maths, fill = factor(ST127Q01TA))) +
  geom_col() +
  labs(je 
    x = "Réponse à ST127Q01TA",
    y = "Moyenne en maths",
    title = "Moyenne en maths selon la réponse à ST127Q01TA"
  ) +
  theme_minimal()



library(dplyr)
library(ggplot2)

# Résumer les données
data_plot <- data_ocde %>%
  filter(ST127Q01TA %in% c(1,2,3)) %>%       # garder seulement les réponses 1,2,3
  group_by(ST127Q01TA) %>%
  summarise(moyenne_moyenne = mean(pv_maths, na.rm = TRUE))  # moyenne de tous les élèves

# Graphique
ggplot(data_plot, aes(x = factor(ST127Q01TA), y = moyenne_moyenne, fill = factor(ST127Q01TA))) +
  geom_col() +
  labs(
    x = "Réponse à ST127Q01TA",
    y = "Moyenne des élèves en maths",
    title = "Moyenne des élèves selon la réponse à ST127Q01TA"
  ) +
  theme_minimal()

install.packages("dplyr")
library(rlang)
