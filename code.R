library(dplyr)

pisa_oecd_clean=read.table("data_ocde.csv", sep=",", header = TRUE)
data=readRDS("stu_qqq.rds")
  
data_ocde<-data %>%
  filter(OECD==1)

# garder uniquement les élèves OCDE
data_ocde <- data[data$OECD == 1, ]

# moyenne des 10 plausible values par élève
data_ocde$moyenne_sciences <- rowMeans(
  data_ocde[, paste0("PV", 1:10, "SCIE")],
  na.rm = TRUE
)

data_ocde$moyenne_maths <- rowMeans(
  data_ocde[, paste0("PV", 1:10, "MATH")],
  na.rm = TRUE
)

data_ocde$moyenne_lecture <- rowMeans(
  data_ocde[, paste0("PV", 1:10, "READ")],
  na.rm = TRUE
)

# Charger le dictionnaire
dico=read.csv("dico.csv", sep=";")



var2 <- dico %>%
  filter(tolower(trimws(dico[[8]])) == "oui") %>%
  pull(1)


pisa_oecd_clean <- data_ocde %>%
  select(
    CNT,
    moyenne_maths,
    moyenne_lecture,
    moyenne_sciences,
    IMMIG,
    any_of(var2)
  )

pisa_oecd_clean <- data_ocde[
  ,
  c(
    "CNT",
    "moyenne_maths",
    "moyenne_lecture",
    "moyenne_sciences",
    "IMMIG",
    intersect(var2, names(data_ocde))
  )
]

pisa_oecd_clean$DI


write.csv(pisa_oecd_clean, "data_ocde.csv", row.names = FALSE)

table(pisa_oecd_clean$CNT)


correspondance <- data.frame(
  NAME = names(pisa_oecd_clean),
  stringsAsFactors = FALSE
)

dico_unique <- dico[!duplicated(dico$NAME), ]


correspondance <- merge(
  correspondance,
  dico_unique,
  by = "NAME"
)


table(pisa_oecd_clean$CNT)

summary(pisa_oecd_clean$moyenne_maths)
summary(pisa_oecd_clean$moyenne_lecture)
summary(pisa_oecd_clean$moyenne_sciences)
sd(pisa_oecd_clean$moyenne_lecture)
sd(pisa_oecd_clean$moyenne_maths)
sd(pisa_oecd_clean$moyenne_sciences)

par(mfrow = c(1, 3))
par(mfrow = c(1, 3), mar = c(5, 4, 4, 1))

boxplot(
  pisa_oecd_clean$moyenne_lecture,
  col = "lightblue",
  main = "Note en lecture",
  ylab = "Score PISA",
  border = "blue"
)

boxplot(
  pisa_oecd_clean$moyenne_maths,
  col = "lightgreen",
  main = "Note en maths",
  ylab = "Score PISA",
  border = "darkgreen"
)

pct_na <- colMeans(is.na(pisa_oecd_clean)) * 100

boxplot(
  pisa_oecd_clean$moyenne_sciences,
  col = "lightpink",
  main = "Note en sciences",
  ylab = "Score PISA",
  border = "red"
)

pct_na

plot(pisa_oecd_clean$moyenne_maths,pisa_oecd_clean$ESCS,col="pink") #Mettre la transparence au minimum
abline(lm(ESCS ~ moyenne_maths, data = pisa_oecd_clean),col="blue")


cor(pisa_oecd_clean$moyenne_maths,pisa_oecd_clean$ESCS)



dpl

# ton vecteur de % NA (copie le tel quel)
# ton vecteur de % NA (copie le tel quel)
pct_na <- c(
  CNT=0, moyenne_maths=0, moyenne_lecture=0, moyenne_sciences=0, IMMIG=7.54412059,
  BOOKID=0, ST001D01T=0, ST003D03T=4.29703514, ST004D01T=0.02642661,
  ST255Q01JA=3.11935682, ST125Q01NA=4.19945995, ST127Q01TA=9.29437554,
  ST260Q01JA=27.10760714, ST267Q01JA=46.42478410, ST267Q02JA=46.73411100,
  ST294Q01JA=7.44654540, ST295Q02JA=6.87938961, ST295Q03JA=7.23987573,
  ST295Q04JA=9.33299905, ST295Q05JA=6.77537717, ST307Q01JA=57.04557236,
  ST016Q01NA=24.85423012, ST059Q01TA=9.91438455, ST059Q02JA=14.85683890,
  ST272Q01JA=16.15411459, FL170Q02JA=63.20161812, FL170Q03JA=63.23007755,
  WB150Q01HA=79.17176282, PA167Q02HA=86.69318363, PA042Q01TA=93.33846055,
  PROGN=0, DURECEC=26.76982081, EXPO21ST=13.55380357, SCHSUST=35.05998502,
  LEARRES=51.68503542, MISCED=7.59968424, FISCED=9.81342133, HISCED=6.95765305,
  HISEI=12.43473812, HOMEPOS=3.43342696, ESCS=5.93751800, PARINVOL=83.67817805,
  PQSCHOOL=83.67546763
)

# transformation en tableau
table_na <- data.frame(
  Variable = names(pct_na),
  Pourcentage_NA = round(as.numeric(pct_na),2)
)

# affichage trié
table_na <- data.frame(
  Variable = names(pct_na),
  Pourcentage_NA = round(as.numeric(pct_na), 2)
)

table_na

pisa_oecd_clean$ESCS[is.na(pisa_oecd_clean$ESCS)] <- mean(pisa_oecd_clean$ESCS, na.rm = TRUE)
cor(pisa_oecd_clean$ESCS,pisa_oecd_clean$moyenne_maths)
cor(pisa_oecd_clean$ESCS,pisa_oecd_clean$moyenne_lecture)
cor(pisa_oecd_clean$ESCS,pisa_oecd_clean$moyenne_sciences)

pisa_oecd_clean$FISCED[is.na(pisa_oecd_clean$FISCED)] <- mean(pisa_oecd_clean$FISCED, na.rm = TRUE)
cor(pisa_oecd_clean$FISCED,pisa_oecd_clean$moyenne_maths)
cor(pisa_oecd_clean$FISCED,pisa_oecd_clean$moyenne_lecture)
cor(pisa_oecd_clean$FISCED,pisa_oecd_clean$moyenne_sciences)

library(ggplot2)


pisa_oecd_clean$ESCS_cl <- cut(pisa_oecd_clean$ESCS,
                  breaks = quantile(pisa_oecd_clean$ESCS, probs = seq(0,1,0.25), na.rm = TRUE),
                  include.lowest = TRUE)

ggplot(pisa_oecd_clean, aes(x = ESCS_cl, y = moyenne_sciences)) +
  geom_boxplot()

pisa_oecd_clean$HOMEPOS
pisa_oecd_clean$HOMEPOS[is.na(pisa_oecd_clean$HOMEPOS)] <-0
mean(pisa_oecd_clean$HOMEPOS)

cor(pisa_oecd_clean$moyenne_maths,pisa_oecd_clean$HOMEPOS)
cor(pisa_oecd_clean$moyenne_lecture,pisa_oecd_clean$HOMEPOS)
cor(pisa_oecd_clean$moyenne_sciences,pisa_oecd_clean$HOMEPOS)

pisa_oecd_clean$HOMEPOS_cl <- cut(pisa_oecd_clean$ESCS,
                               breaks = quantile(pisa_oecd_clean$HOMEPOS, probs = seq(0,1,0.25), na.rm = TRUE),
                               include.lowest = TRUE)

ggplot(pisa_oecd_clean, aes(x = HOMEPOS_cl, y = moyenne_maths)) +
  geom_boxplot()


#box plot pour les moyennes et les revenus des ménages même si bcpp bcp de v  leurs manquantes

boxplot(moyenne_maths ~ PA042Q01TA,
        data = pisa_oecd_clean,
        xlab = "PA042Q01TA",
        ylab = "Moyenne en maths",
        main = "Box-plot de la moyenne en maths selon revenu du ménage",
        col="lightblue")

boxplot(moyenne_lecture ~ PA042Q01TA,
        data = pisa_oecd_clean,
        xlab = "PA042Q01TA",
        ylab = "Moyenne en lecture",
        main = "Box-plot de la moyenne en lecture selon revenu du ménage",
        col="lightgreen")


boxplot(moyenne_sciences ~ PA042Q01TA,
        data = pisa_oecd_clean,
        xlab = "PA042Q01TA",
        ylab = "Moyenne en sciences",
        main = "Box-plot de la moyenne en science selon revenu du ménage",
        col="purple")
#venez on fait une couleur pour les maths, lkecture et sciences qu'on garde pour tous les graphiques comme ça c'est clair tout au long du projet


#voir comment on traitre les variables NA pour le coeff de corrélat

cor(pisa_oecd_clean$moyenne_maths,pisa_oecd_clean$PA042Q01TA)
cor(pisa_oecd_clean$moyenne_lecture,pisa_oecd_clean$PA042Q01TA)
cor(pisa_oecd_clean$moyenne_sciences,pisa_oecd_clean$PA042Q01TA)

vars <- c(
  "PQSCHOOL","SCHSUST","ST127Q01TA","ST059Q01TA","ST059Q02JA",
  "EXPO21ST",
  "ESCS","HISEI","HOMEPOS","LEARRES",
  "PARINVOL"
)
pisa_oecd_clean

# Variables de résultats
moyennes <- c("moyenne_lecture", "moyenne_maths", "moyenne_sciences")

# Matrice de corrélation r
mat_r <- cor(
  pisa_oecd_clean[, vars],
  pisa_oecd_clean[, moyennes],
  use = "pairwise.complete.obs",
  method = "pearson"
)

vars_factor <- c(
  # Système éducatif
  "ST127Q01TA",   # redoublement
  "PROGN",        # filière
  "CNT",          # pays
  
  # Milieu socio-économique
  "ST255Q01JA",   # nombre de livres (classes)
  "FL170Q02JA",   # argent de poche
  "FL170Q03JA",   # travail rémunéré
  
  "ST294Q01JA",   # petit-déjeuner
  "ST295Q04JA",   # travail pour financer études
  "ST295Q03JA",   # aide familiale
  
  # Environnement / comportements
  "DURECEC",
  "HISCED",
  "FISCED",
  "MISCED",
  "IMMIG",
  "ST004D01T",    # genre
  "ST016Q01NA",   # satisfaction
  "ST260Q01JA",   # absence prolongée
  "ST267Q02JA",   # soutien émotionnel
  "ST295Q02JA",   # temps de travail scolaire
  "ST307Q01JA",   # persévérance
  "WB150Q01HA"    # santé perçue
)

eta <- function(y, x){
  x <- as.factor(x)
  
  ok <- complete.cases(y, x)
  y <- y[ok]
  x <- x[ok]
  
  y_bar <- mean(y)
  
  ni <- tapply(y, x, length)
  mi <- tapply(y, x, mean)
  
  num <- sum(ni * (mi - y_bar)^2)
  den <- sum((y - y_bar)^2)
  
  sqrt(num / den)
}

vars_factor_ok <- intersect(vars_factor, names(pisa_oecd_clean))
moyennes_ok    <- intersect(moyennes, names(pisa_oecd_clean))

mat_eta <- matrix(NA,
                  nrow = length(vars_factor_ok),
                  ncol = length(moyennes_ok))

rownames(mat_eta) <- vars_factor_ok
colnames(mat_eta) <- moyennes_ok

for(v in vars_factor_ok){
  for(m in moyennes_ok){
    mat_eta[v, m] <- eta(
      pisa_oecd_clean[[m]],
      pisa_oecd_clean[[v]]
    )
  }
}

mat_eta


vars_factor_ok <- intersect(vars_factor, names(pisa_oecd_clean))

pisa_oecd_clean[vars_factor_ok] <-
  lapply(pisa_oecd_clean[vars_factor_ok], as.factor)


# Affichage
mat_r

pisa_oecd_clean$FISCED
