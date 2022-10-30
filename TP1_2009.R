library(readxl)
txrend <- Donne_es_TP1_ACT_2009_Automne_2022 <- read_excel(
  "Documents/TP1_2009/Données TP1 ACT-2009 Automne 2022.xlsx")
colnames(txrend) <- c("date", "ind_1", "ind_2")
View(txrend)
## Attribution arbitraire des paramètres pour une matrice à 2 états :
# État 1 = bonne condition
#   ~N(mu1, sigma1) où mu1 > 0 car rendement positif
# État 2 = mauvaise condition
#   ~N(mu2 sigma2) où nu2 < 0 car rendement négatif

## Pour partir avec une moyenne à peu près représentative, nous calculons la 
# hausse moyenne de l'indice lorsqu'elle augmente et la baisse moyenne de 
# l'indice lorsqu'elle descend


## haussemoy
# x: fichier contenant nos informations (col 1 = tx de rendement, 
#                                           col 2 = indice 1, col 3 = indice 3)
# y: indice (1 ou 2)
# exemple :
# haussemoy(txrend, 1) # calcule l la moyenne de la hausse de l'indice 1 
#     lorsqu'elle augmente d'un mois è l 'autre

haussemoy <- function(x, y) mean(x[y+1] >= 0)

# à ce point nous pouvons poser comme moyenne de départ, lorsqu'on est dans 
# l'état 1 (bonne condition):
mu11 <- haussemoy(txrend, 1) # mu indice 1, état 1
mu21 <- haussemoy(txrend, 2) # mu indice 2, état 1

## Similairement pour un point de départ dans l'état 2 (mauvaise condition)
# nous pouvons créer une fonction calcule la moyenne de baisse
# du tx de rendement lorsque celui-ci baisse :
baissemoy <- function(x, y) mean(x[y+1] < 0)

# et y assigné les variables :
mu12 <- baissemoy(txrend, 1) # mu indice 1, état 2
mu22 <- baissemoy(txrend, 2) # mu indice 2, état 2