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


ind1 <- unlist(txrend[2])
ind1pos <- ind1[ind1 >= 0]
ind1neg <- ind1[ind1 < 0]
ind2 <- unlist(txrend[3])
ind2pos <- ind2[ind2 >= 0]
ind2neg <- ind2[ind2 < 0]

# à ce point nous pouvons poser comme moyenne de départ, lorsqu'on est dans 
# l'état 1 (bonne condition):
mu11 <- mean(ind1pos) # mu indice 1, état 1
sigma11 <- var(ind1pos) 
mu21 <- mean(ind2pos) # mu indice 2, état 1
sigma21 <- var(ind2pos)
mu12 <- mean(ind1neg) # mu indice 1, état 2
sigma12 <- var(ind1neg)
mu22 <- mean(ind2neg) # mu indice 2, état 2
sigma22 <- var(ind2neg)