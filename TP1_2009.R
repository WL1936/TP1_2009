txrend <- (Donne_es_TP1_ACT_2009_Automne_2022)
colnames(txrend) <- c("date", "ind_1", "ind_2")
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

haussemoy <- function(x, y)
{
  i <- 1
  cumsum <- 0
  n <- 0
  for (i in seq_along(data))
    if (x[i + 1, y + 1] > x[i, y + 1])
      cumsum <- cumsum + x[i + 1, y + 1] - x[i, y + 1]
      n <- n+1
      cumsum / n
}