library(readxl)
txrend <- Donne_es_TP1_ACT_2009_Automne_2022 <- read_excel(
  "Documents/TP1_2009/Données TP1 ACT-2009 Automne 2022.xlsx")
colnames(txrend) <- c("date", "ind_1", "ind_2")
View(txrend)

# valeur arbitraire

mu1 <- 0.01
mu2 <- -0.02
sig1 <- 0.04
sig2 <- 0.08

# Matrice de transition :
P <- matrix(c(0.95, 0.1, 0.05, 0.9), 2)
P2 <- P%*%P
P4 <- P2%*%P2


## Nous désirons maintenant définir les probabilités initiales d'être dans un 
# état ou l'autre. Pour ce faire, nous prenons la proportion du temps où on a
# enregistré une hausse du rendement, ce qui nous fournira une information 
# plausible pour débuter :
ind1 <- unlist(txrend[2])
ind2 <- unlist(txrend[3])
alpha1 <- sum(ind1 > 0)/length(ind1) # probabilité de débuter dans l'état 1, 
alpha2 <- sum(ind1 < 0)/length(ind1) # probabilité de débuter dans l'état 1

## Pour débuter avec la fonction vraissemblance, on utilise le conditionnement 
# sur chaque état :
# Pr(R(1) = r(1) | Theta(0) = 1) P(Theta(0) = 1) + 
#     Pr(R(1) = r(1) | Theta(0) = 2) P(Theta(0) = 2) =
# Pr(R(1) = r(1) | Theta(0) = 1) alpha1 + 
#     Pr(R(1) = r(1) | Theta(0) = 2) alpha2 
