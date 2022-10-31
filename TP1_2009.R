library(readxl)
txrend <- Donne_es_TP1_ACT_2009_Automne_2022 <- read_excel(
  "/Users/mcsommeliers/Documents/TP1_2009/Données TP1 ACT-2009 Automne 2022.xlsx")
colnames(txrend) <- c("date", "ind_1", "ind_2")
View(txrend)

# valeur arbitraire

mu1 <- 0.01
mu2 <- -0.02
sig1 <- 0.04
sig2 <- 0.08

# Matrice de transition :
P <- matrix(c(0.95, 0.1, 0.05, 0.9), 2)

## Nous désirons maintenant définir les probabilités initiales d'être dans un 
# état ou l'autre. Pour ce faire, nous prenons la probabilité limite à partir 
# de la matrice de transition
P2 <- P%*%P
P3 <- P%*%P2
P4 <- P2%*%P2
P8 <- P4%*%P4
P16 <- P8%*%P8
P32 <- P16%*%P16
P64 <- P32%*%P32
plimit <- matrix(c(2/3, 1/3))

## Pour débuter avec la fonction vraisemblance, on utilise le conditionnement 
# sur chaque état :
# Pr(R(1) = r(1) | Theta(0) = 1) P(Theta(0) = 1) + 
#     Pr(R(1) = r(1) | Theta(0) = 2) P(Theta(0) = 2) =
# Pr(R(1) = r(1) | Theta(0) = 1) alpha1 + 
#     Pr(R(1) = r(1) | Theta(0) = 2) alpha2 

# Noter que la dernière entrée de l'année 2015 correspond à la ligne 552

# Voici la première itération 
# L1 = vraisemblance d"être dans l'état 1 au temps x, x = temps, i = indice



# Ensuite on recommence le calcul pour le temps 2 en remplaçant les probabilités
# limites par pi1 et pi2 et ainsi de suite pour toute notre période d'intérêt

# Il serait intéressant de créer une fonction pour calculer rapidement la 
# vraisemblance.

# x: données utilisé (indice 1 ou 2)
# y: état de base (1 ou 2)
# t: indice de temps, correspondant à une date (entre 1 et 624)

ind1 <- unlist(txrend[2])
ind1_2015 <- ind1[-(553:624)]
Ltotx1i1 <- vector()
L1x1i1 <- (plimit[1] * P[1, 1]) + (plimit[2] * P[2, 1]) * 
  dnorm((ind1_2015[1] - mu1)/sig1)
L2x1i1 <- (plimit[2] * P[2, 1]) + (plimit[2] * P[2, 2]) * 
  dnorm((ind1_2015[1] - mu2)/sig2)
Ltotx1i1[1] <- L1x1i1 + L2x1i1 # L total pour l'indice 1 au temps 1
# Ensuite, on détermine Pr(Theta(1) = 1) et P(Theta(1) = 2)
pi1 <- L1x1i1/(Ltotx1i1[1])
pi2 <- L2x1i1/(Ltotx1i1[1])

L1x2i1 <- (pi1 * P[1, 1]) + (pi2 * P[2, 1]) * 
  dnorm((ind1[2] - mu1)/sig1)
L2x2i1 <- (pi2 * P[2, 1]) + (pi2 * P[2, 2]) * 
  dnorm((ind1[2] - mu1)/sig1)
Ltotx2i1 <- L1x2i1 + L2x2i1

vrais <- function(indice)
{
  x <- indice
  Ltot <- numeric(length(x))
  L1t <- (plimit[1] * P[1, 1]) + (plimit[2] * P[2, 1]) * 
    dnorm((x[1] - mu1)/sig1)
  L2t <- (plimit[2] * P[2, 1]) + (plimit[2] * P[2, 2]) * 
    dnorm((x[1] - mu2)/sig2)
  Ltot[1] <- L1t + L2t
  pi1 <- L1t/Ltot[1]
  pi2 <- L2t/Ltot[1]
  x1 <- x[-1]
  L1t <- (pi1 * P[1, 1]) + (pi2 * P[2, 1]) * 
    dnorm((x - mu1)/sig1)
  L2t <- (pi2 * P[2, 1]) + (pi2 * P[2, 2]) * 
    dnorm((x - mu1)/sig1)
  Ltot <- L1t + L2t
  pi1 <- L1t/Ltot[1]
  pi2 <- L2t/Ltot[1]
  print(Ltot)
}
  