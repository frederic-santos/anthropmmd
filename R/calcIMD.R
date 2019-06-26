calcIMD <- function(tab, formule, OMDvalue=0) {
# tab : dataframe, les données.
# formule : chaîne de caractères, "Anscombe" ou "Freeman"
# OMDvalue : le seuil d'OMD (overall measure of divergence) retenu par l'utilisateur dans l'UI.
# Fonction qui calcule les mesures individuelles de divergence de chaque variable.
# output -> liste de 4 éléments : cf. description ci-dessous.

	nbGroupes <- nrow(tab)/2
	Mat_eff <- tab[1:nbGroupes, ] # la matrice des effectifs
	Mat_prop <- tab[(nbGroupes+1):(2*nbGroupes), ] # la matrice des proportions

	# FORMULE DE LA TRANSFORMATION ANGULAIRE (Anscombe ou FT) :
	if (formule=="Anscombe") {
		theta <- function(n,p) { asin((n/(n+3/4))*(1-2*p)) }
	} else { # Freeman-Tukey
		theta <- function(n,p) { 0.5*(asin(1-(2*p*n/(n+1)))+asin(1-(2*((p*n)+1)/(n+1)))) }
	}
	# APPLIQUER LA CORRECTION DE FT :
	thetadiff <- function(nA,pA,nB,pB) { (theta(nA,pA) - theta(nB,pB))^2 - (1/(nA+0.5) + 1/(nB+0.5)) }

	# CALCUL DES IMD :
	IMDMatrix <- matrix(NA, nrow=ncol(Mat_eff), ncol=1)
	rownames(IMDMatrix) <- colnames(Mat_eff)
	tempMatrix <- matrix(0, nrow=nbGroupes, ncol=nbGroupes) # matrice qui contiendra pour chaque variable l'IMD entre chaque paire de groupe. IMD globale = somme des IMD de groupes.

	for (i in 1:ncol(Mat_eff)) { # pour chaque variable

 		for (j in 1:nbGroupes) { # pour chaque paire de groupes
  			for (k in 1:nbGroupes) {
  				if (j > k) { # seulement si on est dans la partie triangulaire (strictement) inférieure de la matrice
   					tempMatrix[j,k] <- thetadiff(Mat_eff[j,i], Mat_prop[j,i], Mat_eff[k,i], Mat_prop[k,i])
   				}
  			}
 		}

		IMDMatrix[i,1] <- sum(tempMatrix)
	}
	
	# IMDMatrix = les valeurs d'IMD de chaque variable, triées dans l'ordre d'origine du tableau de données
	IMDMatrixSorted <- as.matrix(IMDMatrix[order(IMDMatrix[,1], decreasing=TRUE), ]) # les valeurs triées par ordre décroissant
	IMDMatrixSortedPos <- as.matrix(IMDMatrixSorted[IMDMatrixSorted[,1]>OMDvalue, ]) # les valeurs *supérieures a un seuil donné* triées par ordre décroissant
	IMDMatrixPos <- as.matrix(IMDMatrix[IMDMatrix[,1]>OMDvalue, ]) # les valeurs *supérieures a un seuil donné* dans l'ordre d'origine
	return(list("Matrix"=IMDMatrix, "Pos"=IMDMatrixPos, "Sorted"=IMDMatrixSorted, "SortedPos"=IMDMatrixSortedPos))
}
