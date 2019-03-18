calcMMD <- function(dat, formule) {
# dat : dataframe, le jeu de données (forcement de type "table", arrivé au stade où on fait appel a ce script)
# formule : chaîne de caractères, "Anscombe" ou "Freeman", importée depuis l'UI. Formule de transformation angulaire pour les MMD.
# output -> liste de 3 éléments : 1. la matrice symétrique des "vraies valeurs" de MMD ; 2. la matrice avec les valeurs de MMD dans la partie triangulaire supérieure, et les écarts-types dans la partie triangulaire inférieure ; 3. la matrice indiquant le caractère significatif des MMD par des "*".

	nbGroupes <- nrow(dat)/2 # dat étant une 'table', le nombre de groupes est nécessairement égal au nombre de lignes divisé par 2.
	Mat_eff <- dat[1:nbGroupes, ] # la matrice des effectifs
	Mat_prop <- dat[(nbGroupes+1):(2*nbGroupes), ] # la matrice des fréquences

	# 1. FORMULE DE LA TRANSFORMATION ANGULAIRE (Anscombe ou FT) :
	if (formule=="Anscombe") {
		theta <- function(n,p) { asin((n/(n+3/4))*(1-2*p)) }
	} else { # Freeman-Tukey
		theta <- function(n,p) { 0.5*(asin(1-(2*p*n/(n+1)))+asin(1-(2*((p*n)+1)/(n+1)))) }
	}

	# 2. FORMULE SERVANT POUR LE CALCUL DE L'ECART-TYPE DES MMD :
	sdFormula <- function(nA,nB) { (1/(nA+0.5) + 1/(nB+0.5))^2 }

	# 3. APPLIQUER LA CORRECTION DE FT :
	thetaDiff <- function(nA,pA,nB,pB) { (theta(nA,pA) - theta(nB,pB))^2 - (1/(nA+0.5) + 1/(nB+0.5)) }
	
	# 4. CONSTRUCTION DE LA MATRICE DE MMD :
	MMDMatrix <- matrix(0, nrow=nrow(Mat_eff), ncol=nrow(Mat_eff)) # MMDMatrix a autant de lignes et de colonnes qu'on a de groupes dans les donnees
	noms <- rownames(Mat_eff)
	dimnames(MMDMatrix) <- list(substr(noms,3,nchar(noms)), substr(noms,3,nchar(noms))) # On nomme les lignes et colonnes selon les noms de groupes

	for (i in 1:nrow(MMDMatrix)) {
 		for (j in 1:ncol(MMDMatrix)) { # on parcourt les cases (i,j) de MMDMatrix pour la remplir
 
  			MMDVect <- vector("numeric", length(Mat_eff[1,])) 
  			if (j > i) { # on est au-dessus de la diagonale du tableau : on remplit avec les valeurs MMD
   				for (k in 1:length(MMDVect)) { 
    					MMDVect[k] <- thetaDiff(Mat_eff[i,k], Mat_prop[i,k], Mat_eff[j,k], Mat_prop[j,k]) 
   				}
   				MMDMatrix[i, j] <- sum(MMDVect) / length(MMDVect) 
 			} else if (i ==j) { # on est sur la diagonale du tableau
   				MMDMatrix[i, j] <- 0 # on affecte donc une valeur nulle
  			} else { # donc i > j, on est sous la diagonale et on affecte l'écart-type du MMD
   				for (k in 1:length(MMDVect)) { 
    					MMDVect[k] <- sdFormula(Mat_eff[i,k], Mat_eff[j,k]) 
   				}
   				MMDMatrix[i, j] <- sqrt(2*sum(MMDVect)) / length(MMDVect)
  			}
  
 		} 
	}
	
	# 5. AUTRES RESULTATS :
	MMDSym <- MMDMatrix # matrice qui contiendra les valeurs de MMD (symetrique et a diagonale nulle)
	MMDSignif <- round(MMDMatrix,3) # matrice qui contiendra l'info sur la significativite des MMD
	for (i in 1:nrow(MMDMatrix)) {
 		for (j in 1:ncol(MMDMatrix)) { # pour chaque paire de variables...
  			if (i > j) {MMDSym[i,j] <- max(0,MMDMatrix[j,i]) ; MMDSignif[i,j] <- ifelse(MMDMatrix[j,i]>(2*MMDMatrix[i,j]), "*", "NS")}
  			#else if(i < j) {MMDSignif[i,j] <- MMDMatrix[,j]}
  			else if (i==j) {MMDSignif[i,j] <- NA}
  			else {MMDSym[i,j] <- max(0,MMDSym[i,j])} # on remplace (ici et ci-dessus) les valeurs negatives de MMD par des 0.
 		}
	}

	# 6. RETOURNER LES RESULTATS :
	liste_resultats <- list(round(MMDMatrix,6), round(MMDSym,6), MMDSignif)
	names(liste_resultats) <- c("MMDMatrix", "MMDSym", "MMDSignif")
	return(liste_resultats)
}
