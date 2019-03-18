rawToTable <- function(tab) {
# tab : tableau de données binaire (n individus x p variables binaires) ; + une colonne d'identifiant de groupe.
# Convertit tab en type "table" (descriptif des effectifs, variable par variable, groupe par groupe)
# output -> matrix.

	# 1. Définition de quelques raccourcis / constantes :
	nbVars <- ncol(tab)-1 # le nombre de variables (la 1re colonne ne compte pas car c'est l'id de groupes)
	groupes <- factor(tab[,1]) # la premiere colonne est l'indicateur de groupes
	nbGroupes <- nlevels(groupes) # le nombre de groupes
	nomGroupes <- levels(groupes) # le nom des groupes

	# 2. Création et initialisation de la matrice résultat (vide) :
	MatRes <- matrix(nrow=2*nbGroupes, ncol=nbVars)
	colnames(MatRes) <- colnames(tab)[-1]
	rownames(MatRes) <- c(paste("N_",nomGroupes,sep=""), paste("Freq_",nomGroupes,sep=""))
	
	# 3. Remplissage de la matrice :
	for (j in 1:nbVars) { # pour chaque variable...
		x <- split(tab[,j+1], groupes) # ... on sépare la variable en fonction des groupes
 		for (i in 1:nbGroupes) { # puis pour chaque groupe...
 			MatRes[i,j] <- sum(is.na(x[[i]])==FALSE) # ... on compte le nombre de valeurs non-manquantes...
 			MatRes[i+nbGroupes,j] <- sum(x[[i]]==1, na.rm=TRUE) # ... et la fréquence du trait.
		}
	}

	return(MatRes)
}

