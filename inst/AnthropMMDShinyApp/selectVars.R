selectVars <- function(tab, k=10, excludeTraits, OMDvalue=NULL, groups, formule) {
# tab : tableau de type "table"
# k : scalaire, nombre minimal d'individus par groupes pour que le caractère soit retenu dans le calcul du MMD.
# excludeTraits : chaîne importée depuis l'UI ("none", "excludeNPT", "excludeQNPT", ou "excludeNCT") : définit la strategie (éventuelle) d'exclusion automatique de traits insuffisamment polymorphiques
# groups : un facteur ou un vecteur de caractères, indiquant les groupes à retenir pour l'analyse.
# formule : chaîne importée depuis l'UI ("Anscombe" ou "Freeman"), formule de calcul pour les mesures individuelles de divergence
 
 	################################
 	# 0. Filtrer selon les groupes :
	nbGroupes <- nrow(tab)/2 # le nombre *initial* de groupes
	groupeOK <- substr(rownames(tab),3,nchar(rownames(tab))) %in% as.character(groups) # booleen, indique si les groupes font partie de la liste active ou non
	groupeOK[(nrow(tab)/2 +1):nrow(tab)] <- groupeOK[1:(nrow(tab)/2)]
 	tab <- tab[groupeOK, ]
	nbGroupes <- nrow(tab)/2 # le nombre *final* de groupes

	#############################################################################################################################
	# 1. Filtrer pour ne retenir les colonnes suffisamment bien renseignees, i.e. celles qui ont plus de k individus par groupe :
	sel <- rep(NA, ncol(tab)) # futur booleen pour la selection des colonnes
	for (j in 1:ncol(tab)) { # pour chaque variable
		if (all(tab[1:nbGroupes, j]>=k)==TRUE) { # s'il y a plus de k individus dans chaque groupe...
			sel[j] = TRUE # ... retenir la variable...
		} else {
			sel[j] = FALSE # ... sinon non.
		}
	}


	if (sum(sel)>1) { # s'il y a au moins 2 variables satisafaisant les criteres, on effectue le filtrage...
		tab <- tab[ , sel]
 	} else {
 		return() # ... sinon on arrete.
 	}

	###################################################################################################################
	# 2. Selection supplementaire, le cas echeant, pour eliminer des caracteres trop similaires dans tous les groupes :
	IMDs <- calcIMD(tab=tab, formule=formule, OMDvalue=OMDvalue)
	if (excludeTraits=="excludeNPT") { # si on ne retient pas les caracteres non polymorphiques, on procede ci-dessous a leur exclusion
		polym <- rep(NA, ncol(tab))
  		for (j in 1:ncol(tab)) {
  			polym[j] <- ifelse(all(tab[(nbGroupes+1):nrow(tab), j] == 0) | all(tab[(nbGroupes+1):nrow(tab), j] == 1), FALSE, TRUE) # sera TRUE s'il n'y a pas que des 0 ou des 1
  		}
		tab <- tab[ , polym] # on ne retient que les caracteres polymorphiques
		tabDisplay <- IMDs$Sorted[rownames(IMDs$Sorted) %in% colnames(tab), ]
	} 
	  else if (excludeTraits=="excludeQNPT") { # idem ici avec les "quasi non polymorphiques" (i.e. ceux qui ne doivent leur variabilit\'e qu'a l'idiosyncrasie d'un seul individu
		avirer <- rep(NA, ncol(tab))
		for (j in 1:ncol(tab)) {
			tabprov <- round(tab[1:nbGroupes, j] * tab[(nbGroupes+1):nrow(tab), j],1)
			tabprov <- abs(tab[1:nbGroupes, j] - tabprov) 
			avirer[j] <- ifelse(sum(tabprov)<=1 | sum(tabprov)>=(sum(tab[1:nbGroupes, j])-1), FALSE, TRUE) 
  		}
		tab <- tab[ , avirer]
		tabDisplay <- IMDs$Sorted[rownames(IMDs$Sorted) %in% colnames(tab), ]
	} 
	  else if (excludeTraits=="excludeNOMD") { # on elimine tous les traits avec une contribution insuffisante a la MMD
		tab <- tab[ , rownames(IMDs$Pos)]
		tabDisplay <- IMDs$SortedPos
	} 
	  else if (excludeTraits=="keepFisher") { # on elimine tous les traits ne montrant aucune difference significative au test exact de Fisher
	  	tab <- fisherTestTab(tab)$informative
	  	tabDisplay <- IMDs$Sorted[rownames(IMDs$Sorted) %in% colnames(tab), ]
	  
	} else { # il n'y avait aucune strategie particuliere d'exclusion de variables
		tabDisplay <- IMDs$Sorted
	}
	
	##############################
	# 3. Retourner les résultats :
	tabDisplay <- as.matrix(tabDisplay)
	colnames(tabDisplay) <- "Overall MD"
	return(list("TableCalcMMD"=tab, "TableDisplayIMD"=tabDisplay))
	
}
