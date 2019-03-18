tableToFreq <- function(tab) {
 # tab : tableau de donnees binaire n individus x p variables binaires ; + une colonne d'identifiant de groupe.
 # les K dernieres lignes de tab sont les effectifs presentant les caracteres : on les convertit (pour affichage) en *pourcentages* de presence des caracteres

	nbGroupes <- nrow(tab)/2 # le nombre de groupes
	tab[(nbGroupes+1):nrow(tab), ] <- tab[(nbGroupes+1):nrow(tab), ] / tab[1:nbGroupes, ] # conversion des effectifs en pourcentages
	return(tab)
}

