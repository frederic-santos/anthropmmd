extractGroups <- function(tab, type) {
# tab : datafrale importé depuis l'UI.
# type : chaâine de caractères. Sont-ce des données brutes ('raw') ou resumées ('table') ?
# Fonction permettant d'extraire les groupes présents dans le tableau fourni par l'utilisateur.
# output -> vecteur de chaînes de caractères, portant le nom des groupes.
 
	if (type == "raw") {
		# 1. POUR UN TABLEAU DE DONNEES BRUTES :
		return(levels(tab[,1]))
	} else if (type == "table") { 
		# 2. POUR UN TABLEAU DE DONNEES RESUMÉES : 
		nb_grps <- nrow(tab)/2 # le nombre de groupes
		noms <- rownames(tab)[1:nb_grps] # de la forme N_Group1, N_Group2, ...
		return(substr(noms,3,nchar(noms)))
	}
}
