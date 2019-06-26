fisherTestTab <- function(tab) {
# tab : données d'entrée, préalablement converties au format 'table' dans server.R
# Cette fonction teste si chaque trait presente une différence significative (au test exact de Fisher) pour au moins deux des groupes étudiés (Harris & Sjovold, 2004).
# output -> liste comporant : le dataframe réduit aux seules variables présentant au moins une différence significative ; et une matrice de p-valeurs pour chaque paire de groupes et chaque trait.

	nbGroupes <- nrow(tab)/2 # le nombre de groupes
	nbVars <- ncol(tab) # le nombre de variables
	nomsGroupes <- substr(rownames(tab)[1:nbGroupes], 3, nchar(rownames(tab)[1:nbGroupes])) # le nom des groupes
	MatRes <- matrix(NA, ncol=nbVars, nrow=nbGroupes*(nbGroupes-1)/2) # matrice de p-valeurs qui dira si chaque trait est informatif ou non pour chaque paire de groupes
	colnames(MatRes) <- colnames(tab)
	rownames(MatRes) <- 1:nrow(MatRes) # initialisation à une valeur idiote pour éviter une erreur ci-dessous (il *faut* que le nom des lignes soit initialisé)
	
	for (j in 1:nbVars) { # pour chaque trait...
		compteurLignes <- 1
		for (k in 1:(nbGroupes-1)) { # ... et chaque paire...
			for (l in (k+1):nbGroupes) { # ... de groupes...
				rownames(MatRes)[compteurLignes] <- paste(nomsGroupes[k], nomsGroupes[l], sep=" - ")
				presGroupeA <- round(tab[k,j] * tab[(k+nbGroupes),j])
				presGroupeB <- round(tab[l,j] * tab[(l+nbGroupes),j])
				absGroupeA <- round(tab[k,j] * (1-tab[(k+nbGroupes),j]))
				absGroupeB <- round(tab[l,j] * (1-tab[(l+nbGroupes),j]))
				mat <- matrix(c(presGroupeA, presGroupeB, absGroupeA, absGroupeB), ncol=2)# ... on construit la matrice a tester...
				MatRes[compteurLignes , j] <- fisher.test(mat)$p.value # ... et on calcule sa p-valeur.
				compteurLignes <- compteurLignes + 1
			}		
		}
	}
	isInformative <- apply(MatRes, MARGIN=2, FUN= function(x) if (any(x<=0.05)) return(TRUE) else return(FALSE))
	return(list(informative=tab[ , isInformative], pval=MatRes))
}

