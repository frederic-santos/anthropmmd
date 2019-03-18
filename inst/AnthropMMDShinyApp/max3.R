max3 <- function(dat) {
# dat : tableau de type 'table'
# La fonction retourne la valeur maximale admissible pour le slider bar de l'interface graphique (définissant le nombre d'individus par groupe)
# output -> un scalaire.

	mins <- apply(dat[1:(nrow(dat)/2),], MARGIN=2, FUN=min) # les valeurs minimales (en effectifs) associées à chaque variable
 
	return(as.numeric(sort(mins, decreasing=TRUE)[2]))
}
