gphMDS <- function(mmdval, methodMDS, displayAxes=FALSE, displayGOF=FALSE, dim=2, asp=TRUE) {
# mmdval : matrice (symétrique et à diagonale nulle) de valeurs de MMD
# methodMDS : "MMDS" ou l'une des méthodes SMACOF, dépend du choix de l'utilisateur dans l'interface graphique
# displayAxes : booléen récupéré dans l'interface graphique : afficher ou non les axes sur un graphique 2D
# displayGOF : booléen récupéré depuis l'interface graphique : afficher ou non des valeurs de goodness of fit pour les MDS
# dim : dimension *maximale* admissible pour le calcul des coordonnées par MDS. /!\ In fine, l'espace de sortie peut être de dimension 2 même si dim=3.
# asp : booléen récupéré depuis l'interface graphique : mettre tous les axes à la même échelle ou non
# output -> la fonction ne retourne pas de valeur, mais trace le graphique dans l'UI.

	aspValue <- ifelse(asp==TRUE, 1, NA) # on "traduit" asp de façon attendue par plot et scatterplot3d
	
	if (methodMDS=="MMDS") { 
		################################
		# Choix A : Classical metric MDS
		res.pcoa <- cmdscale(mmdval, k=dim, eig=TRUE) # *liste* de divers résultats concernant le MDS
		coor <- res.pcoa$points # coordonnées de la configuration de points
		varByDim <- apply(coor, MARGIN=2, FUN=sd) # les variabilités le long de chaque axe
		# A.1 : MDS 2D
		if ((dim==2) | (dim==3 & ncol(coor)<3) | (dim==3 & ncol(coor)>=3 & min(varByDim)<2e-16)) { # si l'utilisateur voulait dès le départ un graphe 2D, ou un si un graphe 3D n'est pas possible (parce que seulement 2 eigenvalues >0, ou quasiment pas de variabilité sur l'axe 3)
			if (ncol(coor)>=2 & any(mmdval>0)) { # tout va bien, on affiche le graphique
				plot(coor[,1], coor[,2], pch=16, xlab="", ylab="", axes=displayAxes, main="Classical multidimensional scaling of MMD values", ylim=c(min(coor[,2]), 1.15*max(coor[,2])), asp=aspValue)
				text(coor[,1], coor[,2], pos=3, labels=rownames(coor))
				if (displayGOF==TRUE) { # si l'utilisateur a choisi, dans l'UI, d'afficher la valeur de pseudo-R2 :
					mdsDistances <- dist(coor[,1:2], diag=FALSE, upper=FALSE) # calculer les distances entre points sur le graphique MDS
					mmdDistances <- as.dist(mmdval, diag=FALSE, upper=FALSE)
					rho <- cor(as.vector(as.matrix(mmdDistances)), as.vector(as.matrix(mdsDistances)), method="spearman") # calcul de la corrélation entre distances "réelles" (MMD) et distances post-MDS, sans regarder la diagonale nulle
					legend("topleft", legend=c(paste("Spearman's rho=", round(rho,3), sep=""), paste("Eigenvalue-based GoF=", round(res.pcoa$GOF[1], 3), sep="")))
				}
			} else if (ncol(coor)>=2 & all(mmdval==0)) { # si la matrice d'entrée ne contient que des 0 -> message d'erreur
				plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
				text(x=0, y=0.5, labels="The MMD matrix contains only zeroes.", col="black")
				text(x=0, y=-0.5, labels="Impossible to get a MDS plot.", col="black")
			} else { # ncol(coor)<2, le MDS n'a pas pu être calculé
				plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
				text(x=0, y=0, labels="The representation could not be computed since there is only one positive eigenvalue.", col="black")
			}
		} else { # dim=3 & ncol(coor)>=3 & variabilité ~~suffisante sur l'axe 3, i.e. un graphe 3D est possible
		# A.2 : MDS 3D
			graphe <- scatterplot3d(x=coor[,1], y=coor[,2], z=coor[,3], axis=displayAxes, pch=16, highlight.3d=TRUE, type="h", main="Classical multidimensional scaling of MMD values", xlab="", ylab="", zlab="", asp=aspValue)
			coord.labels <- graphe$xyz.convert(x=coor[,1], y=coor[,2], z=coor[,3])
			text(x=coord.labels$x, y=coord.labels$y, pos=3, labels=rownames(coor))
			if (displayGOF==TRUE) { # si l'utilisateur a choisi, dans l'UI, d'afficher la valeur de pseudo-R2 :
					mdsDistances <- dist(coor[,1:3], diag=TRUE, upper=TRUE) # calculer les distances entre points sur le graphique MDS
					mmdDistances <- as.dist(mmdval, diag=FALSE, upper=FALSE)
					rho <- cor(as.vector(as.matrix(mmdDistances)), as.vector(as.matrix(mdsDistances)), method="spearman") # calcul de la corrélation entre distances "réelles" (MMD) et distances post-MDS, sans regarder la diagonale nulle
					legend("topleft", legend=c(paste("Spearman's rho=", round(rho,3), sep=""), paste("Eigenvalue-based GoF=", round(res.pcoa$GOF[1], 3), sep="")))
			}
		}
		
	} else if (methodMDS!="MMDS") {
		######################
		# Choix B : MDS SMACOF
		resNMDS <- smacofSym(as.dist(mmdval), type=methodMDS) # *liste* de divers résultats concernant le MDS
		coor <- resNMDS$conf # coordonnées de la configuration de points
		varByDim <- apply(coor, MARGIN=2, FUN=sd) # les variabilités le long de chaque axe
		# B.1 : MDS 2D
		if ((dim==2) | (dim==3 & ncol(coor)<3) | (dim==3 & ncol(coor)>=3 & min(varByDim)<2e-16)) { # si l'utilisateur voulait dès le départ un graphe 2D, ou un si un graphe 3D n'est pas possible
			plot(resNMDS, xlab="", ylab="", axes=displayAxes, main=paste("Multidimensional scaling of MMD values (", methodMDS, " type)", sep=""), ylim=c(min(resNMDS$conf[,2]), 1.15*max(resNMDS$conf[,2])), asp=aspValue, cex=1, label.conf=list(cex=1))
			if (displayGOF==TRUE) { # si l'utilisateur a choisi, dans l'UI, d'afficher la valeur de stress :
				mdsDistances <- dist(coor[,1:2], diag=TRUE, upper=TRUE) # calculer les distances entre points sur le graphique MDS
				mmdDistances <- as.dist(mmdval, diag=FALSE, upper=FALSE)
				rho <- cor(as.vector(as.matrix(mmdDistances)), as.vector(as.matrix(mdsDistances)), method="spearman") # calcul de la corrélation entre distances "réelles" (MMD) et distances post-MDS, sans regarder la diagonale nulle
				legend("topleft", legend=c(paste("Spearman's rho=", round(rho,3), sep=""), paste("Stress=", round(resNMDS$stress,3), sep="")))
			}
		} else { # dim=3 & ncol(coor)>=3, i.e. un graphe 3D est possible
		# B.2 : MDS 3D
			graphe <- scatterplot3d(x=coor[,1], y=coor[,2], z=coor[,3], axis=displayAxes, pch=16, highlight.3d=TRUE, type="h", main=paste("Multidimensional scaling of MMD values (", methodMDS, " type)", sep=""), xlab="", ylab="", zlab="", asp=aspValue)
			coord.labels <- graphe$xyz.convert(x=coor[,1], y=coor[,2], z=coor[,3])
			text(x=coord.labels$x, y=coord.labels$y, pos=3, labels=rownames(coor))
			if (displayGOF==TRUE) { # si l'utilisateur a choisi, dans l'UI, d'afficher la valeur de pseudo-R2 :
					mdsDistances <- dist(coor[,1:3], diag=TRUE, upper=TRUE) # calculer les distances entre points sur le graphique MDS
					mmdDistances <- as.dist(mmdval, diag=FALSE, upper=FALSE)
					rho <- cor(as.vector(as.matrix(mmdDistances)), as.vector(as.matrix(mdsDistances)), method="spearman") # calcul de la corrélation entre distances "réelles" (MMD) et distances post-MDS, sans regarder la diagonale nulle
					legend("topleft", legend=c(paste("Spearman's rho=", round(rho,3), sep=""), paste("Stress=", round(resNMDS$stress,3), sep="")))
			}
		}
	} 
}
