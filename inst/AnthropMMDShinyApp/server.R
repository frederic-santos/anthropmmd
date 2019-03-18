shinyServer(function(input, output, session) {

	source("calcIMD.R")	
	source("calcMMD.R")
	source("extractGroups.R")
	source("fisherTestTab.R")
	source("gphMDS.R")
	source("max3.R")
	source("rawToTable.R")
	source("selectVars.R")
	source("tableToFreq.R")
	source("validDataMMD.R")
	library(scatterplot3d)
	library(smacof)
	
	myenvg = new.env() # environnement privé au package ; contiendra le jeu de données (vu comme une variable globale)

	#########################################################
	# 1. CHARGER LE JEU DE DONNÉES FOURNI PAR L'UTILISATEUR #
	#########################################################
	observeEvent(input$loadData, {
		if (! is.null(input$file$name)) { # on vérifie que l'utilisateur a bien choisi un fichier !
			if (input$typeData=="raw") { # si c'est un jeu de données brutes, binaires
				dat <- read.table(input$file$datapath, header=input$colNamesRaw, sep=input$fieldSepRaw, na.strings=input$charNA)
				if (input$rowNames) { # s'il y a les noms d'individus...
					dat[,1] <- NULL # on les supprime (ils ne servent à rien ici)
				}		
				dat[,1] <- factor(dat[,1]) # au cas où l'utilisateur aurait juste mis "1", "2", etc., comme noms de groupes, ce ne serait pas reconnu comme facteur. On corrige ça ici.
			} else if (input$typeData=="table") { # si c'est une table d'effectifs et fréquences
				dat <- read.table(input$file$datapath, header=input$colNamesTable, row.names=1, sep=input$fieldSepTable)
			}
			if (validDataMMD(dat, type=input$typeData)) { # on regarde si le fichier est bien valide
				groups <- extractGroups(dat, type=input$typeData) # on extrait les groups presents dans les données
				updateSelectizeInput(session, "selectGroups", choices=groups, selected=groups, server=TRUE) # et on met a jour la liste de sélection des groupes...
				updateNumericInput(session, "MDSdim", value=2, min=2, max=ifelse(length(groups)>3,3,2)) # ... ainsi que la dimension max admissible pour les MDS
				output$text_title_summary <- renderText("Number of individuals and relative frequencies for each active variable within each group")
				output$text_table_MMD <- renderText("MMD values (upper triangular part) and associated SD values (lower triangular part)") # on calcule aussi le titre des tables...
				output$text_table_IMD <- renderText("Overall measure of divergence for each variable, sorted in decreasing order of discriminatory power") 
				output$text_table_MMDSym <- renderText("Symmetrical matrix of MMD values") 
				output$text_table_MMDSignif <- renderText("MMD values (upper triangular part) and their significance (indicated by a * in the lower part; 'NS'='non significant')") # ... jusqu'ici.
				if (input$typeData=="raw") { # si c'est un fichier de données brutes...
					dat <- rawToTable(dat) # on le convertit en table d'effectifs et fréquences
				}
				dat <- tableToFreq(dat)
				assign("dat", dat, envir=myenvg) # on place le jeu de données (qui est desormais forcement de type table) dans l'environnement global
				#updateSliderInput(session, "minNbInd", value=min(c(10, max3(dat))), min=1, max=max3(dat)) # on empêche par la suite la saisie de valeurs "nb min individus" trop élevées au vu des données
			} else { # le fichier n'est pas valide
				showModal(modalDialog(title = "Error", "Invalid file. Please check the field separator, and make sure that there are at least two individuals in each group. Please read the help page of 'StartMMD' for detailed information.", easyClose = TRUE))
			}
		} else { # l'utilisateur avait oublié de choisir un fichier
			showModal(modalDialog(title = "Error", "Please select a file on your computer.", easyClose = TRUE))
		}
	})

	##################
	# 2. ANALYSE MMD #
	##################
	
	dat <- reactive({ # ici, on insère une expression qui retournera en temps réel le jeu de données correspondant aux choix de filtrage de l'utilisateur
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { # si un jeu de données a bien été fourni et qu'il est valide !
						selectVars(get("dat", envir=myenvg), k=as.numeric(input$minNbInd), excludeTraits=as.character(input$exclusionStrategy), groups=as.character(input$selectGroups), formule=as.character(input$formuleMMD), OMDvalue=input$OMDvalue)
					} else { # sinon, s'il n'y a pas de données ou qu'elles sont non-valides,
						return() # on n'affiche rien pour l'instant (évite l'affichage d'erreurs en rouge ou de "résultats vides" en l'absence de fichier correct)
					}
			}) # ce jeu de données est calculé une fois pour toutes, et sera reutilisé a plusieurs reprises ci-dessous (évite des recalculs multiples a differents endroits)
	
	resultatsMMD <- reactive({ # même chose avec les résultats des MMD sur le jeu de données obtenu ci-dessus
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { # si un jeu de données a bien été fourni et qu'il est valide !
						if (ncol(dat()$TableCalcMMD) > 1) { # et s'il reste encore des variables avec les critères de sélection definis !
							calcMMD(dat()$TableCalcMMD, formule=input$formuleMMD)
						} else { 
							return() 
						}
					} else {
						return()
					}
				})
	
	temp <- reactive({ # sera comme dat(), sauf qu'on ne filtre pas en fonction du nb d'individus, on filtre juste en fonction des variables
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { # si un jeu de données a bien été fourni et qu'il est valide !
						tempo <- selectVars(get("dat", envir=myenvg), k=1, excludeTraits=as.character(input$exclusionStrategy), groups=as.character(input$selectGroups), formule=as.character(input$formuleMMD), OMDvalue=input$OMDvalue)
						if (ncol(as.data.frame(tempo$TableCalcMMD))<2) {
							showModal(modalDialog(title="Error", "With the current settings for trait sélection, there is less than two traits suitable for the analysis. Consequently, the MMD will not be calculated. Please change the strategy for trait sélection, or exclude some groups (with very few individuals) from the analysis.", easyClose=FALSE))
							return()
						} else {
							return(tempo)
						}
					} else { # sinon, s'il n'y a pas de données ou qu'elles sont non-valides,
						return() # on n'affiche rien pour l'instant (évite l'affichage d'erreurs en rouge ou de "résultats vides" en l'absence de fichier correct)
					}
			}) # ce jeu de données est calculé une fois pour toutes, et sera reutilisé a plusieurs reprises ci-dessous (évite des recalculs multiples a differents endroits)
	
	# Mise à jour de la réglette :
	calcNbMaxReglette <- reactive({ # sert à calculer dynamiquement la borne max de la réglette *en fonction des groupes en presence* ! (ce nb peut changer en fonction des groupes actifs)
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { # si un jeu de données a bien été chargé...
						max3(temp()$TableCalcMMD)
					} else { # sinon, par défaut,
						return(100) # la valeur maximale est fixée à 100.
					}
	})
	output$regletteNbMinInd <- renderUI({
		sliderInput("minNbInd", label="Only retain the traits with this minimal number of individuals per group", value=min(c(10,calcNbMaxReglette())), min=1, max=calcNbMaxReglette())
	}) # la réglette est par defaut de 1 à 100, puis ensuite elle s'adapte dynamiquement aux groupes actifs du jeu de données
	
	
	####################################################################
	# 2.1. Remplir l'onglet "Summary" avec la table des données filtrees
	output$tableResume <- renderTable(dat()$TableCalcMMD, rownames=TRUE, digits=3)
	# explication (pour rappel) : cette sortie est cachée tant que le fichier n'est pas chargé (grâce a l'expression réactive ci-dessus)

	output$button_download_summary <- renderUI({  # ce bouton n'est generé que lorsque l'utilisateur a uploadé les données et lancé le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_summary", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_summary <- downloadHandler(filename='summary_active_groups.csv', content=function(file) {
		write.csv(dat()$TableCalcMMD, file)
	}) # la fonction déclenchée par le bouton de téléchargement

	output$text_title_pvalFisher <- reactive({
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1 & input$exclusionStrategy=="keepFisher") {
						return("p-values of Fisher's exact tests for pairwise comparisons of frequencies")
					} else {
						return("")
					}
	})
	
	tablePvaleurs <- reactive({
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1 & input$exclusionStrategy=="keepFisher") {
						dataTemp <- selectVars(get("dat", envir=myenvg), k=as.numeric(input$minNbInd), excludeTraits="none", groups=as.character(input$selectGroups), formule=as.character(input$formuleMMD), OMDvalue=input$OMDvalue)$TableCalcMMD
						return(fisherTestTab(dataTemp)$pval)
					} else {
						return()
					}
	})
	
	output$tablePval <- renderTable(tablePvaleurs(), rownames=TRUE, digits=3)
	
	output$button_download_tablePval <- renderUI({  # ce bouton n'est generé que lorsque l'utilisateur a uploadé les données et lancé le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1 & input$exclusionStrategy=="keepFisher") { 
						downloadButton("download_pval", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	
	output$download_pval <- downloadHandler(filename='pairwise_fisher_tests_pvalues.csv', content=function(file) {
		dataTemp <- selectVars(get("dat", envir=myenvg), k=as.numeric(input$minNbInd), excludeTraits="none", groups=as.character(input$selectGroups), formule=as.character(input$formuleMMD), OMDvalue=input$OMDvalue)$TableCalcMMD
		write.csv(fisherTestTab(dataTemp)$pval, file)
	}) #
	
	#######################################################################################
	# 2.2. Remplir l'onglet "MMD Statistics" avec la matrice de MMD et les résultats divers
	# 2.2-a) Matrice de MMD "classique", avec SD :
	output$tableMMD <- renderTable(resultatsMMD()$MMDMatrix, rownames=TRUE, digits=3)
	
	output$button_download_tableMMD <- renderUI({  # ce bouton n'est generé que lorsque l'utilisateur a uploadé les données et lancé le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_tableMMD", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_tableMMD <- downloadHandler(filename='MMD_SD_matrix.csv', content=function(file) {
		write.csv(resultatsMMD()$MMDMatrix, file)
	}) # la fonction déclenchée par le bouton de téléchargement

	# 2.2-b) Matrice des mesures individuelles de divergence :
	output$tableIMD <- renderTable(dat()$TableDisplayIMD, rownames=TRUE, digits=3)
	
	output$button_download_tableIMD <- renderUI({  # ce bouton n'est generé que lorsque l'utilisateur a uploadé les données et lancé le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_tableIMD", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_tableIMD <- downloadHandler(filename='Overall_MD_matrix.csv', content=function(file) {
		write.csv(dat()$TableDisplayIMD, file)
	}) # la fonction déclenchée par le bouton de téléchargement

	# 2.2-c) Matrice symetrique de MMD :
	output$tableMMDSym <- renderTable(resultatsMMD()$MMDSym, rownames=TRUE, digits=3)

	output$button_download_tableMMDSym <- renderUI({  # ce bouton n'est generé que lorsque l'utilisateur a uploadé les données et lancé le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_tableMMDSym", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_tableMMDSym <- downloadHandler(filename='MMD_Sym_matrix.csv', content=function(file) {
		write.csv(resultatsMMD()$MMDSym, file)
	}) # la fonction déclenchée par le bouton de téléchargement

	# 2.2-d) Matrice de signif. des MMD :
	output$tableMMDSignif <- renderTable(resultatsMMD()$MMDSignif, rownames=TRUE, digits=3)
	output$button_download_tableMMDSignif <- renderUI({  # ce bouton n'est generé que lorsque l'utilisateur a uploadé les données et lancé le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_tableMMDSignif", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_tableMMDSignif <- downloadHandler(filename='MMD_Signif_matrix.csv', content=function(file) {
		write.csv(resultatsMMD()$MMDSignif, file)
	}) # la fonction déclenchée par le bouton de téléchargement

	#############################################################################################
	# 2.3. Remplir l'onglet "MDS plot" avec un MDS *seulement s'il y a au moins de trois groupes*
	observeEvent(input$helpMDS, { # bouton d'aide pour le MDS
		showModal(modalDialog(
			title="Graphical options for MDS plots",
			h4("Axes and scales"),
			div("Making all axes use the same scale is strongly recommended in all cases: cf. I. Borg, P. Groenen and P. Mair (2013),",
			span(em("Applied Multidimensional Scaling,")),
			"Springer, chap. 7, p. 79. For a 3D-plot, since the third axis carries generally only a very small percentage of the total variability, you might want to uncheck this option to better visualize the distances along the third axis. In this case, the axes scales must be displayed on the plot, otherwise the plot would be misleading."),
			h4("Goodness of fit values"),
			p("For classical metric MDS, a common statistic is given: the sum of the eigenvalues of the first two axes, divided by the sum of all eigenvalues. It indicates the fraction of the total variance of the data represented in the MDS plot. This statistic comes from the '$GOF' value returned by the function 'cmdscale' (which is the basic R function for PCoA)."),
			p("For SMACOF methods, the statistic given is the '$stress' value returned by the function smacofSym (from the R package smacof). It indicates the final stress-1 value. A value very close to 0 corresponds to a perfect fit."),
			p("For both approaches, a 'rho' value is also given, which is the Spearman's correlation coefficient between real dissimilarities (i.e., MMD values) and distances observed on the MDS plot: cf. G. Dzemyda, O. Kurasova, and J. Zilinskas (2013),",
			span(em("Multidimensional Data Visualization,")),
			"Springer, chap. 2, p. 39-40. A value very close to 1 indicates a perfect fit."),
			easyClose=TRUE,
			size="l"
		))
	})
	
	observeEvent(input$selectGroups, # mise à jour de la dimension max admissible pour les MDS en fonction du nb de groupes retenus :
		updateNumericInput(session, "MDSdim", min=2, max=ifelse(length(input$selectGroups)>3,3,2)) 
	)
	
	output$plotMDS <- renderPlot({
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>2) { # si un jeu de données valide a bien été fourni et qu'on a au moins 3 groupes !
						gphMDS(mmdval=resultatsMMD()$MMDSym, methodMDS=input$methodMDS, displayAxes=input$axesMDSplot, displayGOF=input$checkboxGOFstats, dim=input$MDSdim, asp=input$aspMDSplot)
					} else { # sinon, s'il n'y a pas de données ou qu'elles sont non-valides,
						return() # on n'affiche rien pour l'instant.
					}
	})

	output$button_download_plotMDS <- renderUI({  # ce bouton n'est generé que lorsque l'utilisateur a uploadé les données et lancé le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>2 ) { 
						mmdval <- resultatsMMD()$MMDSym
						mmdtoy <- mmdval; diag(mmdtoy) <- rep(1, nrow(mmdtoy))
						if (input$methodMDS=="MMDS") {
							coor <- cmdscale(mmdval, k=2)
						} else if (input$methodMDS!="MMDS" & all(mmdtoy>0)) {
							coor <- smacofSym(as.dist(mmdval), type=input$methodMDS)$conf
						} else {
							return()
						}
						if (ncol(coor)==2 & any(mmdval>0)) {
							downloadButton("download_plotMDS", "Download this plot [PNG file]")
						} else {
							return()
						}
					} else { 
						return() 
					}
	})

	output$download_plotMDS <- downloadHandler(filename='MDS_plot.png', content=function(file) { # la fonction déclenchée par le bouton de téléchargement
					png(file, width=800, height=800)
						par(cex=1.16)
						gphMDS(mmdval=resultatsMMD()$MMDSym, methodMDS=input$methodMDS, displayAxes=input$axesMDSplot, displayGOF=input$checkboxGOFstats, dim=input$MDSdim, asp=input$aspMDSplot)
					dev.off()
	})
	
	###########################################################################################
	# 2.4. Remplir l'onglet "CAH" avec un dendro *seulement s'il y a au moins de trois groupes*
	output$plotCAH <- renderPlot({
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>2) { # si un jeu de données valide a bien été fourni et qu'on a au moins 3 groupes !
						if (any(resultatsMMD()$MMDSym>0)) {
							distances <- as.dist(resultatsMMD()$MMDSym) 
							plot(hclust(distances, method=input$methodCAH), main="Hierarchical clustering", xlab="")
						} else { 
							plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
							text(x=0, y=0.5, labels="The MMD matrix contains only zeros.", col="black")					
						} 
					} else { # sinon, s'il n'y a pas de données ou qu'elles sont non-valides,
						return() # on n'affiche rien pour l'instant.
					}
	})

	output$button_download_plotCAH <- renderUI({  # ce bouton n'est generé que lorsque l'utilisateur a uploadé les données et lancé le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>2 ) { 
						if (any(resultatsMMD()$MMDSym>0)) {
							downloadButton("download_plotCAH", "Download this plot [PNG file]")
						} else {
							return()
						}
					} else { 
						return() 
					}
	})

	output$download_plotCAH <- downloadHandler(filename='Hierarchical_clustering_MMD.png', content=function(file) { # la fonction déclenchée par le bouton de téléchargement
					distances <- as.dist(resultatsMMD()$MMDSym) 
					png(file, width=900, height=900)
						par(cex=1.16)
						plot(hclust(distances, method=input$methodCAH), main="Hierarchical clustering", xlab="")
					dev.off()
	})	
})
