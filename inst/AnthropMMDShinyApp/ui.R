shinyUI(fluidPage(theme="kappa.css",
	
	titlePanel("AnthropMMD — A GUI for Smith's Mean Measure of Divergence"),
	#tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #0098B6}")), # pour régler la couleur de la barre du slider
	tags$style(type = "text/css", "
		.irs-bar {width: 100%; background: #0098B6;}
		.irs-bar-edge {background: #0098B6;}
		.irs-single {color:white; background:#0098B6; font-weight: bold;}
 	"),
 	tags$style(HTML("
		.btn-help.btn {
			display: inline-block;
			padding: 7px 12px;
			font-size: 1em;
			margin: 0 0 0 0;
			vertical-align: middle;
			color: gray;
			font-weight: bold;
			background-color: white;
			border-color: gray;
		}
		.btn-help.btn:hover {
			color: white;
			background-color: #0098B6;
		}
		.btn-help.active {
			color: white;
			background-color: #0098B6;
			border-color: #0098B6;
		}
		"
	)),
	
	sidebarLayout(
		#############################################################
		# I] Le menu de gauche, présentant les options de l'analyse :
		sidebarPanel(
			# 1.a) Choix du type de données :
			fileInput("file", label=h3("1. Data import"), accept=c(".csv", ".txt")),
			radioButtons("typeData", label=strong("Type of dataset"), choices=list("Raw binary dataset"="raw", "Table of n's and absolute frequencies for each group"="table")),
			# 1.b) Specification des info utiles pour l'import des données :
			conditionalPanel(condition="input.typeData == 'raw'", # panneau qui ne s'affiche que pour un "raw dataset"
				fluidRow( # on découpe en lignes et colonnes
					column(6,
						checkboxInput("colNamesRaw", label="Variable names in first line", value=TRUE),
						selectInput("fieldSepRaw", label="Field separator", choices=list("Semicolon (;)"=";", "Comma (,)"=",", "Tabulation"="\t", "Space"=" "))
					),
					column(6,
						checkboxInput("rowNames", label="Row names in first column", value=TRUE),
						textInput("charNA", label="Indicator for missing values", value="")
					)
				)
				#helpText("The first variable should be a group identifier. Only .csv and .txt files are accepted.")
			),
			conditionalPanel(condition="input.typeData == 'table'", # panneau qui ne s'affiche que pour une "table of n's and frequencies"
				fluidRow(
					column(6,
						selectInput("fieldSepTable", label="Field separator", choices=list("Semicolon (;)"=";", "Comma (,)"=",", "Tabulation"="\t", "Space"=" "))
					),
					column(6,
						checkboxInput("colNamesTable", label="Variable names in first line", value=TRUE)
					)
				),
				helpText("No missing values allowed here. Row names are mandatory.")
			),
			actionButton("loadData", "Load dataset", icon=icon("file-upload")), # bouton de chargement de données (server.R attend un clic pour démarrer)
			br(),
			br(),
			
			# 2. Paramètres divers de l'analyse :
			h3("2. Analysis settings"),
			selectizeInput("selectGroups", label=h4("Selection of active groups"), choices=NULL, multiple=TRUE),
			
			radioButtons("formuleMMD", label=h4("Formula"), choices=list("Use Anscombe formula"="Anscombe", "Use Freeman and Tukey formula"="Freeman"), inline=TRUE, selected="Anscombe"),
			
			h4("Trait selection"),
			fluidRow(
				column(6,
					radioButtons("exclusionStrategy", label="Exclusion strategy", choices=list("None"="none", "Exclude nonpolymorphic traits"="excludeNPT", "Exclude quasi-nonpolymorphic traits"="excludeQNPT", "Use Fisher's exact test (may be slow)"="keepFisher", "Exclude traits with overall MD lower than..."="excludeNOMD")),
					conditionalPanel(condition="input.exclusionStrategy == 'excludeNOMD'", # panneau qui ne s'affiche que pour le critère overall MD
						numericInput("OMDvalue", label=NULL, value=0, step=0.05, min=0)
					)
				),
				column(6,
					uiOutput("regletteNbMinInd")
				)
			)
		),

		############################################################
		# II] Le panneau principal, pour l'affichage des résultats :
		mainPanel(
			tabsetPanel(
				# 1. L'onglet d'affichage des données filtrées :
    				tabPanel("Summary",
    					br(),
    					strong(textOutput("text_title_summary")), # cet element est calculé dans server.R seulement après l'importation du fichier : il ne s'affiche donc qu'a ce moment
    					br(),
    					div(style="overflow:auto; width:100%;", tableOutput("tableResume")), # le "div" sert à mettre la table dans une frame avec scrollbar, si elle est large
    					uiOutput("button_download_summary"), # le bouton de téléchargement des résultats n'est calculé / affiché qu'au bout du processus (cf. server.R)
						br(),
						strong(textOutput("text_title_pvalFisher")),
						br(),
						br(),
						div(style="overflow:auto; width:100%;", tableOutput("tablePval")),
						uiOutput("button_download_tablePval")
    				), 
    				# 2. L'onglet d'affichage du résultat des MMD :
				tabPanel("MMD Statistics",
					fluidRow( # grille 2x2 pour l'affichage des résultats
						column(6,
							br(),
    							strong(textOutput("text_table_MMDSym")), # cet element est calculé dans server.R seulemt après l'importation du fichier
    							br(),
    							div(style="overflow:auto; width:100%;", tableOutput("tableMMDSym")),
    							uiOutput("button_download_tableMMDSym"),
    							br(),
    							br(),
    							strong(textOutput("text_table_IMD")), # cet element est calculé dans server.R seulemt après l'importation du fichier
    							br(),
    							div(style="overflow:auto; height:210px; width:50%;", tableOutput("tableIMD")),
    							uiOutput("button_download_tableIMD")
    						),
    						column(6,
							br(),
							strong(textOutput("text_table_MMD")), # cet element est calculé dans server.R seulemt après l'importation du fichier
							br(),
							div(style="overflow:auto; width:100%;", tableOutput("tableMMD")),
							uiOutput("button_download_tableMMD"),
							br(),
							br(),
							strong(textOutput("text_table_MMDSignif")), # cet element est calculé dans server.R seulemt après l'importation du fichier
							br(),
							div(style="overflow:auto; width:100%;", tableOutput("tableMMDSignif")),
							uiOutput("button_download_tableMMDSignif")
								
    						)
    					)
				), 
				# 3. L'onglet d'affichage de l'éventuel graphique MDS :
				tabPanel("MDS plot", 
					helpText("A 2D multidimensional scaling plot (MDS) can be displayed below if and only if there are at least three active groups. For a 3D MDS plot, at least four groups are needed, and the first three eigenvalue found during the MDS calculation must be positive (so that you will not necessarily get a 3D plot even if you allow the maximum dimension of the space to be 3)."),
					br(),
					fluidRow(
						column(6,
							numericInput("MDSdim", label="Maximum dimension of the space", value=2, step=1, min=2, max=3),
							selectInput("methodMDS", label="MDS method", choices=list("Classical metric MDS (a.k.a. PCoA)"="MMDS", "SMACOF, interval type"="interval", "SMACOF, ratio type"="ratio", "SMACOF, ordinal (nonmetric) MDS"="ordinal"), selected="MMDS", multiple=FALSE)
						),
						column(6,
							strong("Graph options"),
							checkboxInput("aspMDSplot", label="Make all axes use the same scale", value=TRUE),
							checkboxInput("axesMDSplot", label="Display axes on the MDS plot", value=TRUE),
							p(checkboxInput("checkboxGOFstats", label="Display goodness of fit statistics on the MDS plot", value=FALSE), actionButton("helpMDS", label="Help", class="btn-help", icon=icon("info-circle")))
						)
					),
					br(),
					plotOutput("plotMDS", width="80%"),
					br(),
					uiOutput("button_download_plotMDS") # le bouton de téléchargement des résultats n'est calculé / affiché qu'au bout du processus (cf. server.R)
				),
				# 4. L'onglet d'affichage de l'éventuel graphique CAH :
				tabPanel("Hierarchical clustering", 
					helpText("A hierarchical clustering using Ward's method is displayed below if and only if there are at least three active groups."),
					br(),
					selectInput("methodCAH", label="Agglomeration method", choices=list("Ward's method (squared distances)"="ward.D2", "Single linkage"="single", "Complete linkage"="complete", "Average linkage (UPGMA)"="average"), selected="ward.D2", multiple=FALSE),
					br(),
					plotOutput("plotCAH", width="80%"),
					br(),
					uiOutput("button_download_plotCAH") # le bouton de téléchargement des résultats n'est calculé / affiché qu'au bout du processus (cf. server.R)
				)
			)
		)
	)

))
