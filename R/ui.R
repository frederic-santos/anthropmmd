ui <- shinyUI(fluidPage(theme = "style/kappa.css", # prefix 'style' to locate the CSS file: cf. StartMMD.R
  
  titlePanel("AnthropMMD \u2014 A GUI for Smith's Mean Measure of Divergence"),
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
###################################################################
### 1. Define widgets in the left panel (various analysis settings)
      sidebarPanel(
          ## 1.1. Widgets allowing to choose the type of dataset:
          fileInput("file",
                    label = h3("1. Data import"),
                    accept = c(".csv", ".txt")),
          radioButtons("typeData",
                       label = strong("Type of dataset"),
                       choices = list("Raw binary dataset" = "raw",
                                      "Table of n's and absolute frequencies for each group" = "table")),
      
          ## 1.2. Widgets to specify some details about the data file:
          ## The following panel will be displayed for a "raw dataset" only:
          conditionalPanel(condition = "input.typeData == 'raw'",
                           fluidRow( # create a grid of widgets with 2 columns of equal widths
                               column(6,
                                      checkboxInput("colNamesRaw",
                                                    label = "Variable names in first row",
                                                    value = TRUE),
                                      selectInput("fieldSepRaw",
                                                  label = "Field separator",
                                                  choices = list("Semicolon (;)" = ";",
                                                                 "Comma (,)" = ",",
                                                                 "Tabulation" = "\t",
                                                                 "Space" = " "))
                                      ),
                               column(6,
                                      checkboxInput("rowNames",
                                                    label = "Row names in first column",
                                                    value = TRUE),
                                      textInput("charNA",
                                                label = "Indicator for missing values",
                                                value = "")
                                      )
                           )
                           ),
          ## The following panel will be displayed for a "table of n's and frequencies" only:
          conditionalPanel(condition = "input.typeData == 'table'",
                           fluidRow(
                               column(6,
                                      selectInput("fieldSepTable",
                                                  label = "Field separator",
                                                  choices = list("Semicolon (;)" = ";",
                                                                 "Comma (,)" = ",",
                                                                 "Tabulation" = "\t",
                                                                 "Space" = " ")
                                                  )
                                      ),
                               column(6,
                                      checkboxInput("colNamesTable",
                                                    label = "Variable names in first line",
                                                    value = TRUE)
                                      )
                           ),
                           helpText("No missing values allowed here. Row names are mandatory.")
                           ),
          
          ## Action button to load the data (server.R is waiting a mouse click here to begin)
          actionButton("loadData", "Load dataset", icon=icon("file-upload")),
          ## Blank lines (vertical spacing):
          br(),
          br(),
      
          ## 1.3. Various analysis settings:
          h3("2. Analysis settings"),
          ## Select the active groups:
          selectizeInput("selectGroups",
                         label = h4("Selection of active groups"),
                         choices = NULL,
                         multiple = TRUE),
          ## Select MMD formula:
          radioButtons("formuleMMD",
                       label = h4("Formula"),
                       choices = list("Use Anscombe formula" = "Anscombe",
                                      "Use Freeman and Tukey formula" = "Freeman"),
                       inline = TRUE,
                       selected = "Anscombe"),
          ## Strategy for trait selection:
          h4("Trait selection"),
          fluidRow(
              column(6,
                     radioButtons("exclusionStrategy",
                                  label = "Exclusion strategy",
                                  choices = list("None" = "none",
                                                 "Exclude nonpolymorphic traits" = "excludeNPT",
                                                 "Exclude quasi-nonpolymorphic traits" = "excludeQNPT",
                                                 "Use Fisher's exact test (may be slow)" = "keepFisher",
                                                 "Exclude traits with overall MD lower than..." = "excludeNOMD")
                                  ),
                     ## The following panel will be displayed only if the button "overall MD" is active:
                     conditionalPanel(condition = "input.exclusionStrategy == 'excludeNOMD'",
                                      numericInput("OMDvalue",
                                                   label = NULL,
                                                   value = 0,
                                                   step = 0.05,
                                                   min = 0)
                                      )
                     ),
              column(6,
                     ## Display the slider for the minimum number of individuals:
                     uiOutput("regletteNbMinInd")
                     )
          )
      ),

##########################################
### 2. Main panel (to display the results)
      mainPanel(
          tabsetPanel(
              ## 2.1. First tab, displaying the selected subset of the dataset:
              tabPanel("Summary",
                       br(),
                       strong(textOutput("text_title_summary")), # this element is processed in server.R only after loading the file
                       br(),
                       div(style = "overflow:auto; width:100%;", tableOutput("tableResume")), # the "div" allows to put this element into a proper frame with a scrollbar
                       uiOutput("button_download_summary"), # the download button is processed in server.R, and displayed, only once everything is OK.
                       br(),
                       strong(textOutput("text_title_pvalFisher")),
                       br(),
                       br(),
                       div(style = "overflow:auto; width:100%;", tableOutput("tablePval")),
                       uiOutput("button_download_tablePval")
                       ),
              ## 2.2. Tab of MMD matrices:
              tabPanel("MMD Statistics",
                       fluidRow( # make a 2x2 grid to display the four matrices of results
                           column(6,
                                  br(),
                                  strong(textOutput("text_table_MMDSym")), # this element is processed in server.R only after loading the file
                                  br(),
                                  div(style = "overflow:auto; width:100%;", tableOutput("tableMMDSym")),
                                  uiOutput("button_download_tableMMDSym"),
                                  br(),
                                  br(),
                                  strong(textOutput("text_table_OMD")), # this element is processed in server.R only after loading the file
                                  br(),
                                  div(style = "overflow:auto; height:210px; width:50%;", tableOutput("tableOMD")),
                                  uiOutput("button_download_tableOMD")
                                  ),
                           column(6,
                                  br(),
                                  strong(textOutput("text_table_MMD")), # this element is processed in server.R only after loading the file
                                  br(),
                                  div(style = "overflow:auto; width:100%;", tableOutput("tableMMD")),
                                  uiOutput("button_download_tableMMD"),
                                  br(),
                                  br(),
                                  strong(textOutput("text_table_MMDSignif")), # this element is processed in server.R only after loading the file
                                  br(),
                                  div(style = "overflow:auto; width:100%;", tableOutput("tableMMDSignif")),
                                  uiOutput("button_download_tableMMDSignif")
                                  )
                       )
                       ),
              ## 2.3. Tab of MDS plot:
              tabPanel("MDS plot",
                       helpText("A 2D multidimensional scaling plot (MDS) can be displayed below if and only if there are at least three active groups. For a 3D MDS plot, at least four groups are needed, and the first three eigenvalue found during the MDS calculation must be positive (so that you will not necessarily get a 3D plot even if you allow the maximum dimension of the space to be 3)."),
                       br(),
                       fluidRow(
                           column(6,
                                  numericInput("MDSdim",
                                               label = "Maximum dimension of the space",
                                               value = 2,
                                               step = 1,
                                               min = 2,
                                               max = 3),
                                  selectInput("methodMDS",
                                              label = "MDS method",
                                              choices = list("Classical metric MDS (a.k.a. PCoA)" = "classical",
                                                             "SMACOF, interval type" = "interval",
                                                             "SMACOF, ratio type" = "ratio",
                                                             "SMACOF, ordinal (nonmetric) MDS" = "ordinal"),
                                              selected = "MMDS",
                                              multiple = FALSE)
                                  ),
                           column(6,
                                  strong("Graph options"),
                                  checkboxInput("aspMDSplot",
                                                label = "Make all axes use the same scale",
                                                value = TRUE),
                                  checkboxInput("axesMDSplot",
                                                label = "Display axes on the MDS plot",
                                                value = TRUE),
                                  p(checkboxInput("checkboxGOFstats",
                                                  label = "Display goodness of fit statistics on the MDS plot",
                                                  value = FALSE),
                                    actionButton("helpMDS",
                                                 label = "Help",
                                                 class = "btn-help",
                                                 icon = icon("info-circle"))
                                    )
                                  )
                       ),
                       br(),
                       plotOutput("plotMDS", width = "80%"),
                       br(),
                       uiOutput("button_download_plotMDS") # the download button is processed in server.R, and displayed, only once everything is OK.
                       ),
              ## 2.4. Hierarchical clustering tab:
              tabPanel("Hierarchical clustering",
                       helpText("A hierarchical clustering using Ward's method is displayed below if and only if there are at least three active groups."),
                       br(),
                       selectInput("methodCAH",
                                   label = "Agglomeration method",
                                   choices = list("Ward's method (squared distances)" = "ward.D2",
                                                  "Single linkage" = "single",
                                                  "Complete linkage" = "complete",
                                                  "Average linkage (UPGMA)" = "average"),
                                   selected = "ward.D2",
                                   multiple = FALSE),
                       br(),
                       plotOutput("plotCAH", width = "80%"),
                       br(),
                       uiOutput("button_download_plotCAH") # the download button is processed in server.R, and displayed, only once everything is OK.
                       )
          )
      )
  )
))
