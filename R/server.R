server <- shinyServer(function(input, output, session) {
 
    myenvg = new.env() # package environement, will contain the dataset (seen as a global variable)

#################################################
### 1. Read the dataset imported through the UI #
#################################################
    observeEvent(input$loadData, {
        if (! is.null(input$file$name)) { # check that the user has indeed loaded a file!
            if (input$typeData == "raw") { # this dataset is a binary dataset
                dat <- read.table(input$file$datapath, header = input$colNamesRaw, sep = input$fieldSepRaw, na.strings = input$charNA)
                if (input$rowNames) { # if the dataset includes row names, then remove them (they are useless in the MMD workflow)
                    dat[ , 1] <- NULL
                }
		if (ncol(dat) > 2) {
                    dat[ , 1] <- factor(dat[ , 1]) # to be sure that the first column (group indicator) will be recognized as a factor, even if the labels are numeric ('1', '2', etc.)
                } else {
                    showModal(modalDialog(title = "Error", "Invalid file or invalid settings: less than two columns could be found in the data file. Please check the field separator.", easyClose = TRUE))
                }
            } else if (input$typeData == "table") { # this dataset is already a table of frequencies
                dat <- read.table(input$file$datapath, header = input$colNamesTable, row.names = 1, sep = input$fieldSepTable)
            }
            if (valid_data_mmd(dat, type = input$typeData)) { # quick checks: is it a valid data file?
                groups <- extract_groups(dat, type = input$typeData) # retrieve the groups from the data file
                updateSelectizeInput(session, "selectGroups", choices = groups, selected = groups, server = TRUE) # update the UI widget displaying the list of groups...
                updateNumericInput(session, "MDSdim", value = 2, min = 2, max = ifelse(length(groups)>3,3,2)) # ... and maximal admissible dimension for the MDS
                output$text_title_summary <- renderText("Number of individuals and relative frequencies for each active variable within each group")
                output$text_table_MMD <- renderText("MMD values (upper triangular part) and associated SD values (lower triangular part)") # render titles of all tables (passed to UI)
                output$text_table_OMD <- renderText("Overall measure of divergence for each variable, sorted in decreasing order of discriminatory power") 
                output$text_table_MMDSym <- renderText("Symmetrical matrix of MMD values") 
                output$text_table_MMDSignif <- renderText("MMD values (upper triangular part) and their significance (indicated by a * in the lower part; 'NS'='non significant')")
                if (input$typeData == "raw") { # this dataset is already a table of sample sizes and frequencies
                    dat <- binary_to_table(dat, relative = FALSE) # turn it into relative frequencies
                }
                dat <- table_relfreq(dat)
                assign("dat", dat, envir = myenvg) # put the data into the package's global environment
            } else { # this is not a valid data file!
                showModal(modalDialog(title = "Error", "Invalid file. Please check the field separator, and make sure that there are at least two individuals in each group. Please read the help page of 'StartMMD' for detailed information.", easyClose = TRUE))
            }
        } else { # the user provided no data file before clicking on 'Load' button
            showModal(modalDialog(title = "Error", "Please select a file on your computer.", easyClose = TRUE))
        }
    })

#####################
### 2. MMD analysis #
#####################
    
    dat <- reactive({ # reactive expression that will return ('in real time') the filtered dataset, according to user-defined criteria
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { # if a valid data file has been loaded, we return this:
            select_traits(get("dat", envir = myenvg), k = as.numeric(input$minNbInd),
                          strategy = as.character(input$exclusionStrategy), groups = as.character(input$selectGroups),
                          angular = as.character(input$formuleMMD), OMDvalue = input$OMDvalue)
        } else { # either no data file, or invalid data file
            return() # display nothing (avoid unexplicit red error messages in the UI)
        }
    }) # 'dat' is computed once, but will be reused multiple times below (avoid useless re-computations)
    
    resultatsMMD <- reactive({ # reactive expression that will return MMD results computed on 'dat'
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { # if a valid data file has been loaded:
            if (ncol(dat()$filtered) > 1) { # we check that there are at least two remaining traits after applying the user-defined criteria for filtering
                mmd(dat()$filtered, angular = input$formuleMMD)
            } else { 
                return() 
            }
        } else {
            return()
        }
    })
    
    temp <- reactive({ # similar to dat(), except that we do not filter according to 'k' (the number of individuals), only according to the traits
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { # if a valid data file has been loaded:
            tempo <- select_traits(get("dat", envir = myenvg), k = 1,
                                   strategy = as.character(input$exclusionStrategy), groups = as.character(input$selectGroups),
                                   angular = as.character(input$formuleMMD), OMDvalue = input$OMDvalue)
            if (ncol(as.data.frame(tempo$filtered)) < 2) {
                showModal(modalDialog(title = "Error", "With the current settings for trait selection, there is less than two traits suitable for the analysis. Consequently, the MMD will not be calculated. Please change the strategy for trait selection, or exclude some groups (with very few individuals) from the analysis.", easyClose = FALSE))
                return()
            } else {
                return(tempo)
            }
        } else { # either no data file, or invalid data file
            return() # display nothing (avoid unexplicit red error messages in the UI)
        }
    })
    
    ## Update slider bar:
    calcNbMaxReglette <- reactive({ # computes dynamically the upper bound of the slider bar according to the select groups (this bound may change if the select groups change as well)
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { # if a valid data file has been loaded:
            max3(temp()$filtered)
        } else {
            return(15) # the default value is set to 100 if no data file has been loaded
        }
    })
    output$regletteNbMinInd <- renderUI({
        sliderInput("minNbInd", label = "Only retain the traits with this minimal number of individuals per group",
                    value = min(c(10, calcNbMaxReglette())), min = 1, max = calcNbMaxReglette())
    }) # by default, the slider bar ranges from 1 to 15, and then it adapts dynamically to the active groups
    
    
    ## ##############################
    ## 2.1. Fill in the 'Summary' tab
    ## 2.1.1. Summary (summary of filtered dataset):
    output$tableResume <- renderTable(dat()$filtered, rownames = TRUE, digits = 3)
    ## Reminder: this output is rendered only once the data file has been loaded (thanks to the reactive expression dat())

    output$button_download_summary <- renderUI({ # this button is rendered and displayed only once the data have been loaded
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { 
            downloadButton("download_summary", "Download this table [CSV file]")
        } else { 
            return() 
        }
    })
    
    output$download_summary <- downloadHandler(filename = 'summary_active_groups.csv', content = function(file) {
        write.csv(dat()$filtered, file)
    }) # the function triggered by the download button

    ## 2.1.2. Table of p-values for Fisher's exact test
    output$text_title_pvalFisher <- reactive({ # render and display the title of the table of p-values for Fisher's test (passed to UI)
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1 & input$exclusionStrategy == "keepFisher") {
            return("p-values of Fisher's exact tests for pairwise comparisons of frequencies")
        } else {
            return("")
        }
    })
    
    tablePvaleurs <- reactive({ # compute table of p-values for Fisher's exact test (if the radio button "Fisher exact test" is selected in the UI)
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1 & input$exclusionStrategy == "keepFisher") {
            dataTemp <- select_traits(get("dat", envir = myenvg), k = as.numeric(input$minNbInd),
                                      strategy = "none", groups = as.character(input$selectGroups),
                                      angular = as.character(input$formuleMMD), OMDvalue = input$OMDvalue)$filtered
            return(fisherTestTab(dataTemp)$pval)
        } else {
            return()
        }
    })
    
    output$tablePval <- renderTable(tablePvaleurs(), rownames = TRUE, digits = 3) # display table of p-values for Fisher's exact test (passed to UI)
    
    output$button_download_tablePval <- renderUI({ # this button is rendered and displayed only once the data have been loaded
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1 & input$exclusionStrategy == "keepFisher") { 
            downloadButton("download_pval", "Download this table [CSV file]")
        } else { 
            return() 
        }
    })
    
    output$download_pval <- downloadHandler(filename = 'pairwise_fisher_tests_pvalues.csv', content = function(file) {
        dataTemp <- select_traits(get("dat", envir = myenvg), k = as.numeric(input$minNbInd),
                                  strategy = "none", groups = as.character(input$selectGroups),
                                  angular = as.character(input$formuleMMD), OMDvalue = input$OMDvalue)$filtered
        write.csv(fisherTestTab(dataTemp)$pval, file)
    }) # the function triggered by the download button
    
    ## ######################################################
    ## 2.2. Fill in the tab of MMD results (various matrices)
    ## 2.2.1. Matrix of MMD and SD:
    output$tableMMD <- renderTable(resultatsMMD()$MMDMatrix, rownames = TRUE, digits = 3)
    
    output$button_download_tableMMD <- renderUI({ # this button is rendered and displayed only once the data have been loaded
        if (input$loadData>0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { 
            downloadButton("download_tableMMD", "Download this table [CSV file]")
        } else { 
            return() 
        }
    })
    output$download_tableMMD <- downloadHandler(filename = 'MMD_SD_matrix.csv', content = function(file) {
        write.csv(resultatsMMD()$MMDMatrix, file)
    }) # the function triggered by the download button

    ## 2.2.2. Matrix of 'OMD' values:
    output$tableOMD <- renderTable(dat()$OMD, rownames = TRUE, digits = 3)
    
    output$button_download_tableOMD <- renderUI({ # this button is rendered and displayed only once the data have been loaded
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { 
            downloadButton("download_tableOMD", "Download this table [CSV file]")
        } else { 
            return() 
        }
    })
    output$download_tableOMD <- downloadHandler(filename='Overall_MD_matrix.csv', content=function(file) {
        write.csv(dat()$OMD, file)
    }) # the function triggered by the download button

    ## 2.2.3. Symmetrical matrix of MMD values:
    output$tableMMDSym <- renderTable(resultatsMMD()$MMDSym, rownames = TRUE, digits = 3)

    output$button_download_tableMMDSym <- renderUI({ # this button is rendered and displayed only once the data have been loaded 
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { 
            downloadButton("download_tableMMDSym", "Download this table [CSV file]")
        } else { 
            return() 
        }
    })
    output$download_tableMMDSym <- downloadHandler(filename = 'MMD_Sym_matrix.csv', content = function(file) {
        write.csv(resultatsMMD()$MMDSym, file)
    }) # the function triggered by the download button

    ## 2.2.4. Matrix of MMD significance:
    output$tableMMDSignif <- renderTable(resultatsMMD()$MMDSignif, rownames = TRUE, digits = 3)
    
    output$button_download_tableMMDSignif <- renderUI({ # this button is rendered and displayed only once the data have been loaded
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 1) { 
            downloadButton("download_tableMMDSignif", "Download this table [CSV file]")
        } else { 
            return() 
        }
    })
    output$download_tableMMDSignif <- downloadHandler(filename = 'MMD_Signif_matrix.csv', content = function(file) {
        write.csv(resultatsMMD()$MMDSignif, file)
    }) # the function triggered by the download button

    ## #################################################################################
    ## 2.3. Fill in the tab 'MDS plot', *only if there are at least three active groups*
    observeEvent(input$helpMDS, { # help button for MDS info
        showModal(modalDialog(
            title = "Graphical options for MDS plots",
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
            easyClose = TRUE,
            size = "l"
        ))
    })
    
    observeEvent(input$selectGroups, # update maximal admissible dimension for the MDS plot according to the selected groups
                 updateNumericInput(session, "MDSdim", min = 2, max = ifelse(length(input$selectGroups)>3,3,2)) 
                 )
    
    output$plotMDS <- renderPlot({
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 2) { # if a valid dataset has been loaded, with at least 3 active groups
            plot_mmd(data = resultatsMMD()$MMDSym, method = input$methodMDS, axes = input$axesMDSplot, gof = input$checkboxGOFstats,
                     dim = input$MDSdim, asp = input$aspMDSplot)
        } else { # either no data file, or invalid data file
            return() # display nothing
        }
    })

    output$button_download_plotMDS <- renderUI({ # this button is rendered and displayed only once the data have been loaded
        ## Display the button only if the MDS can be computed. So, we check it is the case:
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 2 ) { 
            mmdval <- resultatsMMD()$MMDSym
            mmdtoy <- mmdval
            diag(mmdtoy) <- rep(1, nrow(mmdtoy))
            if (input$methodMDS == "classical") {
                coor <- cmdscale(mmdval, k = 2)
            } else if (input$methodMDS != "classical" & all(mmdtoy > 0)) {
                coor <- smacofSym(as.dist(mmdval), type = input$methodMDS)$conf
            } else {
                return()
            }
            if (ncol(coor) == 2 & any(mmdval > 0)) {
                downloadButton("download_plotMDS", "Download this plot [PNG file]")
            } else {
                return()
            }
        } else { 
            return() 
        }
    })

    output$download_plotMDS <- downloadHandler(filename = 'MDS_plot.png', content = function(file) { # the function triggered by the download button
        png(file, width = 800, height = 800)
        par(cex = 1.16)
        plot_mmd(data = resultatsMMD()$MMDSym, method = input$methodMDS, axes = input$axesMDSplot,
                 gof = input$checkboxGOFstats, dim = input$MDSdim, asp = input$aspMDSplot)
        dev.off()
    }) # the function triggered by the download button
    
    ## ############################################################################
    ## 2.4. Fill in the tab 'CAH', *only if there are at least three active groups*
    output$plotCAH <- renderPlot({
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 2) { # if a valid dataset has been loaded, with at least 3 active groups
            if (any(resultatsMMD()$MMDSym > 0)) {
                distances <- as.dist(resultatsMMD()$MMDSym) 
                plot(hclust(distances, method = input$methodCAH), main = "Hierarchical clustering", xlab = "")
            } else { 
                plot(x = 0, y = 0, xlab = "", ylab = "", axes = FALSE, xlim = c(-2,2), ylim = c(-2,2), pch = "")
                text(x = 0, y = 0.5, labels = "The MMD matrix contains only zeroes.", col = "black")					
            } 
        } else { # either no data file, or invalid data file
            return() # display nothing
        }
    })

    output$button_download_plotCAH <- renderUI({ # this button is rendered and displayed only once the data have been loaded
        if (input$loadData > 0 & exists("dat", envir = myenvg) & length(input$selectGroups) > 2 ) { 
            if (any(resultatsMMD()$MMDSym > 0)) {
                downloadButton("download_plotCAH", "Download this plot [PNG file]")
            } else {
                return()
            }
        } else { 
            return() 
        }
    })

    output$download_plotCAH <- downloadHandler(filename = 'Hierarchical_clustering_MMD.png', content = function(file) {
        distances <- as.dist(resultatsMMD()$MMDSym) 
        png(file, width = 900, height = 900)
        par(cex = 1.16)
        plot(hclust(distances, method = input$methodCAH), main = "Hierarchical clustering", xlab = "")
        dev.off()
    }) # the function triggered by the download button
})
