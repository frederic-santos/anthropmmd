select_traits <- function(tab, k = 10,
                          strategy = c("none", "excludeNPT", "excludeQNPT", "excludeNOMD", "keepFisher"),
                          OMDvalue = NULL, groups = NULL,
                          angular = c("Anscombe", "Freeman")) {
### tab: table of sample sizes and frequencies
### k: numeric value, required minimal number of individuals per group
### strategy: scheme of exclusion of non-polymorphic traits
### groups: a factor or character vector, indicating the groups to be included
### angular: angular transformation to be used in MMD formula

    strategy <- match.arg(strategy) # avoid a warning if the user gives no arg
    angular <- match.arg(angular)   # idem

    ###############################################################
    ## 1. Select a subset of the dataset according to the groups ##
    ###############################################################
    nb_groups <- nrow(tab) / 2 # *initial* number of groups, before subsetting
    if (is.null(groups)) {     # if the user specified nothing, keep all groups
        etiquettes <- rownames(tab)[1:nb_groups]
        groups <- factor(substr(etiquettes, 3, nchar(etiquettes)))
    }
    ## A boolean indicating whether each group sould be included:
    take_group <- substr(rownames(tab), 3, nchar(rownames(tab))) %in% as.character(groups)
    ## Perform subsetting in data to keep only those groups:
    take_group[(nrow(tab) / 2 + 1):nrow(tab)] <- take_group[1:(nrow(tab) / 2)]
    tab <- tab[take_group, ]
    ## *Final* number of groups after subsetting:
    nb_groups <- nrow(tab) / 2

    ###########################################################################
    ## 2. Filter data to keep only the traits with >=k individuals per group ##
    ###########################################################################
    sel <- rep(NA, ncol(tab)) # initialize an empty boolean
    for (j in seq_len(ncol(tab))) {  # For each trait...
        ## if there are more than 'k' individuals per group for this trait,
        if (all(tab[1:nb_groups, j] >= k) == TRUE) {
            sel[j] <- TRUE           # then keep this trait,
        } else {
            sel[j] <- FALSE          # else discard it.
        }
    }

    ## If at least 2 traits meet this criterion, filter the dataset:
    if (sum(sel) > 1) {
        tab <- tab[, sel]
    } else {
        ## else stop here (a MMD does not make sense if there is only one trait)
        return()
    }

    ######################################################################
    ## 3. Addtional step to discard the traits with too few variability ##
    ######################################################################
    omd <- compute_omd(data = tab, formule = angular, OMDvalue = OMDvalue)

    ## 3.1. If scheme of exclusion = exclude non-polymorphic traits:
    if (strategy == "excludeNPT") {
        polym <- rep(NA, ncol(tab))
        for (j in seq_len(ncol(tab))) {
            polym[j] <- ifelse(all(tab[(nb_groups+1):nrow(tab), j] == 0) | all(tab[(nb_groups+1):nrow(tab), j] == 1), FALSE, TRUE)
            ## (this is TRUE iff the trait has two levels)
        }
        tab <- tab[, polym] # keep polymorphic traits only
        tab_display <- omd$Sorted[rownames(omd$Sorted) %in% colnames(tab), ]
    }

    ## 3.2. If scheme of exclusion = "quasi-non-polymorphic" traits:
    else if (strategy == "excludeQNPT") {
        avirer <- rep(NA, ncol(tab))
        for (j in seq_len(ncol(tab))) {
            tabprov <- round(tab[1:nb_groups, j] * tab[(nb_groups + 1):nrow(tab), j], 1)
            tabprov <- abs(tab[1:nb_groups, j] - tabprov)
            avirer[j] <- ifelse(sum(tabprov) <= 1 | sum(tabprov) >= (sum(tab[1:nb_groups, j])-1), FALSE, TRUE)
        }
        tab <- tab[, avirer]
        tab_display <- omd$Sorted[rownames(omd$Sorted) %in% colnames(tab), ]
    }

    ## 3.3. If scheme of exclusion = weak contribution to MMD:
    else if (strategy == "excludeNOMD") {
        tab <- tab[, rownames(omd$Pos)]
        tab_display <- omd$SortedPos
    }

    ## 3.4. If scheme of exclusionis based on Fisher's exact tests:
    else if (strategy=="keepFisher") {
        tab <- fisherTestTab(tab)$informative
        tab_display <- omd$Sorted[rownames(omd$Sorted) %in% colnames(tab), ]
    }

    ## If scheme of exclusion = "none", then no filtering at all:
    else {
        tab_display <- omd$Sorted
    }

    ###########################
    ## 4. Return the results ##
    ###########################
    tab_display <- as.matrix(tab_display)
    colnames(tab_display) <- "Overall MD"
    return(list("filtered" = tab, "OMD" = tab_display))
}
