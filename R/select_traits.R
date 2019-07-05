select_traits <- function(tab, k = 10, strategy = c("none", "excludeNPT", "excludeQNPT", "excludeNOMD", "keepFisher"), OMDvalue = NULL, groups = NULL, angular = c("Anscombe", "Freeman")) {
### tab: table of sample sizes and frequencies
### k: numeric value, required minimal number of individuals per group
### strategy: scheme of exclusion of non-polymorphic traits
### groups: a factor or character vector, indicating the group to be considered in the analysis
### angular: angular transformation to be used in MMD formula

    strategy <- match.arg(strategy) # to avoid a warning if the user do not specify anything
    angular <- match.arg(angular) # idem
    
#############################################################
### 1. Select a subset of the dataset according to the groups
    nbGroupes <- nrow(tab) / 2 # *initial* number of groups
    if (is.null(groups)) { # if the user did not specify any group, take all of them
        etiquettes <- rownames(tab)[1:nbGroupes]
        groups <- factor(substr(etiquettes, 3, nchar(etiquettes)))
    }
    groupeOK <- substr(rownames(tab), 3, nchar(rownames(tab))) %in% as.character(groups) # boolean, indicated whether each group is to be considered or not
    groupeOK[(nrow(tab)/2 + 1):nrow(tab)] <- groupeOK[1:(nrow(tab)/2)]
    tab <- tab[groupeOK, ]
    nbGroupes <- nrow(tab) / 2 # *final* number of groups after subsetting

###############################################################################################
### 2. Filter the dataset to keep only the traits having at least 'k' individuals in each group
    sel <- rep(NA, ncol(tab)) # initialize an empty boolean
    for (j in 1:ncol(tab)) { # for each trait,
        if (all(tab[1:nbGroupes, j] >= k) == TRUE) { # if there are more than 'k' individuals per group for this trait,
            sel[j] <- TRUE # then keep this trait,
        } else {
            sel[j] <- FALSE # else discard it.
        }
    }

    if (sum(sel) > 1) { # if there are at least 2 traits meeting this criterion, we filter the dataset,
        tab <- tab[ , sel]
    } else {
        return() # else we stop here (a MMD does not make sense if there is only one available trait!)
    }

####################################################################
### 3. Addtional step to discard the traits with too few variability
    OMDs <- compute_omd(data = tab, formule = angular, OMDvalue = OMDvalue)
    if (strategy == "excludeNPT") { # Scheme of exclusion: exclude non-polymorphic traits
        polym <- rep(NA, ncol(tab))
        for (j in 1:ncol(tab)) {
            polym[j] <- ifelse(all(tab[(nbGroupes+1):nrow(tab), j] == 0) | all(tab[(nbGroupes+1):nrow(tab), j] == 1), FALSE, TRUE) # TRUE iff the trait has two levels
        }
        tab <- tab[ , polym] # keep polymorphic traits only
        tabDisplay <- OMDs$Sorted[rownames(OMDs$Sorted) %in% colnames(tab), ]
    } 
    else if (strategy=="excludeQNPT") { # Scheme of exclusion: "quasi-non-polymorphic" traits
        avirer <- rep(NA, ncol(tab))
        for (j in 1:ncol(tab)) {
            tabprov <- round(tab[1:nbGroupes, j] * tab[(nbGroupes+1):nrow(tab), j],1)
            tabprov <- abs(tab[1:nbGroupes, j] - tabprov) 
            avirer[j] <- ifelse(sum(tabprov)<=1 | sum(tabprov)>=(sum(tab[1:nbGroupes, j])-1), FALSE, TRUE) 
        }
        tab <- tab[ , avirer]
        tabDisplay <- OMDs$Sorted[rownames(OMDs$Sorted) %in% colnames(tab), ]
    } 
    else if (strategy=="excludeNOMD") { # Scheme of exclusion: weak contribution to MMD
        tab <- tab[ , rownames(OMDs$Pos)]
        tabDisplay <- OMDs$SortedPos
    } 
    else if (strategy=="keepFisher") { # Scheme of exclusion: keep only those traits that show significant differencies between groups at Fisher's exact test
        tab <- fisherTestTab(tab)$informative
        tabDisplay <- OMDs$Sorted[rownames(OMDs$Sorted) %in% colnames(tab), ]
        
    } else { # Scheme of exclusion: "none", no particular criterion -> no filtering at all.
        tabDisplay <- OMDs$Sorted
    }
    
#########################
### 4. Return the results
    tabDisplay <- as.matrix(tabDisplay)
    colnames(tabDisplay) <- "Overall MD"
    return(list("filtered" = tab, "OMD" = tabDisplay))
}
