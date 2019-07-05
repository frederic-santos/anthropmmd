fisherTestTab <- function(tab) {
### Checks whether each trait in 'tab' exhibits a significant difference at Fisher's exact test for at least one pair of groups
### tab: data in 'table' format
### output -> a list with two components

    ## 1. Set some constants:
    nb_groups <- nrow(tab) / 2 # number of groups
    nb_traits <- ncol(tab) # number of traits
    group_names <- extract_groups(tab, type = "table")
    MatRes <- matrix(NA, ncol = nb_traits, nrow = nb_groups*(nb_groups-1)/2) # initialize an empty matrix of p-values
    colnames(MatRes) <- colnames(tab)
    rownames(MatRes) <- 1:nrow(MatRes) # dumb initialization to avoid an error in subsequent commands

    ## 2. Fisher's exact tests:
    for (j in 1:nb_traits) { # for each trait,
        compteurLignes <- 1
        for (k in 1:(nb_groups-1)) { # and each pair of groups,
            for (l in (k+1):nb_groups) {
                rownames(MatRes)[compteurLignes] <- paste(group_names[k], group_names[l], sep = " - ")
                presGroupA <- round(tab[k,j] * tab[(k+nb_groups), j])
                presGroupB <- round(tab[l,j] * tab[(l+nb_groups), j])
                absGroupA <- round(tab[k,j] * (1-tab[(k+nb_groups), j]))
                absGroupB <- round(tab[l,j] * (1-tab[(l+nb_groups), j]))
                mat <- matrix(c(presGroupA, presGroupB, absGroupA, absGroupB), ncol = 2) # build the confusion matrix,
                MatRes[compteurLignes , j] <- fisher.test(mat)$p.value # and run Fisher's exact test
                compteurLignes <- compteurLignes + 1
            }		
        }
    }

    ## 3. Return the results:
    isInformative <- apply(MatRes, MARGIN = 2, FUN = function(x) return(ifelse(any(x <= 0.05), TRUE, FALSE)))
    return(list(informative = tab[ , isInformative], pval = MatRes))
}

