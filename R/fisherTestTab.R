fisherTestTab <- function(tab) {
### tab: data in 'table' format
### output -> a list with two components
### Check whether each trait in 'tab' exhibits a significant difference
### at Fisher's exact test for at least one pair of groups.

    ###########################
    ## 1. Set some constants ##
    ###########################
    nb_groups <- nrow(tab) / 2 # number of groups
    nb_traits <- ncol(tab)     # number of traits
    group_names <- extract_groups(tab, type = "table")
    ## Initialize an empty matrix:
    res <- matrix(NA, ncol = nb_traits,
                  nrow = nb_groups * (nb_groups - 1) / 2)
    colnames(res) <- colnames(tab)
    rownames(res) <- seq_len(nrow(res)) # avoid an error in subsequent commands

    #############################
    ## 2. Fisher's exact tests ##
    #############################
    for (j in seq_len(nb_traits)) { # For each trait,
        counter <- 1
        for (k in 1:(nb_groups - 1)) { # and each pair of groups,
            for (l in seq(from = k + 1, to = nb_groups, by = 1)) {
                rownames(res)[counter] <- paste(group_names[k], group_names[l], sep = " - ")
                pres_group1 <- round(tab[k, j] * tab[(k + nb_groups), j])
                pres_group2 <- round(tab[l, j] * tab[(l + nb_groups), j])
                abs_group1 <- round(tab[k, j] * (1 - tab[(k + nb_groups), j]))
                abs_group2 <- round(tab[l, j] * (1 - tab[(l + nb_groups), j]))
                ## Build the confusion matrix:
                mat <- matrix(c(pres_group1, pres_group2, abs_group1, abs_group2),
                              ncol = 2)
                ## and run Fisher's exact test:
                res[counter, j] <- fisher.test(mat)$p.value
                counter <- counter + 1
            }
        }
    }

    ###########################
    ## 3. Return the results ##
    ###########################
    is_informative <- apply(res, MARGIN = 2, FUN = function(x) return(ifelse(any(x <= 0.05), TRUE, FALSE)))
    return(list(informative = tab[, is_informative], pval = res))
}
