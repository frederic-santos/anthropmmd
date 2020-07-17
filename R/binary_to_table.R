binary_to_table <- function(data, relative = FALSE) {
### data: matrix of binary data, + one column for the group indicator
### relative: boolean, indicates whether the last rows of the table contain
###           absolute frequencies (i.e., number of individuals with each trait)
###           or relative frequencies (i.e., proportions).
### Converts this data frame into a table of group sample sizes and frequencies.

    ###########################
    ## 1. Set some constants ##
    ###########################
    nb_traits <- ncol(data) - 1  # 1st column is a group indicator, not a trait
    groups <- factor(data[, 1])  # group indicator in the input dataset
    nb_groups <- nlevels(groups) # number of groups
    group_names <- levels(groups)

    ###################################################
    ## 2. Initialize an empty matrix for the results ##
    ###################################################
    mat <- matrix(nrow = 2 * nb_groups, ncol = nb_traits)
    colnames(mat) <- colnames(data)[-1]
    rownames(mat) <- c(paste("N", group_names, sep = "_"),
                       paste("Freq", group_names, sep = "_"))

    ###########################
    ## 3. Fill in the matrix ##
    ###########################
    for (j in seq_len(nb_traits)) { # For each trait...
        ## ... split the current trait values according to the groups:
        x <- split(data[, j + 1], groups)
        for (i in 1:nb_groups) { # And then for each group...
            ## ... compute the number of nonmissing values:
            mat[i, j] <- sum(is.na(x[[i]]) == FALSE)
            ## and the relative frequency for the current trait:
            mat[i + nb_groups, j] <- sum(x[[i]] == 1, na.rm = TRUE)
        }
    }

    ###########################
    ## 4. Return the results ##
    ###########################
    if (relative == FALSE) {
        return(mat)
    } else {
        return(table_relfreq(mat))
    }
}
