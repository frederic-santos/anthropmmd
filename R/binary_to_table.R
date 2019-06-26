binary_to_table <- function(data, relative = FALSE) {
### data: matrix of binary data, + one column for the group indicator
### relative: boolean, indicates if the last rows of the table must contain frequencies (i.e., number of individuals having a given trait) or relative frequencies (i.e., proportions)
### Converts this data frame into a table of group sample sizes and frequencies.

    ## 1. Set some constants:
    nb_traits <- ncol(data) - 1 # number of traits (first column does not count since it is the group indicator)
    groups <- factor(data[ , 1]) # group indicator in the input dataset
    nb_groups <- nlevels(groups) # number of groups
    group_names <- levels(groups)

    ## 2. Initialize an empty matrix in which to fill the results:
    MatRes <- matrix(nrow = 2*nb_groups, ncol = nb_traits)
    colnames(MatRes) <- colnames(data)[-1]
    rownames(MatRes) <- c(paste("N_", group_names, sep=""), paste("Freq_", group_names, sep=""))
    
    ## 3. Fill in the matrix:
    for (j in 1:nb_traits) { # For each trait...
        x <- split(data[ , j+1], groups) # ... split the current trait values according to the groups.
        for (i in 1:nb_groups) { # Then for each group...
            MatRes[i, j] <- sum(is.na(x[[i]]) == FALSE) # compute the number of nonmissing values...
            MatRes[i+nb_groups, j] <- sum(x[[i]] == 1, na.rm = TRUE) # and the relative frequency for the current trait.
        }
    }

    ## 4. Return the results:
    if (relative == FALSE) {
        return(MatRes)
    } else {
        return(table_relfreq(MatRes))
    }
}
