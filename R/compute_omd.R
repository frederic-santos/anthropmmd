compute_omd <- function(data, formule, OMDvalue = 0) {
### Computes the overall measure of divergence for each trait
### tab: the data in 'table of frequencies' format
### formule: string, "Anscombe" or "Freeman"
### OMDvalue: OMD (overall measure of divergence) threshold for a trait to be kept
### output -> list with 4 components

    ## 1. Define some useful constants and matrices:
    nb_groups <- nrow(data) / 2 # number of groups in the data
    mat_size <- data[1:nb_groups, ] # portion of the data corresponding to the sample sizes
    mat_freq <- data[(nb_groups+1):(2*nb_groups), ] # portion of the data corresponding to the relative frequencies

    ## 2. Compute the OMD:
    OMD_matrix <- matrix(NA, nrow = ncol(mat_size), ncol = 1) # initialize an empty matrix
    rownames(OMD_matrix) <- colnames(mat_size)
    temp_matrix <- matrix(0, nrow = nb_groups, ncol = nb_groups) # matrix that will contain, for each trait, the MD between each pair of groups.
    ## OMD = sum of MD's

    for (i in 1:ncol(mat_size)) { # for each trait

        for (j in 1:nb_groups) { # for each pair of groups
            for (k in 1:nb_groups) {
                if (j > k) { # (strict) upper part of the matrix
                    temp_matrix[j,k] <- compute_md(nA = mat_size[j,i], pA = mat_freq[j,i],
                                                   nB = mat_size[k,i], pB = mat_freq[k,i],
                                                   choice = formule)
                }
            }
        }

        OMD_matrix[i, 1] <- sum(temp_matrix) # OMD of trait number "i"
    }
    ## At this stage, OMD_matrix = OMD values for each trait, sorted in the original order of traits in the data
    
    ## 3. Return the results:
    OMD_matrix_sorted <- as.matrix(OMD_matrix[order(OMD_matrix[,1], decreasing = TRUE), ]) # OMD values sorted by decreasing order
    OMD_matrix_sorted_pos <- as.matrix(OMD_matrix_sorted[OMD_matrix_sorted[,1] > OMDvalue, ]) # OMD values, sorted and *greater than a given threshold*
    OMD_matrix_pos <- as.matrix(OMD_matrix[OMD_matrix[,1] > OMDvalue, ]) # OMD values, in the original order and *greater than a given threshold*
    return(list("Matrix" = OMD_matrix,
                "Pos" = OMD_matrix_pos,
                "Sorted" = OMD_matrix_sorted,
                "SortedPos" = OMD_matrix_sorted_pos))
}
