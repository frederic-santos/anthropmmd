compute_omd <- function(data, formule, OMDvalue = 0) {
### tab: the data in 'table of frequencies' format
### formule: string, "Anscombe" or "Freeman"
### OMDvalue: OMD (overall measure of divergence) threshold for a trait to be kept
### Compute the overall measure of divergence for each trait.
### output -> list with 4 components

    ##################################################
    ## 1. Define some useful constants and matrices ##
    ##################################################
    nb_groups <- nrow(data) / 2 # number of groups in the data
    ## part of the data corresponding to the sample sizes:
    mat_size <- data[1:nb_groups, ]
    ## part of the data corresponding to the relative frequencies:
    mat_freq <- data[seq(from = nb_groups + 1, to = 2 * nb_groups, by = 1), ]
    ## angular transformation of relative frequencies:
    for (i in seq_len(nrow(mat_freq))) {
        for (j in seq_len(ncol(mat_freq))) {
            mat_freq[i, j] <- theta(n = mat_size[i, j],
                                    p = mat_freq[i, j],
                                    choice = formule)
        }
    }

    ########################
    ## 2. Compute the OMD ##
    ########################
    ## Initialize an empty matrix:
    omd_matrix <- matrix(NA, nrow = ncol(mat_size), ncol = 1)
    rownames(omd_matrix) <- colnames(mat_size)
    ## This will contain, for each trait, the MD between each pair of groups:
    temp_matrix <- matrix(0, nrow = nb_groups, ncol = nb_groups)
    ## (reminder: OMD = sum of MD's)

    for (i in seq_len(ncol(mat_size))) { # For each trait,

        for (j in seq_len(nb_groups)) { # and for each pair of groups,
            for (k in seq_len(nb_groups)) {
                if (j > k) { # (strict) upper part of the matrix
                    temp_matrix[j, k] <- compute_md(nA = mat_size[j, i],
                                                    pA = mat_freq[j, i],
                                                    nB = mat_size[k, i],
                                                    pB = mat_freq[k, i])
                }
            }
        }

        omd_matrix[i, 1] <- sum(temp_matrix) # OMD of trait number "i"
    }
    ## At this stage, omd_matrix = OMD values for each trait,
    ## sorted in the original order of traits in the data

    ###########################
    ## 3. Return the results ##
    ###########################
    ## # OMD values sorted by decreasing order:
    omd_matrix_sorted <- as.matrix(omd_matrix[order(omd_matrix[, 1], decreasing = TRUE), ])
    ## OMD values, sorted and *greater than a given threshold*:
    omd_matrix_sorted_pos <- as.matrix(omd_matrix_sorted[omd_matrix_sorted[,1] > OMDvalue, ])
    ## OMD values, in the original order and *greater than a given threshold*:
    omd_matrix_pos <- as.matrix(omd_matrix[omd_matrix[,1] > OMDvalue, ])
    
    return(list("Matrix" = omd_matrix,
                "Pos" = omd_matrix_pos,
                "Sorted" = omd_matrix_sorted,
                "SortedPos" = omd_matrix_sorted_pos))
}
