mmd <- function(data, angular = c("Anscombe", "Freeman")) {
### data: table of group sample sizes and frequencies, such as returned by the function table_relfreq
### angular: choice of a formula for angular transformation

    angular <- match.arg(angular)

    ##################################################
    ## 1. Define some useful constants and matrices ##
    ##################################################
    nb_groups <- nrow(data) / 2
    nb_traits <- ncol(data)
    ## portion of the data corresponding to the sample sizes:
    mat_size <- data[1:nb_groups, ]
    group_names <- rownames(mat_size)
    ## portion of the data corresponding to the relative frequencies:
    mat_freq <- data[(nb_groups + 1):(2 * nb_groups), ]
    ## angular transformation of relative frequencies:
    for (i in 1:nb_groups) {
        for (j in 1:nb_traits) {
            mat_freq[i, j] <- theta(n = mat_size[i, j],
                                    p = mat_freq[i, j],
                                    choice = angular)
        }
    }
    
    #######################################
    ## 2. Initialize an empty MMD matrix ##
    #######################################
    mmd_matrix <- matrix(NA, nrow = nb_groups, ncol = nb_groups)
    ## the rows and columns of mmd_matrix are labeled according to group names:
    dimnames(mmd_matrix) <- list(substr(group_names, 3, nchar(group_names)),
                                 substr(group_names, 3, nchar(group_names)))
    ## matrix of p-values:
    mmd_pval <- mmd_matrix

    ###############################
    ## 3. Fill in the MMD matrix ##
    ###############################
    for (i in 1:nb_groups) {
        for (j in 1:nb_groups) { # for each pair of groups (i,j),
            mmd_vect <- rep(NA, nb_traits)
            if (j > i) { # upper-diagonal part, to be filled with MMD values
                somme <- 0
                for (k in 1:nb_traits) { # for each trait,
                    mmd_vect[k] <- compute_md(nA = mat_size[i, k], pA = mat_freq[i, k],
                                              nB = mat_size[j, k], pB = mat_freq[j, k])
                    somme <- somme + ((mat_freq[i, k] - mat_freq[j, k])^2 / (1/(mat_size[i,k]+0.5) + 1/(mat_size[j,k]+0.5)))
                }
                mmd_matrix[i, j] <- sum(mmd_vect) / nb_traits
                mmd_pval[i, j] <- mmd_pval[j, i] <- pchisq(somme, df = nb_traits, lower.tail = FALSE)
            } else if (i == j) { # on the diagonal, fill with null values (dissimilarity between a group and itself)
                mmd_matrix[i, j] <- 0
            } else { # i.e. i > j: lower-diagonal part, to be filled with SD values
                for (k in 1:nb_traits) {
                    mmd_vect[k] <- sd_mmd(nA = mat_size[i, k], nB = mat_size[j, k])
                }
                mmd_matrix[i, j] <- sqrt(2 * sum(mmd_vect)) / nb_traits
            }
        }
    }

    ######################
    ## 4. Other results ##
    ######################
    mmd_sym <- mmd_matrix # a symmetrical matrix of MMD values
    mmd_signif <- round(mmd_matrix, 3) # matrix of '*' for significant MMDs
    for (i in 1:nb_groups) {
        for (j in 1:nb_groups) { # for each pair of traits,
            if (i > j) {
                ## lower-diagonal part: replace SD by MMD (or 0 if MMD<0)
                mmd_sym[i, j] <- max(0, mmd_matrix[j, i])
                mmd_signif[i, j] <- ifelse(mmd_matrix[j, i] > (2 * mmd_matrix[i, j]),
                                           "*",
                                           "NS")
            } else if (i == j) {
                ## diagonal
                mmd_signif[i, j] <- NA
            }
            else {
                ## upper-diagonal part: already filled by MMDs, just replace by 0 if negative
                mmd_sym[i, j] <- max(0, mmd_sym[i, j])
            }
        }
    }

    ###########################
    ## 5. Return the results ##
    ###########################
    list_results <- list(MMDMatrix = round(mmd_matrix, 6),
                         MMDSym = round(mmd_sym, 6),
                         MMDSignif = mmd_signif,
                         MMDpval = round(mmd_pval, 4))
    return(list_results)
}
