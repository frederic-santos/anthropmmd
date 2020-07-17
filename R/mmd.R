mmd <- function(data, angular = c("Anscombe", "Freeman")) {
### data: table of group sample sizes and frequencies,
###       such as returned by the function table_relfreq
### angular: choice of a formula for angular transformation

    angular <- match.arg(angular) # avoid a warning if no arg is given

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
    ## 2. Initialize some empty matrices ##
    #######################################
    ## MMD matrix (symmetrical):
    mmd_sym <- matrix(NA, nrow = nb_groups, ncol = nb_groups)
    ## the rows and columns of mmd_sym are labeled according to group names:
    dimnames(mmd_sym) <- list(substr(group_names, 3, nchar(group_names)),
                              substr(group_names, 3, nchar(group_names)))
    ## Other matrices:
    pval_matrix <- sd_matrix <- signif_matrix <- mmd_sym

    #############################
    ## 3. Fill in the matrices ##
    #############################
    for (i in 1:nb_groups) {
        for (j in 1:nb_groups) { # For each pair of groups (i, j)...
            mmd_vect <- sd_vect <- rep(NA, nb_traits)
            sum_pval <- 0
            for (k in 1:nb_traits) { # and for each trait k,
                ## Compute the measure of divergence on trait k:
                mmd_vect[k] <- compute_md(nA = mat_size[i, k],
                                          pA = mat_freq[i, k],
                                          nB = mat_size[j, k],
                                          pB = mat_freq[j, k])
                ## Compute the SD for this trait:
                sd_vect[k] <- sd_mmd(nA = mat_size[i, k], nB = mat_size[j, k])
                ## Intermediate result for computing the p-value:
                sum_pval <- sum_pval + ((mat_freq[i, k] - mat_freq[j, k])^2 / (1 / (mat_size[i, k] + 0.5) + 1 / (mat_size[j, k] + 0.5)))
            }
            ## The MMD is the mean of those MD values:
            mmd_sym[i, j] <- max(mean(mmd_vect), 0) # replace by 0 if negative
            if (i != j) { # avoid NaN when comparing a group to itself
                ## The associated SD is as follows:
                sd_matrix[i, j] <- sqrt(2 * sum(sd_vect)) / nb_traits
                ## The associated p-value:
                pval_matrix[i, j] <- pchisq(sum_pval, df = nb_traits,
                                            lower.tail = FALSE)
                ## And finally the significance ('*' or 'NS'):
                signif_matrix[i, j] <- ifelse(pval_matrix[i, j] < 0.05, "*", "NS")
            }
        }
    }
    diag(mmd_sym) <- 0 # distance between a group and itself must be null

    #################################################
    ## 4. Prepare the matrices for elegant display ##
    #################################################
    pval_matrix <- mix_matrices(m = mmd_sym, n = pval_matrix, diag_value = NA)
    mmd_matrix <- mix_matrices(m = mmd_sym, n = sd_matrix, diag_value = 0)
    signif_matrix <- mix_matrices(m = round(mmd_sym, 3), n = signif_matrix,
                                  diag_value = NA)

    ###########################
    ## 5. Return the results ##
    ###########################
    list_results <- list(MMDMatrix = round(mmd_matrix, 6),
                         MMDSym = round(mmd_sym, 6),
                         MMDSignif = signif_matrix,
                         MMDpval = round(pval_matrix, 4))
    class(list_results) <- "anthropmmd_result"
    return(list_results)
}
