mmd <- function(data, angular = c("Anscombe", "Freeman")) {
### data: table of group sample sizes and frequencies, such as returned by the function table_relfreq
### angular: choice of a formula for angular transformation

    angular <- match.arg(angular) # to avoid a warning if the user do not specify anything
    
    ## 1. Define some useful constants and matrices:
    nb_groups <- nrow(data) / 2 # number of groups in the data
    mat_size <- data[1:nb_groups, ] # portion of the data corresponding to the sample sizes
    mat_freq <- data[(nb_groups+1):(2*nb_groups), ] # portion of the data corresponding to the relative frequencies

    ## 2. Initialize an empty MMD matrix:
    mmd_matrix <- matrix(0, nrow = nrow(mat_size), ncol = nrow(mat_size))
    group_names <- rownames(mat_size)
    dimnames(mmd_matrix) <- list(substr(group_names,3,nchar(group_names)), substr(group_names,3,nchar(group_names))) # the rows and columns of mmd_matrix will be labeled according to the group names

    ## 3. Fill in the MMD matrix:
    for (i in 1:nrow(mmd_matrix)) {
        for (j in 1:ncol(mmd_matrix)) {
            mmd_vect <- rep(NA, ncol(mat_size))
            if (j > i) { # upper-diagonal part, to be filled with MMD values
                for (k in 1:length(mmd_vect)) { 
                    mmd_vect[k] <- compute_md(nA = mat_size[i,k], pA = mat_freq[i,k], nB = mat_size[j,k], pB = mat_freq[j,k], choice = angular) 
                }
                mmd_matrix[i, j] <- sum(mmd_vect) / length(mmd_vect) 
            } else if (i ==j) { # on the diagonal, fill with null values (dissimilarity between a group and itself)
                mmd_matrix[i, j] <- 0
            } else { # i.e. i > j: lower-diagonal part, to be filled with SD values
                for (k in 1:length(mmd_vect)) { 
                    mmd_vect[k] <- sd_mmd(nA = mat_size[i,k], nB = mat_size[j,k]) 
                }
                mmd_matrix[i, j] <- sqrt(2*sum(mmd_vect)) / length(mmd_vect)
            }
        }
    }
    
    ## 4. Other results:
    mmd_sym <- mmd_matrix # mmd_sym: will be a symmetrical matrix filled with MMD values only
    mmd_signif <- round(mmd_matrix,3) # mmd_signif: the matrix will contain the information about 'significant' MMD values
    for (i in 1:nrow(mmd_matrix)) {
        for (j in 1:ncol(mmd_matrix)) { # for each pair of traits,
            if (i > j) {
                mmd_sym[i, j] <- max(0, mmd_matrix[j, i]) # lower-diagonal part: replace SD by MMD (or 0 if the MMD value is negative)
                mmd_signif[i, j] <- ifelse(mmd_matrix[j, i] > (2*mmd_matrix[i,j]), "*", "NS")
            } else if (i == j) {
                mmd_signif[i, j] <- NA
            }
            else {
                mmd_sym[i, j] <- max(0, mmd_sym[i, j]) # upper-diagonal: already filled by MMDs, just replace by 0 if negative
            }
        }
    }

    ## 5. Return the results:
    list_results <- list(round(mmd_matrix,6), round(mmd_sym,6), mmd_signif)
    names(list_results) <- c("MMDMatrix", "MMDSym", "MMDSignif")
    return(list_results)
}
