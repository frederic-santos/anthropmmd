theta <- function(n, p, choice = c("Anscombe", "Freeman")) {
### Function for angular transformation
### n: sample size
### p: relative frequency for the given trait
### choice: variant of angular transformation to be used

    if (choice == "Anscombe") {
        return(asin((n/(n+3/4)) * (1-2*p)))
    } else { # Freeman-Tukey
        return(0.5 * ( asin(1-(2*p*n/(n+1))) + asin(1-(2*((p*n)+1)/(n+1))) ))
    }    
}

sd_mmd <- function(nA, nB) {
### nA & nB: sample sizes in the groups A et B
    return((1 / (nA + 0.5) + 1 / (nB + 0.5))^2)
}
                           
compute_md <- function(nA, pA, nB, pB, choice = c("Anscombe", "Freeman")) {
### Computes the measure of divergence for one given trait
### nA & nB: sample sizes in the groups A et B
### pA & pB: trait frequencies in the groups A et B
### choice: variant of angular transformation to be used    
    return((theta(nA, pA, choice = choice) - theta(nB, pB, choice = choice))^2 - sqrt(sd_mmd(nA, nB)))
}   

mds_rho <- function(mmd, coor) {
### Computes the rho values displayed on MDS plots
### mmd: symmetrical matrix of MMD values
### coor: MDS coordinates
    mds_distances <- dist(coor[ , 1:ncol(coor)], diag = FALSE, upper = FALSE) # compute pairwise distances between all individuals on the MDS plot
    mmd_distances <- as.dist(mmd, diag = FALSE, upper = FALSE) # MMD dissimilarities
    ## Compute correlation between "real" (MMD) distances, and the distances post-MDS, not taking into account the null diagonal:
    rho <- cor(as.vector(as.matrix(mmd_distances)), as.vector(as.matrix(mds_distances)), method = "spearman")
    return(round(rho, 3))
}

max3 <- function(dat) {
### dat: table of sample sizes and frequencies, such as returned by 'binary_to_table'
### Returns the maximal value admissible for the slider bar in the UI (i.e., the maximal value that can be set for the required number of individuals in each group)
       
    mins <- apply(dat[1:(nrow(dat)/2),], MARGIN = 2, FUN = min) # for each trait, the minimal sample size reached among all groups
    return(as.numeric(sort(mins, decreasing = TRUE)[2])) # return the second greatest value (so that at least *two* traits can be used in further analyses)
}
