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

mds_rho <- function(mmd, coor, k) {
### Computes the rho values displayed on MDS plots
### mmd: symmetrical matrix of MMD values
### coor: MDS coordinates
### k: dimension (2 or 3, for 2D or 3D plots)
    mds_distances <- dist(coor[ , 1:k], diag = FALSE, upper = FALSE) # compute pairwise distances between all individuals on the MDS plot
    mmd_distances <- as.dist(mmd, diag = FALSE, upper = FALSE) # MMD dissimilarities
    ## Compute correlation between "real" (MMD) distances, and the distances post-MDS, not taking into account the null diagonal:
    rho <- cor(as.vector(as.matrix(mmd_distances)), as.vector(as.matrix(mds_distances)), method = "spearman")
    return(round(rho, 3))
}
