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
    rho <- cor(as.vector(mmd_distances), as.vector(mds_distances), method = "spearman")
    return(round(rho, 3))
}

max3 <- function(dat) {
### dat: table of sample sizes and frequencies, such as returned by 'binary_to_table'
### Returns the maximal value admissible for the slider bar in the UI (i.e., the maximal value that can be set for the required number of individuals in each group)
       
    mins <- apply(dat[1:(nrow(dat)/2),], MARGIN = 2, FUN = min) # for each trait, the minimal sample size reached among all groups
    return(as.numeric(sort(mins, decreasing = TRUE)[2])) # return the second greatest value (so that at least *two* traits can be used in further analyses)
}

extract_groups <- function(tab, type) {
### Function extracting the group names from the data
### tab: dataframe loaded through the UI
### type: string ('raw' or 'table')
### output -> vector of strings
    
    if (type == "raw") {
        ## raw binary data:
        return(levels(tab[ , 1]))
    } else if (type == "table") { 
        ## table of n's and frequencies:
        nb_grps <- nrow(tab) / 2 # number of groups
        noms <- rownames(tab)[1:nb_grps] # should be 'N_Group1', 'N_Group2', ...
        return(substr(noms, 3 , nchar(noms))) # thus discard the 'N_' in each one.
    }
}
