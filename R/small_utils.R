theta <- function(n, p, choice = c("Anscombe", "Freeman")) {
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
### nA & nB: sample sizes in the groups A et B
### pA & pB: trait frequencies in the groups A et B
### choice: variant of angular transformation to be used    
### Computes the measure of divergence for one given trait
    return((theta(nA, pA, choice = choice) - theta(nB, pB, choice = choice))^2 - sqrt(sd_mmd(nA, nB)))
}   
