max3 <- function(dat) {
### dat: table of sample sizes and frequencies, such as returned by 'binary_to_table'
### Returns the maximal value admissible for the slider bar in the UI (i.e., the maximal value that can be set for the required number of individuals in each group)
       
    mins <- apply(dat[1:(nrow(dat)/2),], MARGIN = 2, FUN = min) # for each trait, the minimal sample size reached among all groups
    return(as.numeric(sort(mins, decreasing = TRUE)[2])) # return the second greatest value (so that at least *two* traits can be used in further analyses)
}
