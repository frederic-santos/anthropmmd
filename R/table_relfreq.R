table_relfreq <- function(tab) {
## tab: table of sample sizes and raw frequencies, such as returned by 'binary_to_table'
## Converts the *K* last rows of tab into relative frequencies.
    nb_groupes <- nrow(tab)/2 # number of groups
    tab[(nb_groupes+1):nrow(tab), ] <- tab[(nb_groupes+1):nrow(tab), ] / tab[1:nb_groupes, ] # convert raw frequencies into relative frequencies
    return(tab)
}
