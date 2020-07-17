table_relfreq <- function(tab) {
### tab: table of sample sizes and raw frequencies,
###      such as returned by `binary_to_table()'
### Converts the `N/2' last rows of tab into relative frequencies.

    ## Useful constant:
    n <- nrow(tab)

    ## Indices of last N/2 rows:
    last_rows <- seq(from = n / 2 + 1, to = n, by = 1)

    ## Indices of first N/2 rows:
    first_rows <- seq_len(n / 2)

    ## Convert raw frequencies into relative frequencies:
    tab[last_rows, ] <- tab[last_rows, ] / tab[first_rows, ]

    ## Return result:
    return(tab)
}
