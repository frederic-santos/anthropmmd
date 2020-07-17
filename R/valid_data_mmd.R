valid_data_mmd <- function(tab, type) {
### tab : dataframe loaded through the UI
### type : 'raw' or 'table', indicating the type of data submitted
### Output -> boolean. Check whether "tab" is suitable for AnthropMMD.

    ## For both types (raw or table), a dataframe with only 1 column is invalid
    ## (probably due to a wrong field separator):
    if (ncol(tab) <= 1) {
        warning("Only one column read in the data file. Please check the field separator.")
        return(FALSE)
    } else {

        if (type == "raw") { # for raw (i.e., binary) files
            ## Condition 1: all groups must include at least 2 individuals
            if (min(table(tab[, 1])) < 2) {
                warning("All groups must include at least 2 individuals. Please remove at least one of your groups.")
                return(FALSE)
            }
            tab[, 1] <- NULL # Then, the group identifier can be removed

            ## Condition 2: invalid data if at least one column has more
            ## than 2 levels (thus, other levels than '0' and '1'):
            for (j in seq_len(ncol(tab))) {
                tab[, j] <- factor(tab[, j])
            }
            nb_levels <- apply(tab, MARGIN = 2,
                             FUN = function(x) return(nlevels(factor(x))))
            if (max(nb_levels) > 2) {
                warning("At least one of your columns does not contain only zeroes and ones. Please check your data.")
                return(FALSE)
            }

            ## Condition 3: are there only zeroes and ones?
            ok01 <- rep(NA, ncol(tab))
            for (j in seq_len(ncol(tab))) {
                ok01[j] <- all(levels(tab[, j]) %in% c("0", "1"))
            }
            if (all(ok01)) { # all factors have only 0s and 1s
                return(TRUE)
            } else {
                warning("At least one of your columns does not contain only zeroes and ones. Please check your data.")
                return(FALSE)
            }

        } else if (type == "table") {        # For 'tables'
            nb_grps <- nrow(tab) / 2         # number of groups
            noms <- rownames(tab)[1:nb_grps] # should be something like 'N_Group1', 'N_Group2', ...
            if (all(substr(noms, 1, 2) == "N_") & all(apply(tab, MARGIN = 2, FUN = mode) == "numeric")) {
                return(TRUE)
            } else {
                warning("If there are k groups in your data file, the first k lines should be something like 'N_Group1', 'N_Group2', ..., with a mandatory 'N_' prefix.")
                return(FALSE)
            }
        }
    }
}
