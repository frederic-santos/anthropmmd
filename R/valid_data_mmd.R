valid_data_mmd <- function(tab, type) {
### tab : dataframe loaded through the UI
### type : 'raw' or 'table', indicating the type of data submitted
### Output -> boolean. This function checks if "tab" is suitable for AnthropMMD
    
    if (ncol(tab) <= 1) { # For both types (raw or table), the data file is invalid if it has only one column (probably wrong field separator)
        warning("Only one column read in the data file. Please check the field separator.")
        return(FALSE)
    } else {

	if (type =="raw") { # For 'raw' files
            ## Condition 1: all groups must include at least 2 individuals
            if (min(table(tab[,1])) < 2) {
                warning("All groups must include at least 2 individuals. Please remove at least one of your groups.")
                return(FALSE)
            }
            tab[ , 1] <- NULL # Then, do not consider the group identifier any longer
            
            ## Condition 2: invalid data if at least one column has more than 2 levels (thus, other levels than '0' and '1')
            for (j in 1:ncol(tab)) {
                tab[,j] <- factor(tab[,j])
            }
            niveaux <- apply(tab, MARGIN = 2, FUN = function(x) return(nlevels(factor(x)))) # nb of levels for each trait
            if (max(niveaux) > 2) {
                warning("At least one of your columns does not contain only zeroes and ones. Please check your data.")
                return(FALSE)
            }
            
            ## Condition 3: are there only zeroes and ones?
            ok01 <- rep(NA, ncol(tab))
            for (j in 1:ncol(tab)) {
                ok01[j] <- all(levels(tab[,j]) %in% c("0", "1"))
            }
            if (all(ok01)) { # all factors have only 0s and 1s
                return(TRUE)		
            } else {
                warning("At least one of your columns does not contain only zeroes and ones. Please check your data.")
                return(FALSE)
            }
            
	} else if (type == "table") { # For 'tables'
            nb_grps <- nrow(tab) / 2 # number of groups
            noms <- rownames(tab)[1:nb_grps] # should be something like 'N_Group1', 'N_Group2', ...
            if (all(substr(noms,1,2) == "N_") & all(apply(tab, MARGIN=2, FUN=mode) == "numeric")) {
                return(TRUE)
            } else {
                warning("If there are k groups in your data file, the first k lines should be something like 'N_Group1', 'N_Group2', ..., with a mandatory 'N_' prefix.")
                return(FALSE)
            }
	}
    }
}

