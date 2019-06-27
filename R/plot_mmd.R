plot_mmd <- function(data, method = c("classical", "interval", "ratio", "ordinal"), axes = FALSE, gof = FALSE, dim = 2, asp = TRUE) {
### data: symmetrical matrix of MMD values
### method: type of MDS. "classical" for cmdscale, or one of SMACOF methods
### axes: boolean, display axes on plot or not
### gof: boolean, display goodness of fit value on plot or not
### dim: *maximal* dimension for the calculation of MDS coordinates. /!\ In fine, the solution can be computed on 2 axes only even if dim=3.
### asp: boolean, TRUE passes "asp=1" to plot function.

    aspValue <- ifelse(asp == TRUE, 1, NA) # 'translate' asp parameter as required by plot function
    method <- match.arg(method) # to avoid a warning with the default value

#########################################################
### 1. Compute the MDS according to user-defined criteria
    if (method == "classical") {
        ## Option A. Classical metric MDS
        res.mds <- cmdscale(data, k = dim, eig = TRUE) # *list* of MDS results
        coor <- res.mds$points # MDS coordinates
        varByDim <- apply(coor, MARGIN = 2, FUN = sd) # sd along each axis
        legend.plot <- "Classical multidimensional scaling of MMD values"
        if (gof == TRUE) {
            legend.gof <- "Eigenvalue-based GoF="
            gof.value <- round(res.mds$GOF[1], 3)
            rho.value <- mds_rho(mmd = data, coor = coor)
        }
    } else {
        ## Option B: MDS via SMACOF package
        res.mds <- smacofSym(as.dist(data), type = method) # *list* of MDS results
        coor <- res.mds$conf # MDS coordinates
        varByDim <- apply(coor, MARGIN = 2, FUN = sd) # sd along each axis
        legend.plot <- paste("Multidimensional scaling of MMD values (", method, " type)", sep = "")
        if (gof == TRUE) {
            legend.gof <- "Stress="
            gof.value <- round(res.mds$stress, 3)
            rho.value <- mds_rho(mmd = data, coor = coor)
        }
    }

#######################
### 2. Display MDS plot
    ## Option A. MDS 2D:
    if ((dim == 2) | (dim == 3 & ncol(coor) < 3) | (dim == 3 & ncol(coor) >= 3 & min(varByDim) < 2e-16)) { # if the user wanted a 2D plot, or if a 3D plot could not be computed
        if (ncol(coor) >= 2 & any(data > 0)) { # OK, the plot can be displayed
            plot(x = coor[ , 1], y = coor[ , 2], pch = 16, xlab = "", ylab = "", axes = axes,
                 main = legend.plot, ylim=c(min(coor[,2]), 1.15*max(coor[,2])), asp = aspValue)
            text(coor[ , 1], coor[ , 2], pos = 3, labels = rownames(coor))
            
            if (gof == TRUE) { # if the user wants the GOF to be displayed    
                legend("topleft", legend = c(paste("Spearman's rho=", rho.value, sep = ""),
                                             paste(legend.gof, gof.value, sep = "")))
            }
        } else if (ncol(coor) >= 2 & all(data == 0)) { # if the input matrix is filled only with zeroes -> error
            plot(x = 0, y = 0, xlab = "", ylab = "", axes = FALSE, xlim = c(-2,2), ylim = c(-2,2), pch = "")
            text(x = 0, y = 0.5, labels = "The MMD matrix contains only zeroes.", col = "black")
            text(x = 0, y = -0.5, labels = "Impossible to get a MDS plot.", col = "black")
        } else { # ncol(coor)<2, so that MDS cannot be computed
            plot(x = 0, y = 0, xlab = "", ylab = "", axes = FALSE, xlim = c(-2,2), ylim = c(-2,2), pch = "")
            text(x = 0, y = 0, labels = "The representation could not be computed since there is only one positive eigenvalue.", col = "black")
        }
        
    } else { # dim=3 & ncol(coor)>=3 & sufficient variability on third axis: a 3D graph is possible
        ## Option B. MDS 3D:
        graphe <- scatterplot3d(x = coor[,1], y = coor[,2], z = coor[,3], axis = axes, pch = 16,
                                highlight.3d = TRUE, type = "h", main = legend.plot,
                                xlab = "", ylab = "", zlab = "", asp = aspValue)
        coord.labels <- graphe$xyz.convert(x = coor[,1], y = coor[,2], z = coor[,3])
        text(x = coord.labels$x, y = coord.labels$y, pos = 3, labels = rownames(coor))
        if (gof == TRUE) { # if the user wants the GOF to be displayed
            legend("topleft", legend = c(paste("Spearman's rho=", rho.value, sep = ""),
                                         paste(legend.gof, gof.value, sep = "")))
        }
    }
}
