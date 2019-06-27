plot_mmd <- function(data, method = c("classical", "interval", "ratio", "ordinal"), axes = FALSE, gof = FALSE, dim = 2, asp = TRUE) {
### data: symmetrical matrix of MMD values
### method: type of MDS. "classical" for cmdscale, or one of SMACOF methods
### axes: boolean, display axes on plot or not
### gof: boolean, display goodness of fit value on plot or not
### dim: *maximal* dimension for the calculation of MDS coordinates. /!\ In fine, the solution can be computed on 2 axes only even if dim=3.
### asp: boolean, TRUE passes "asp=1" to plot function.

    aspValue <- ifelse(asp == TRUE, 1, NA) # 'translate' asp parameter as required by plot function
    
    if (method == "classical") { 
##################################
### Option A. Classical metric MDS
        res.pcoa <- cmdscale(data, k = dim, eig = TRUE) # *list* of MDS results
        coor <- res.pcoa$points # MDS coordinates
        varByDim <- apply(coor, MARGIN = 2, FUN = sd) # sd along each axis
        ## A.1. MDS 2D:
        if ((dim == 2) | (dim == 3 & ncol(coor) < 3) | (dim == 3 & ncol(coor) >= 3 & min(varByDim) < 2e-16)) { # if the user wanted a 2D plot, or if a 3D plot could not be computed
            if (ncol(coor) >= 2 & any(data > 0)) { # OK, the plot can be displayed
                plot(x = coor[,1], y = coor[,2], pch = 16, xlab = "", ylab = "", axes = axes,
                     main="Classical multidimensional scaling of MMD values",
                     ylim=c(min(coor[,2]), 1.15*max(coor[,2])), asp = aspValue)
                text(coor[,1], coor[,2], pos = 3, labels = rownames(coor))
                
                if (gof == TRUE) { # if the user wants the GOF to be displayed
                    mdsDistances <- dist(coor[,1:2], diag = FALSE, upper = FALSE) # compute pairwise distances between all individuals
                    mmdDistances <- as.dist(data, diag = FALSE, upper = FALSE)
                    rho <- cor(as.vector(as.matrix(mmdDistances)), as.vector(as.matrix(mdsDistances)), method = "spearman") # compute correlation between "real" (MMD) distances, and the distances post-MDS, not taking into account the null diagonal
                    legend("topleft", legend = c(paste("Spearman's rho=", round(rho, 3), sep = ""),
                                                 paste("Eigenvalue-based GoF=", round(res.pcoa$GOF[1], 3), sep = ""))
                           )
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
            ## A.2 : MDS 3D:
            graphe <- scatterplot3d(x = coor[,1], y = coor[,2], z = coor[,3], axis = axes, pch = 16,
                                    highlight.3d = TRUE, type = "h", main = "Classical multidimensional scaling of MMD values",
                                    xlab = "", ylab = "", zlab = "", asp = aspValue)
            coord.labels <- graphe$xyz.convert(x = coor[,1], y = coor[,2], z = coor[,3])
            text(x = coord.labels$x, y = coord.labels$y, pos = 3, labels = rownames(coor))
            if (gof == TRUE) { # if the user wants the GOF to be displayed
                mdsDistances <- dist(coor[,1:3], diag = TRUE, upper = TRUE)
                mmdDistances <- as.dist(data, diag = FALSE, upper = FALSE)
                rho <- cor(as.vector(as.matrix(mmdDistances)), as.vector(as.matrix(mdsDistances)), method = "spearman")
                legend("topleft", legend = c(paste("Spearman's rho=", round(rho, 3), sep = ""), paste("Eigenvalue-based GoF=", round(res.pcoa$GOF[1], 3), sep = "")))
            }
        }
        
    } else if (method!="classical") {

####################################
### Option B: MDS via SMACOF package
        resNMDS <- smacofSym(as.dist(data), type = method) # *list* of MDS results
        coor <- resNMDS$conf # MDS coordinates
        varByDim <- apply(coor, MARGIN=2, FUN=sd) # sd along each axis
        ## B.1. MDS 2D:
        if ((dim == 2) | (dim == 3 & ncol(coor) < 3) | (dim == 3 & ncol(coor) >= 3 & min(varByDim) < 2e-16)) { 
            plot(resNMDS, xlab = "", ylab = "", axes = axes,
                 main = paste("Multidimensional scaling of MMD values (", method, " type)", sep = ""),
                 ylim = c(min(resNMDS$conf[,2]), 1.15*max(resNMDS$conf[,2])), asp = aspValue, cex = 1, label.conf = list(cex=1))

            if (gof == TRUE) { 
                mdsDistances <- dist(coor[,1:2], diag = TRUE, upper = TRUE)
                mmdDistances <- as.dist(data, diag = FALSE, upper = FALSE)
                rho <- cor(as.vector(as.matrix(mmdDistances)), as.vector(as.matrix(mdsDistances)), method = "spearman")
                legend("topleft", legend = c(paste("Spearman's rho=", round(rho, 3), sep = ""), paste("Stress=", round(resNMDS$stress, 3), sep = "")))
            }
        } else { # dim=3 & ncol(coor)>=3, i.e. a 3D graph is possible
            ## B.2. MDS 3D:
            graphe <- scatterplot3d(x = coor[,1], y = coor[,2], z = coor[,3], axis = axes, pch = 16,
                                    highlight.3d = TRUE, type = "h", main = paste("Multidimensional scaling of MMD values (", method, " type)", sep=""),
                                    xlab = "", ylab = "", zlab = "", asp = aspValue)
            coord.labels <- graphe$xyz.convert(x = coor[,1], y = coor[,2], z = coor[,3])
            text(x = coord.labels$x, y = coord.labels$y, pos = 3, labels = rownames(coor))
            if (gof == TRUE) { 
                mdsDistances <- dist(coor[,1:3], diag = TRUE, upper = TRUE)
                mmdDistances <- as.dist(data, diag = FALSE, upper = FALSE)
                rho <- cor(as.vector(as.matrix(mmdDistances)), as.vector(as.matrix(mdsDistances)), method = "spearman")
                legend("topleft", legend = c(paste("Spearman's rho=", round(rho, 3), sep = ""), paste("Stress=", round(resNMDS$stress, 3), sep = "")))
            }
        }
    } 
}
