plot.anthropmmd_result <- function(x,
                                   method = c("classical", "interval", "ratio", "ordinal"),
                                   axes = FALSE, gof = FALSE,
                                   dim = 2, asp = TRUE,
                                   xlim = NULL, ...) {
### x: an object of class "anthropmmd_result"
### method: type of MDS. "classical" for cmdscale, or one of SMACOF methods
### axes: boolean, display axes on plot or not
### gof: boolean, display goodness of fit value on plot or not
### dim: *maximal* dimension for the calculation of MDS coordinates. /!\ In fine, the solution can be computed on 2 axes only even if dim=3.
### asp: boolean, TRUE passes "asp=1" to plot function.
### ...: possibly other parameters passed to plot()

    ## Check input:
    stopifnot(class(x) == "anthropmmd_result")

    ## Pass to main plotting function:
    plot_mmd(data = x$MMDSym,
             method = method,
             axes = axes,
             gof = gof,
             dim = dim,
             asp = asp,
             xlim = NULL)
}
