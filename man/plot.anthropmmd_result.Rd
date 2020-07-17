\name{plot.anthropmmd_result}
\alias{plot.anthropmmd_result}
\title{
  Display a multidimensional scaling (MDS) plot with the MMD
  dissimilarities as input
}
\description{
  This function plots a 2D or 3D MDS to represent the MMD
  dissimilarities among the groups compared. Various MDS methods are
  proposed, and most of them are based on the R package \code{smacof}.
}
\usage{
\method{plot}{anthropmmd_result}(x, method = c("classical", "interval", "ratio", "ordinal"),
axes = FALSE, gof = FALSE, dim = 2, asp = TRUE, xlim = NULL, ...)
}
\arguments{
  \item{x}{An object of class \code{anthropmmd_result}, produced by the
    function \code{\link{mmd}}.}.
  \item{method}{Specification of MDS type. \code{classical} uses the
    metric MDS implemented in \code{stats::cmdscale}; the three other
    values are passed to the R function \code{smacof::smacofSym} (see
    its help page for more details).}
  \item{axes}{Boolean: should the axes be displayed on the plot?}
  \item{gof}{Boolean: should goodness of fit statistics be displayed on
    the topleft corner of the plot? More details below.}
  \item{dim}{Numeric value, 2 or 3. Indicates the maximal dimension
    desired for the MDS plot. It should be noted that, even with
    \code{dim = 3}, the final solution may include only two axes.}
  \item{asp}{Boolean. If \code{TRUE}, the same scale is used for all
    axes. More details below.}
  \item{xlim}{Parameter passed to \code{plot}, can be \code{NULL}.}
  \item{...}{Other arguments possibly passed to \code{plot}.}
}
\value{
  This function returns no value by itself, and only plots a MDS in a
  new device.
}
\details{
  \itemize{
    \item \bold{Axes and scale}. Making all axes use the same scale is
    strongly recommended in all cases (Borg et al., 2013). For a
    3D-plot, since the third axis carries generally only a very small
    percentage of the total variability, you might want to uncheck this
    option to better visualize the distances along the third axis. In
    this case, the axes scales must be displayed on the plot, otherwise
    the plot would be misleading.
    \item \bold{Goodness of fit values}. (i) For classical metric MDS, a
    common statistic is given: the sum of the eigenvalues of the first
    two axes, divided by the sum of all eigenvalues. It indicates the
    fraction of the total variance of the data represented in the MDS
    plot. This statistic comes from the \code{$GOF} value returned by
    the function \code{stats::cmdscale}. (ii) For SMACOF methods, the
    statistic given is the \code{$stress} value returned by the function
    \code{smacof::smacofSym} It indicates the final stress-1 value. A
    value very close to 0 corresponds to a perfect fit. (iii) For both
    approaches, a 'rho' value is also given, which is the Spearman's
    correlation coefficient between real dissimilarities (i.e., MMD
    values) and distances observed on the MDS plot (Dzemyda et
    al.,2013). A value very close to 1 indicates a perfect fit.
  }
}
\references{
  G. Dzemyda, O. Kurasova and J. Zilinskas (2013) \emph{Multidimensional
    Data  Visualization}, Springer, chap. 2, p. 39--40.

  I. Borg, P. Groenen and P. Mair (2013) \emph{Applied Multidimensional
  Scaling}, Springer, chap. 7, p. 79.
}
\seealso{
  \code{\link{start_mmd}}, \code{stats::cmdscale}, \code{smacof::smacofSym}
}
\examples{
## Load and visualize a binary dataset:
data(toyMMD)
head(toyMMD)

## Convert this dataframe into a table of sample sizes and relative
## frequencies:
tab <- binary_to_table(toyMMD, relative = TRUE)
tab

## Compute and display a symmetrical matrix of MMD values:
mmd_out <- mmd(tab, angular = "Freeman")

## Plot a classical metric MDS in two dimensions:
plot(x = mmd_out, method = "classical",
     axes = TRUE, gof = TRUE, dim = 2)
}
\author{Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}}
