\name{mmd}
\alias{mmd}
\title{
  Compute MMD values from a table of sample sizes and relative
  frequencies
}

\description{
  Compute various MMD results, typically using a table returned by the
  function \code{\link{binary_to_table}} with the argument
  \code{relative = TRUE}.
}

\usage{
mmd(data, angular = c("Anscombe", "Freeman"))
}

\arguments{
  \item{data}{A table of sample sizes and frequencies}
  \item{angular}{Choice of a formula for angular transformation: either
    Anscombe or Freeman-Tukey transformation.}
}

\value{A list with four components:
  \item{MMDMatrix}{Following the presentation adopted in many research
    articles, a matrix filled with MMD values above the diagonal, and
    standard deviations of MMD below the diagonal.}
  \item{MMDSym}{A symmetrical matrix of MMD values, where negative
    values are replaced by zeroes.}
  \item{MMDSignif}{A matrix where any pair of traits having a
    significant MMD value is indicated by a star, \sQuote{*}.}
  \item{MMDpval}{A matrix filled with MMD values above the diagonal, and
    p-values below the diagonal.}
}

\references{
  de Souza, P. and Houghton, P. (1977). The mean measure of divergence
  and the use of non-metric data in the estimation of biological
  distances. \emph{Journal of Archaeological Science}, \bold{4}(2),
  163--169. doi: 10.1016/0305-4403(77)90063-2
  
  Harris, E. F. and Sjøvold, T. (2004) Calculation of Smith's mean
  measure of divergence for intergroup comparisons using nonmetric
  data. \emph{Dental Anthropology}, \bold{17}(3), 83--93.

  Nikita, E. (2015) A critical review of the mean measure of divergence
  and Mahalanobis distances using artificial data and new approaches to
  the estimation of biodistances employing nonmetric
  traits. \emph{American Journal of Physical Anthropology}, \bold{157},
  284--294. doi: 10.1002/ajpa.22708
}

\seealso{
  \code{\link{start_mmd}}
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
mmd_out <- mmd(tab, angular = "Anscombe")
mmd_out$MMDSym

## Significant MMD values are indicated by a star:
mmd_out$MMDSignif
}
\author{Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}}
\concept{MMD}
\concept{mean measure of divergence}
