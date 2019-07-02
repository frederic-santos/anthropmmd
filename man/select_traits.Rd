\name{select_traits}
\alias{select_traits}
\title{
  Select a subset of traits meeting certain criteria
}
\description{
  This function provides several strategies to discard some useless
  traits (non-polymorphic, non-discriminatory, etc.) upstream the MMD
  analysis.
}
\usage{
select_traits(tab, k = 10, strategy = c("none", "excludeNPT",
"excludeQNPT", "excludeNOMD", "keepFisher"), OMDvalue = NULL, groups,
angular = c("Anscombe", "Freeman"))
}
\arguments{
  \item{tab}{A table of sample sizes and frequencies, typically returned
    by the function \code{\link{binary_to_table}} with the argument
    \code{relative = TRUE}.}
  \item{k}{Numeric value: the required minimal number of individuals per
    group. Any trait that could be taken on fewer individuals in at
    least one group will be removed from the dataset. This allows to
    select only the traits with a sufficient amount of information in
    each group.}
  \item{strategy}{Strategy for trait selection, i.e. for the removal of
    non-polymorphic traits. The four options are fully described in
    Santos (2018) and in the help page of \code{\link{StartMMD}}.}
  \item{OMDvalue}{To be specified if and only if \code{strategy =
    "excludeNOMD"}. Set the desired threshold for the \dQuote{overall
    measure of divergence} that must be reached for a trait to be kept.}
  \item{groups}{A factor or character vector, indicating the group to be
    considered in the analysis. Since some groups can have a very low
    sample size, this will allow to discard those groups in order to
    facilitate the trait selection via the argument
    \code{k}. (Otherwise, almost all traits would be removed.)}
  \item{angular}{Formula for angular transformation, see Harris and
    Sjøvold (2004). Useful only for the calculation of overall measure
    of divergence.}
}
\value{
  A list with two components:
  \item{filtered}{The dataset filtered according to the user-defined
    criteria.}
  \item{OMD}{The \dQuote{overall measure of divergence} for
    each trait.}
}
\references{
  Harris, E. F. and Sjøvold, T. (2004) Calculation of Smith's mean
  measure of divergence for intergroup comparisons using nonmetric
  data. \emph{Dental Anthropology}, \bold{17}(3), 83--93.

  Santos, F. (2018) AnthropMMD: an R package with a graphical user
  interface for the mean measure of divergence. \emph{American Journal
  of Physical Anthropology}, \bold{165}(1), 200--205. doi:
  10.1002/ajpa.23336
}
\seealso{
  \code{\link{start_mmd}}
}
\examples{
# Load and visualize a binary dataset:
data(toyMMD)
head(toyMMD)
# Convert this dataframe into a table of sample sizes and
# relative frequencies:
tab <- binary_to_table(toyMMD, relative = TRUE)
tab

# Filter this dataset to keep only those traits that have at
# least k=10 individuals in each group:
select_traits(tab, k = 10)
# Only Trait1 is excluded.

# Filter this dataset to keep only those traits that have at
# least k=11 individuals in each group, and show significant
# differences at Fisher's exact test:
select_traits(tab, k = 11, strategy = "keepFisher")
# Traits 1, 5 and 8 are excluded.
}
\author{Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}}
