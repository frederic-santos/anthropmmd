\name{binary_to_table}
\alias{binary_to_table}
\title{
  Converts a data frame of binary (i.e., presence/absence) trait
  information into a table of sample sizes and frequencies.
}
\description{
  This function allows to get a summary of sample sizes and frequencies
  for each trait in each group. It is also mandatory to apply this
  function before using the \code{\link{mmd}} function, since the latter
  only accepts table of frequencies, and cannot work with raw binary data.
}
\usage{
binary_to_table(data, relative = FALSE)
}
\arguments{
  \item{data}{A binary (0/1 for presence/absence of traits) data frame
    with \eqn{n} rows (one per individual) and \eqn{p+1} columns (one
    for each of the \eqn{p} traits, plus one column provided as a group
    indicator).}
  \item{relative}{Boolean. Indicates if the last rows of
    the table must contain frequencies (i.e., number of individuals
    having a given trait) or relative frequencies (i.e., proportions).}
}
\value{
  A matrix with \eqn{2*K} rows (\eqn{K} being the number of groups in
  the dataset) and \eqn{p} columns (one per trait).
  The first \eqn{K} rows are the sample sizes, the last \eqn{K} rows are
  trait frequencies.
}
\references{
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
# Convert this dataframe into a table of sample sizes and relative frequencies:
binary_to_table(toyMMD, relative = TRUE)
}
\author{Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}}
