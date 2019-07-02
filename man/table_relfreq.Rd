\name{table_relfreq}
\alias{table_relfreq}
\title{
  Converts a table of sample sizes and frequencies into a table of
  sample sizes and relative frequencies.
}
\description{
  Mostly used as an internal function, but could also be convenient to
  transform frequencies (i.e., number of individuals having a given
  trait) into relative frequencies (i.e., proportions).
}
\usage{
table_relfreq(tab)
}
\arguments{
  \item{tab}{A table of sample sizes and frequencies, such as the tables
  returned by the function \code{\link{binary_to_table}.}}
}
\value{
  The last \eqn{K} rows (\eqn{K} being the number of groups) of
  \code{tab} are simply transformed to relative frequencies.
}
\seealso{
  \code{\link{binary_to_table}}, \code{\link{start_mmd}}
}
\examples{
# Load and visualize a binary dataset:
data(toyMMD)
head(toyMMD)
# Convert this dataframe into a table of sample sizes and frequencies:
tab <- binary_to_table(toyMMD, relative = FALSE)
tab
# Convert this table into relative frequencies:
table_relfreq(tab)
}
\author{Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}}
