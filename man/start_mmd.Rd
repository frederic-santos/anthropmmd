\name{start_mmd}
\alias{start_mmd}
\alias{StartMMD}
\title{
  An R-Shiny application for the mean measure of divergence
}

\description{
  Launches a graphical user interface (GUI) for the calculation of the
  mean measure of divergence.
}

\usage{
start_mmd()
StartMMD()
}

\note{
  The R console is not available when the GUI is active. To exit the
  GUI, type Echap (on MS Windows systems) or Ctrl+C (on Linux systems)
  in the R console.

  On 14-inch (or smaller) screens, for convenience, it may be necessary
  to decrease the zoom level of your web browser and/or to turn on
  fullscreen mode.
}

\details{
  The GUI of AnthropMMD is completely autonomous: reading the data file
and specifying the parameters of the analysis are done through the
interface. Once the dataset is loaded, the output reacts dynamically to
any change in the analysis settings.

\itemize{
  \item AnthropMMD accepts .CSV or .TXT data files, but does not support
  .ODS or .XLS(X) files. Two types of data input formats can be used:
  \itemize{
    \item A \sQuote{Raw binary dataset} (one row for each individual,
    one column for each variable). The first column must be the group
    indicator, and the other columns are binary data for the traits
    studied, where 1 indicates the presence of a trait, and 0 its
    absence. Row names are optional for this type of file. An example of
    valid data file can be found as Supporting Information online in
    Santos (2018).
    \item A \sQuote{Table of n's and absolute frequencies for each
    group}, i.e. a dataset of sample sizes and absolute
    frequencies. This type of dataset has \eqn{2 \times K}{2*K} rows
    (\eqn{K}{K} being the number of groups compared) and \eqn{p}{p}
    columns (\eqn{p}{p} being the number of traits studied). The first
    \eqn{K}{K} lines must be the group n's for each trait, and the last
    \eqn{K}{K} lines are absolute frequencies for each trait (i.e. the
    number of times the trait is present). Row names are mandatory for
    this type of file. The first \eqn{K}{K} rows must be labelled with
    names beginning with \sQuote{N_}, such as: N_GroupA, N_GroupB, ...,
    N_GroupK. The last \eqn{K}{K} rows should be labelled with names
    beginning with \sQuote{Freq_}, such as: Freq_GroupA, ...,
    Freq_GroupK. An example of valid data file can be found as
    Supporting Information online in Santos (2018).
  }
  For both data types, column names are strongly recommended for better
  interpretability of the results.

  \item One can choose between Anscombe or Freeman-Tukey formula for
  angular transformation (cf. Harris and Sjøvold 2004; Irish 2010).
  \item \sQuote{Only retain the traits with this minimal number of
  individuals per group}: the traits with fewer individuals in at least
  one active group will not be considered in the analysis.
  \item \sQuote{Exclusion strategy}: a careful selection of traits is
  crucial when using MMD (cf. Harris and Sjøvold 2004 for a complete
  explanation), and the user should probably \dQuote{exclude the traits
  that are nondiscriminatory across groups} (Irish 2010).
  \itemize{
    \item \sQuote{Exclude nonpolymorphic traits} removes all the traits
    showing no variability at all, i.e. with the same value (\sQuote{0}
    or \sQuote{1}) for all individuals.
    \item \sQuote{Exclude quasi-nonpolymorphic traits} also removes the
    traits whose variability is only due to a single individual: for
    example, a trait with only one positive observation in the whole
    dataset.
    \item \sQuote{Use Fisher's exact test} implements the advice given
    by Harris and Sjøvold (2004) to select contributory traits, defined
    as those \dQuote{showing a statistically significant difference
    between at least one pair of the groups being evaluated}. Fisher's
    exact tests are performed for each pair of groups, and the traits
    showing no intergroup difference at all are excluded. Note that if
    you have a large number of groups (say, 10 groups), a trait with
    strictly equal frequencies for the last 8 groups may be considered
    as useful according to this criterion if there is a significant
    difference for the first two groups. This criterion will select all
    traits that can be useful for a given pair of groups, even if they
    are nondiscriminatory for all the other ones.
    \item \sQuote{Exclude traits with overall MD} lower than a given
    threshold: it is a simple way of removing the traits with quite similar
    frequencies across groups (the \sQuote{overall MD} is defined as the sum
    of the variable's measures of divergence over all pairs of groups). This
    criterion aims to select the traits whose frequency differs
    substantially across most or all groups.
  }
  These four options are designed to avoid negative MMD values.
  \item Some groups/populations can be manually excluded from the
  analysis. This may be useful if very few individuals belonging to a
  given population could be recorded for the variables retained by the
  criteria described above.

  \item A MDS plot and a hierarchical clustering, done using MMD
  dissimilarities as inputs, are displayed in the last two tabs. As MMD
  can sometimes be negative, those negatives values are replaced by
  zeros, so that the MMD matrix can be seen as a symmetrical distance
  matrix. Please note that the classical two-dimensional metric MDS plot
  cannot be displayed if there is only one positive eigenvalue. Several
  MDS options are proposed, cf. the help page of the \code{smacofSym}
  function from the R package \code{smacof} for detailed technical
  information.
}
}

\value{
  The function returns no value by itself, but all results can be
  individually downloaded through the graphical interface.
  \itemize{
    \item The \sQuote{true} MMD values (i.e., which can be negative in
    the case of small samples with similar traits frequencies, cf. Irish
    2010) and their standard deviations are presented in the matrix
    labelled \sQuote{MMD values (upper triangular part) and associated
    SD values (lower triangular part)}.
    \item A MMD value can be considered as significant if it is greater
    than twice its standard deviation. Significance is assessed in
    another ad-hoc table of results.
    \item The negative MMD values, if any, are replaced by zeros in the
    \sQuote{Symmetrical matrix of MMD values}.
  }
}

\references{
  Harris, E. F. and Sjøvold, T. (2004) Calculation of Smith's mean
  measure of divergence for intergroup comparisons using nonmetric
  data. \emph{Dental Anthropology}, \bold{17}(3), 83--93.

  Irish, J. (2010) The mean measure of divergence: Its utility in
  model-free and model-bound analyses relative to the Mahalanobis D2
  distance for nonmetric traits. \emph{American Journal of Human
  Biology}, \bold{22}, 378--395. doi: 10.1002/ajhb.21010

  Nikita, E. (2015) A critical review of the mean measure of divergence
  and Mahalanobis distances using artificial data and new approaches to
  the estimation of biodistances employing nonmetric
  traits. \emph{American Journal of Physical Anthropology}, \bold{157},
  284--294. doi: 10.1002/ajpa.22708

  Santos, F. (2018) AnthropMMD: an R package with a graphical user
  interface for the mean measure of divergence. \emph{American Journal
  of Physical Anthropology}, \bold{165}(1), 200--205. doi:
  10.1002/ajpa.23336
}

\author{
  Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}
}

\examples{
# An example of valid binary dataset:
data(toyMMD)
head(toyMMD)
# An example of valid table:
data(absolute_freqs)
absolute_freqs
# Launch the UI:
\dontrun{ start_mmd()}
}
