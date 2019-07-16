# AnthropMMD 3.0.1 (Release date: 2019-07-16)

## Minor changes
* Fixed warning when the angular transformation is not specified in `mmd` function.
* Fixed typos in package vignette and documentation files.

# AnthropMMD 3.0.0 (Release date: 2019-07-05)

## Change in dependencies
* Due to some new features implemented in `AnthropMMD`, the package now depends on R 3.5.0 or greater.
* `AnthropMMD` now imports `plotrix`.
* `AnthropMMD` now suggests `covr`, `knitr`, `rmarkdown` and `testthat`.

## Change of License
* `AnthropMMD` is now distributed under CeCILL 2.1 license (instead of GPL 3).

## User visible changes
* `AnthropMMD` can now be used by command lines, and not only as an R-shiny application. The main goal is to make AnthropMMD suitable for reproducible research.
* An example data file, `toyMMD`, is now available.
* A vignette is now available.
* Improved display of group labels on the MDS plots.
* Some small improvements in documentation files.

## Other changes
* A large majority of comments in the R source files have been translated from French into English.
* The reliability of future updates has been improved by implementing unit tests; and a CI pipeline has been set on the GitLab repo.
* The indentation style in `.R` and `.Rd` files now follows the R standards.

## Bug fixes
* The Shiny app does not crash anymore if the user specifies a wrong field separator.
* Fix in the computation of Spearman's measure of agreement for MDS, where the distances where erroneously counted twice. Although the impact was generally negligible, this resulted in a small overestimation of the quality of agreement.

# AnthropMMD 2.5.3 (Release date: 2019-03-18)

* First release on GitLab
* Added LICENSE, NEWS and README files
* Change of GPL license (GPL3 instead of GPL2)
* Updated DESCRIPTION file
