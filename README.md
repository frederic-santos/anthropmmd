AnthropMMD 
==========
[![pipeline status](https://gitlab.com/f-santos/anthropmmd/badges/master/pipeline.svg)](https://gitlab.com/f-santos/anthropmmd/commits/master)
[![coverage report](https://gitlab.com/f-santos/anthropmmd/badges/master/coverage.svg)](https://gitlab.com/f-santos/anthropmmd/commits/master)

`AnthropMMD` is an R package for an easy calculation of the mean measure of divergence (MMD). It offers both a graphical user interface (for scientists who are not R programmers), and a set of functions which can be used through the command line interface (for reproducible research or scientists who are already familiar with R).

For the technical and scientific aspects, the package is better described in its [vignette](https://cran.r-project.org/package=AnthropMMD/vignettes/intro_AnthropMMD.html).

Feature requests or bug reports are welcome.

# Installation of the R package AnthropMMD from GitLab

## Install prerequisites

Make sure that Git and a recent version of R are installed. Then:

1. Install the R package `remotes` by typing the following command line into the R console:

```r
install.packages("remotes")
```

2. Install build environment:
    * **Windows:** Install latest version of *[Rtools](https://cran.r-project.org/bin/windows/Rtools/)*. During the installation process, make sure to select *"Edit the system path"*.
    * **OSX:** Install *[XCODE](https://developer.apple.com/xcode/)*

## Install AnthropMMD

Run the following command in R:

```r
remotes::install_git('https://gitlab.com/f-santos/anthropmmd.git')
```

# Installation of AnthropMMD from CRAN

The latest stable version of AnthropMMD is also available on CRAN, and can be installed by typing the following command line into the R console:

```r
install.packages("AnthropMMD", dep = TRUE)
```
	
# Run AnthropMMD

To start the graphical interface, run the following commands into the R console:

```r
library(AnthropMMD)
start_mmd()
```

# Citing AnthropMMD

To cite the package in a scientific article, citation information can be found by typing:

```r
citation("AnthropMMD")
```

into the R console.
