AnthropMMD 
==========

## Installation of the R-package AnthropMMD using *devtools*

### Install prerequisites

1. Install *devtools* by typing the following command line into the R console:

	install.packages("devtools")

2. Install build environment:
    * **Windows:** Install latest version of *[Rtools](https://cran.r-project.org/bin/windows/Rtools/)*. During the installation process, make sure to select *"Edit the system path"*.
    * **OSX:** Install *[XCODE](https://developer.apple.com/xcode/)*

### Install AnthropMMD

Run the following commands in R:
        
	library(devtools)
	install_git('https://gitlab.com/f.santos/anthropmmd.git')

### Run AnthropMMD

To start the graphical interface, run the following commands into the R console:

	library(AnthropMMD)
	StartMMD()
	
## Installation of the R-package AnthropMMD from CRAN

The latest stable version of AnthropMMD is also available on CRAN, and can be installed by typing the following command line into the R console:

	install.packages("AnthropMMD", dep=TRUE)

## Citing AnthropMMD

The users of AnthropMMD that are willing to cite the package in a scientific article can find citation information by typing:

	citation("AnthropMMD")

into the R console.
