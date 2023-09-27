![DeployBook](https://github.com/NIWAFisheriesModelling/r4Casal2/actions/workflows/deploy_bookdown.yml/badge.svg)

# r4Casal2

An R package that extends the functionality of the base [Casal2](https://github.com/alistairdunn1/CASAL2) R package. The purpose of `r4Casal2` is to aid in visualising, intepreting and diagnosing Casal2 models. 

## Casal2 clone

This respository is a clone of r4Casal2 of https://github.com/NIWAFisheriesModelling/R4Casal2. It is intended to have additional bug fixes and enhancements for potential inclusion into the original NIWA Casal2 and r4Casal2 code.

## Install `r4Casal2` from remote repository

`r4Casal2` depends on the `Casal2` R library. `r4Casal2` provides helper functions for visualising and summarising. `r4Casal2` is available from [here](https://github.com/alistairdunn1/r4Casal2), and can be installed directly from GitHub using


```r
devtools::install_github("https://github.com/alistairdunn1/CASAL2/", subdir = "R-libraries/casal2")
```

Addition packages are required, and can be installed using

```r
install.packages(c("reshape2", "dplyr", "ggplot2", "mvtnorm", "DHARMa","MASS", "knitr"))
```

The `Casal2` R package can be downloaded from [here](https://github.com/alistairdunn1/CASAL2/tree/master/R-libraries) or installed using

```r
devtools::install_github("https://github.com/alistairdunn1/CASAL2", subdir="R-libraries/casal2", ref = "HEAD")
```

## Query Functionality

Once the library is installed you can query the functionality to see the functions `library(help="r4Casal2")` or the [**the online book**](https://niwafisheriesmodelling.github.io/r4Casal2/).

## Issues

If you have an issues please create a github issue

## Contributing to r4Casal2

If you have some R code for Casal2 models or output, the best way to contribute is to clone or fork this repository and push and pull changes. Otherwise contact the owner of this repository.
