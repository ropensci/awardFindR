# awardFindR
<!-- badges: start -->
[![R-CMD-check](https://github.com/PESData/awardFindR/workflows/R-CMD-check/badge.svg)](https://github.com/PESData/awardFindR/actions)
[![Codecov test coverage](https://codecov.io/gh/PESData/awardFindR/branch/master/graph/badge.svg)](https://codecov.io/gh/PESData/awardFindR?branch=master)
<!-- badges: end -->

`awardFindR` is an framework that scrapes a variety of grant databases for specific keywords, rendering the results together in a large `data.frame`. It is designed to be modular and extensible, supporting a wide variety of APIs and other means of making award data available online. 

## How the package works

The `awardFindR()` routine is the top-level function in this package, and by default will call on each source routine. `awardFindR()` has parameters to change keyword, sources and dates as search criteria. Dates are interpreted with varying degrees of precision based on the data available from each source. See included help on individual sources to understand their respective limitations.

Individual sources have functions of their own. `nsf_get()` specifically fetches results from NSF, and `nih_get()` fetches results from NIH, for example. 

## Dependencies

This package depends on `rvest` and `httr`.

## Installation
Install `awardFindR` directly from this repository using the `remotes` package
```
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("PESData/awardFindR")
```
