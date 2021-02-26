# awardFindR
<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/PESData/awardFindR/workflows/R-CMD-check/badge.svg)](https://github.com/PESData/awardFindR/actions)
[![Codecov test coverage](https://codecov.io/gh/PESData/awardFindR/branch/master/graph/badge.svg)](https://codecov.io/gh/PESData/awardFindR?branch=master)
<!-- badges: end -->

`awardFindR` is a framework that scrapes and searches a variety of grant and award databases for specific keywords. These include major US federal agencies like NSF and NIH as well as private organizations like the Bill & Melinda Gates Foundation. Results from searching each of these databases are collected and made available to users. The package is designed to be modular and extensible, supporting any number of APIs and other web-based sources that provide award data available online.

Packages that have provided similar functionality include [awardsBot](https://github.com/NCEAS/awards-bot).

Sources supported currently include:

- [Arnold Ventures](https://www.arnoldventures.org/grants-search)
- [Carnegie Corporation of New York](https://www.carnegie.org/grants/grants-database/)
- [Federal Reporter](https://federalreporter.nih.gov/)
- [Bill & Melinda Gates Foundation](https://www.gatesfoundation.org/)
- [MacArthur Foundation](https://www.macfound.org/grants/)
- [Mellon Foundation](https://mellon.org/grants/grants-database/advanced-search/)
- [National Endowment for the Humanities](https://securegrants.neh.gov/publicquery/main.aspx)
- [NIH RePORTER](https://projectreporter.nih.gov/reporter.cfm)
- [National Science Foundation](https://nsf.gov/awardsearch/)
- [Open Philanthrophy](https://www.openphilanthropy.org/giving/grants)
- [Open Society Foundations](https://www.opensocietyfoundations.org/grants/past)
- [Rockefeller Foundation](https://www.rockefellerfoundation.org/)
- [Russell Sage Foundation](https://www.russellsage.org)
- [Robert Wood Johnson Foundation](https://www.rwjf.org/en/how-we-work/grants-explorer.html)
- [Alfred P. Sloan Foundation](https://sloan.org/grants-database)
- [Social Science Research Council](https://www.ssrc.org)
- [John Templeton Foundation](https://www.templeton.org/grants/grant-database)
- [USASpending.gov](https://www.usaspending.gov/search)

## How the package works

`awardFindR` has parameters to change keywords, sources and dates as search criteria, which are passed on to source routines. Dates are interpreted with varying degrees of precision based on the data available from each source. See included help on individual sources to understand their respective limitations.

Individual sources have functions of their own. `nsf_get` specifically fetches results from NSF, and `nih_get` fetches results from NIH, for example. These are meant to provide end users with higher levels of detail if they're interested in a specific source.

### Quick introduction

Search for all awards matching a keyword since the default cutoff of Jan 1, 2019 to today

```
awardFindR(keywords="ethnography")
```

See the included vignette for additional examples.

## Installation
Install `awardFindR` directly from this repository using the `remotes` package
```
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("PESData/awardFindR")
```
### Dependencies

This package depends on `rvest`, `xml2` and `httr`.
