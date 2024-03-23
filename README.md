# awardFindR
<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/ropensci/awardFindR/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/awardFindR/actions)
[![Codecov test coverage](https://codecov.io/gh/ropensci/awardFindR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ropensci/awardFindR?branch=master)
[![](https://badges.ropensci.org/432_status.svg)](https://github.com/ropensci/software-review/issues/432)
<!-- badges: end -->

`awardFindR` is a framework that scrapes and searches a variety of grant and award databases for specific keywords. These include major US federal agencies like NSF and NIH as well as private organizations like the Bill & Melinda Gates Foundation. Results from searching each of these databases are collected and made available to users. The package is designed to be modular and extensible, supporting any number of APIs and other web-based sources that provide award data available online.

Packages that have provided similar functionality include [awardsBot](https://github.com/NCEAS/awards-bot).

## Installation
Install `awardFindR` directly from this repository using the `remotes` package
```
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("ropensci/awardFindR")
```

## Supported sources

- [Arnold Ventures](https://www.arnoldventures.org/grants-search) (`arnold`)
- [Carnegie Corporation of New York](https://www.carnegie.org/grants/grants-database/) (`carnegie`)
- [Bill & Melinda Gates Foundation](https://www.gatesfoundation.org/) (`gates`)
- [MacArthur Foundation](https://www.macfound.org/grants/) (`macarthur`)
- [Mellon Foundation](https://www.mellon.org/grant-database/) (`mellon`)
- [NIH RePORTER](https://projectreporter.nih.gov/reporter.cfm) (`nih`)
- [National Science Foundation](https://www.nsf.gov/awardsearch/) (`nsf`)
- [Open Philanthropy](https://www.openphilanthropy.org/giving/grants) (`ophil`)
- [Open Society Foundations](https://www.opensocietyfoundations.org/grants/past) (`osociety`)
- [Rockefeller Foundation](https://www.rockefellerfoundation.org/) (`rockefeller`)
- [Russell Sage Foundation](https://www.russellsage.org) (`rsf`)
- [Robert Wood Johnson Foundation](https://www.rwjf.org/en/how-we-work/grants-explorer.html) (`rwjf`)
- [Alfred P. Sloan Foundation](https://sloan.org/grants-database) (`sloan`)
- [Social Science Research Council](https://www.ssrc.org) (`ssrc`)
- [John Templeton Foundation](https://www.templeton.org/grants/grant-database) (`templeton`)
- [USASpending.gov](https://www.usaspending.gov/search) (`usaspend`)

## How the package works

`search_awards` has parameters to change keywords, sources and dates as search criteria, which are passed on to source routines. Dates are interpreted with varying degrees of precision based on the data available from each source. See included help on individual sources to understand their respective limitations.

Individual sources have functions of their own. `get_nsf` specifically fetches results from NSF, and `get_nih` fetches results from NIH, for example. These are meant to provide end users with higher levels of detail if they're interested in a specific source.

### Quick introduction

Search for all awards matching a keyword since the default cutoff of Jan 1, 2019 to today

```
awardFindR::search_awards(keywords="ethnography")
```

See the included vignette for additional examples.

For those interested in the results from a specific source, each source has its own function. For example, someone interested in NSF results for "ethnography" between 2018 and 2020 could run the following:

```
awardFindR::get_nsf("ethnography", "2018-01-01", "2020-01-01")
```

Similar functions exist for each supported source. See included help for further details, as the arguments differ slightly between each.

### Dependencies

This package depends on `rvest`, `xml2` and `httr`.

### Funding

This package was developed with support from [a grant from the Sloan Foundation](https://sloan.org/grant-detail/8725).
