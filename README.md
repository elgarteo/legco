
<!-- README.md is generated from README.Rmd. Please edit that file -->

# legco: Accessing Hong Kong Legislative Council Data

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/legco)](https://cran.r-project.org/package=legco)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

R bindings for the Hong Kong Legislative Council (LegCo) API.

本套件提供能在R中直接透過香港立法會開放數據應用程式介面擷取數據的函數。

To install this package, run:

``` r
install.packages("legco")

# To install the development version:
#install.packages("devtools")
devtools::install_github("elgarteo/legco")
```

## What it does

This R package provides access to the LegCo open data API in R. The
functions in this package correspond to the data endpoints of the API.

## How it works

This package compiles the correct query and requests data from the
following databases of the LegCo API:

- [Bills
  Database](https://www.legco.gov.hk/en/open-legco/open-data/bills-database.html)
- [Hansard
  Database](https://www.legco.gov.hk/en/open-legco/open-data/hansard-database.html)
- [Meeting Attendance
  Database](https://www.legco.gov.hk/en/open-legco/open-data/meeting-attendance.html)
- [Meeting Schedule
  Database](https://www.legco.gov.hk/en/open-legco/open-data/meeting-schedule.html)
- [Voting Result
  Database](https://www.legco.gov.hk/en/open-legco/open-data/voting-result-database.html)

## How to use

Read the [vignettes](https://elgarteo.github.io/legco/) for details.

## Disclaimer

This package is not affiliated or endorsed by the Legislative Council of
Hong Kong.

The Legislative Council of Hong Kong is the copyright owner of data
retrieved from its open data API.
