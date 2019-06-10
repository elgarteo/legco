# legco: Accessing Hong Kong Legislative Council Data
An R package to fetch data from the Hong Kong Legislative Council (LegCo) APIs. 

Inspired by [evanodell/hansard](https://github.com/evanodell/hansard).

Under development.

To install this package, run:
```
#install.pacakges("devtools")
devtools::install_github("elgarteo/legco")
```

## What it does
This R package provides access to the LegCo open data APIs in R. The functions in this package 
correspond to the data endpoints of the APIs.

Because of the inherent structure of the LegCo database, results from the APIs may not be in 
the most usable form for the general public. ~~It is therefore recommended to also install an
addon package [elgarteo/legcoplus]() which provides functions that generate more usable outputs.~~

## How it works
This package compiles the correct query and request data from the following LegCo APIs:
* [Hansard Database](https://www.legco.gov.hk/odata/english/hansard-db.html)
* [Attendance Database](https://www.legco.gov.hk/odata/english/attendance-db.html)

## Disclaimer
This package is not affiliated or endorsed by the Legislative Council of Hong Kong.
