# legco: Accessing Hong Kong Legislative Council Data
An R package to fetch data from the Hong Kong Legislative Council (LegCo) APIs. 

Inspired by [evanodell/hansard](https://github.com/evanodell/hansard).

Under active development.

To install this package, run:
```
#install.packages("devtools")
devtools::install_github("elgarteo/legco")
```

## What it does
This R package provides access to the LegCo open data APIs in R. The functions in this package 
correspond to the data endpoints of the APIs.

It is recommended to also install an addon package [elgarteo/legcoplus](https://github.com/elgarteo/legcoplus)
which provides functions that facilitate the use LegCo's data.

## How it works
This package compiles the correct query and request data from the following LegCo APIs:
* [Attendance Database](https://www.legco.gov.hk/odata/english/attendance-db.html)
* [Bills Database](https://www.legco.gov.hk/odata/english/billsdb.html)
* [Hansard Database](https://www.legco.gov.hk/odata/english/hansard-db.html)
* [Schedule Database](https://www.legco.gov.hk/odata/english/schedule-db.html)
* [Voting Result Database](https://www.legco.gov.hk/odata/english/vrdb.html)

## How to use
Read the [vignettes](https://elgarteo.github.io/legco/) for details.

## Disclaimer
This package is not affiliated or endorsed by the Legislative Council of Hong Kong. 

The Legislative Council of Hong Kong is the copyright owner of data retieved from its open data APIs.
