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
This R package aims to:
1. Fetch data from the LegCo open data APIs in R
2. Produce meaningful and usable result from the APIs

The LegCo APIs mainly work with hansard PDF files and rely on the file structure like 
section headers and paragraphs to return data. For example, a query to fetch questions raised by LegCo
members only tells you in which hansard PDF file and in which section the questions are located in.
To find out the question text and the answering body, you'd have to crawl through the hansard structure
and make multiple API calls.

This package solves just that by providing functions that return more usable result from the API.

## How it works
This package utilises the following LegCo APIs:
* [Hansard Database](https://www.legco.gov.hk/odata/english/hansard-db.html)
* [Attendance Database](https://www.legco.gov.hk/odata/english/attendance-db.html)
* [Bills Database](https://www.legco.gov.hk/odata/english/billsdb.html)

## Disclaimer
This package is not affiliated nor endorsed by the Legislative Council of Hong Kong.
