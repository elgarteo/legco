# legco
An R package to fetch data from the Hong Kong Legislative Council (LegCo) API. 

Inspired by [evanodell/hansard](https://github.com/evanodell/hansard).

Under development.

## What it does
This R package aims:
1. To provide easy access to the LegCo open data APIs
2. To facilitate the use of the APIs

Point 1 is easy to understand. Point 2 essentially means an add-on improvement for the APIs.
The LegCo hansard API is built upon the hansard PDF files. It works with the structure of the files 
like section headers. Hansard files are created based on the workflow of the LegCo. 
But most of us understand LegCo matters in a different way. We are interested in the bills, motions and questions etc.

This inherent difference means the public may have a hard time using the LegCo APIs.
For example, the "Questions" data endpoint returns only the subject of the question 
and its location in the hansard file.
In order to fetch the questions put on the government by a certain LegCo member, 
one has to crawl through the hansard structure and make multiple API calls.
This package solves that by providing functions to do exactly that.

---

This package is in no way officially related to or endorsed by the Legislative Council of Hong Kong.
