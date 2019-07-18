#' legco: Provides Access to the Hong Kong Legislative Council APIs
#'
#' Provides functions to request data from the Hong Kong Legislative Council
#' APIs.
#'
#' For more details please see the API documentation on
#' \url{https://www.legco.gov.hk/odata/english/hansard-db.html} and
#' \url{https://www.legco.gov.hk/odata/english/attendance-db.html}.
#'
#' In addition to the standard function names, each function in the `legco`
#' package has a wrapper where the name is prefixed with `'legco_'`. For
#' example, both `speakers()` and `legco_speakers()` will return the same
#' result. This is because function names are taken from the data endpoints
#' provided by the APIs on , which nonetheless are often not very informative
#' and could clash with functions in other packages (e.g. `speakers()` is not a
#' term unique to LegCo).
#'
#' This package is not officially related to or endorsed by the Legislative
#' Council of Hong Kong.
#'
#' @docType package
#' @name legco
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
NULL
