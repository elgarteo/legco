#' legco: Provides Easy Downloading Capabilities for the UK Parliament API
#'
#' Provides functions to request data from the Hong Kong Legislative Council APIs.
#' 
#' For more details please see the API documentation on <https://www.legco.gov.hk/odata/english/hansard-db.html>.
#'
#' In addition to the standard function names, each function in the
#' `legco` package has a wrapper where the name is prefixed with
#' `'legco_'`. For example, both `speakers()` and
#' `legco_speakers()` will return the same result. This is because
#' function names are taken from the specific API on
#' <https://www.legco.gov.hk/odata/english/hansard-db.html>, but they are often not very
#' informative and could clash with functions in other packages (e.g.
#' `speakers()` is not a term unique to LegCo).
#' 
#' This package is in no way officially related to or endorsed by the Legislative Council of Hong Kong.
#'
#'
#' @docType package
#' @name legco
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @importFrom stringr str_detect
NULL