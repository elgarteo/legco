#' Hansard of LegCo
#'
#' Fetch metadata and URLs of the hansard files of LegCo council meetings.
#'
#' @param id The id of a hansard file, or a vector of ids. If `NULL`, returns a
#'   list of all hansard files. Defaults to `NULL`.
#'
#' @param lang The language of hansard files to search from. `'en'` returns the
#'   English version. `'zh'` returns the Traditional Chinese version. Defaults
#'   to `'en'`.
#'
#' @param from Only includes hansards of meetings on or after this date. Accepts
#'   character values in `'YYYY-MM-DD'` format, and objects of class `Date`,
#'   `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced to a
#'   date with `as.Date()`. Defaults to `'1900-01-01'`.
#'
#' @param to Only includes hansards of meetings on or before this date. Accepts
#'   character values in `'YYYY-MM-DD'` format, and objects of class `Date`,
#'   `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced to a
#'   date with `as.Date()`. Defaults to the current system date.
#'
#' @param floor Whether to fetch the floor version of the hansard files. The
#'   floor version is the first presented version of hansard file in the
#'   original language delivered by the speakers in LegCo. If `TRUE`, the
#'   language option is ignored. Defaults to `FALSE`.
#'
#' @param n The number of hansard files to request. Defaults to `1000`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'   
#' @param count If `TRUE`, returns only the total count of records that matches
#'   the paramter(s) instead of the result. Defaults to `FALSE`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
hansard <- function(id = NULL, lang = "en", from = '1900-01-01', to = Sys.Date(), floor = FALSE, 
                    n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Hansard?$select=HansardID,MeetingDate,AgendaDate,isCEQandA,isCEQT,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("HansardID", id)) # in utils-misc.R
  }
  
  if(is.null(id)) {
    lang <- tolower(lang)
    if (floor) {
      filter_args <- c(filter_args, "HansardType eq 'Floor'")
    } else if (lang == "en") {
      filter_args <- c(filter_args, "HansardType eq 'English'")
    } else if (lang == "zh") {
      filter_args <- c(filter_args, "HansardType eq 'Chinese'")
    } 
  }
  
  from <- as.Date(from)
  to <- as.Date(to)
  filter_args <- c(filter_args, paste0("MeetingDate ge datetime\'", from, 
                    "\' and MeetingDate le datetime\'", to, "\'"))
  
  
  query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))

  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  legco_api("hansard", query, n, count, verbose)
}

#' @rdname hansard
#' @export
legco_hansard <- hansard
