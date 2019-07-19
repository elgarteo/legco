#' Session of LegCo
#'
#' Fetch the basic information of LegCo sessions.
#'
#' @param id The id of a session, or a vector of ids. If `NULL`, returns terms.
#'   Defaults to `NULL`.
#'
#' @param term_id The id of a term, or a vector of ids. If `NULL`, returns
#'   sessions of all terms.
#'
#' @param date Only fetch the session in which the specified date falls within.
#'   Accepts character values in `'YYYY-MM-DD'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a date with `as.Date()`. Defaults to `NULL`.
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
session <- function(id = NULL, term_id = NULL, date = NULL, extra_param = NULL, 
                    count = FALSE, verbose = TRUE) {
  query <- "Tsession?$select=session_id,session_code,term_id,start_date,end_date"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("session_id", id))
  }
  
  if (!is.null(term_id)) {
    filter_args <- c(filter_args, generate_filter("term_id", term_id))
  }
  
  if (!is.null(date)) {
    date <- as.Date(date)
    filter_args <- c(filter_args, paste0("start_date le datetime\'", date, 
                                         "\' and end_date ge datetime\'", date, "\'"))
  }
  
  if(!is.null(filter_args)) {
    query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  }
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("schedule", query, 10000, count, verbose)
  
  if (!count) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
  }
  
  df
}

#' @rdname session
#' @export
legco_session <- session
