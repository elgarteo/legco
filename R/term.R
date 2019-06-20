#' Terms of LegCo
#'
#' Fetch the number and duration of LegCo terms.
#'
#' @param id The id of a term, or a vector of ids. If `NULL`, returns terms.
#'   Defaults to `NULL`.
#'
#' @param date Only fetch the term in which the specified date falls within.
#'   Accepts character values in `'YYYY-MM-DD'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a date with `as.Date()`. Defaults to `NULL`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
term <- function(id = NULL, date = NULL, extra_param = NULL, verbose = TRUE) {
  query <- "Tterm?$select=term_id,term,start_date,end_date"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("term_id", id))
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
  
  df <- legco_api("schedule", query, 10000, verbose)
  
  if (!is.null(df)) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    
    df
  }
}

#' @rdname term
#' @export
legco_term <- term
