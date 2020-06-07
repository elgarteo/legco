#' Session of LegCo
#'
#' Fetch the basic information of LegCo sessions.
#'
#' This function corresponds to the \emph{Tsession} data endpoint of the Meeting
#' Schedule Database.
#'
#' @inherit meeting_schedule-db
#'
#' @examples
#' \donttest{
#' # Fetch all LegCo sessions of the fifth term
#' x <- session(term_id = 4)
#' }
#'
#' @export
#' 
session <- function(session_id = NULL, term_id = NULL, date = NULL, extra_param = NULL, 
                    count = FALSE, verbose = TRUE) {
  query <- "Tsession?$select=session_id,session_code,term_id,start_date,end_date"
  
  filter_args <- {}
  
  if (!is.null(session_id)) {
    filter_args <- c(filter_args, .generate_filter("session_id", session_id))
  }
  
  if (!is.null(term_id)) {
    filter_args <- c(filter_args, .generate_filter("term_id", term_id))
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
    colnames(df) <- .unify_colnames(colnames(df)) # in utils-misc.R
  }
  
  df
}

#' @rdname session
#' @export
legco_session <- session
