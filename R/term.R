#' Term of LegCo
#'
#' Fetch the basic information of LegCo terms.
#'
#' This function corresponds to the \emph{Tterm} data endpoint of the Meeting
#' Schedule Database.
#'
#' @inherit meeting_schedule-db
#'
#' @examples
#' \donttest{
#' # Fetch all LegCo terms
#' x <- term()
#' }
#'
#' @export
#' 
term <- function(term_id = NULL, date = NULL, extra_param = NULL, verbose = TRUE) {
  query <- "Tterm?$select=term_id,term,start_date,end_date"
  
  filter_args <- {}
  
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
  
  df <- legco_api("schedule", query, 10000, FALSE, verbose)
  
  colnames(df) <- .unify_colnames(colnames(df)) # in utils-misc.R
  
  df
}

#' @rdname term
#' @export
legco_term <- term
