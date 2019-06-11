#' Meetings of LegCo Committees
#'
#' Fetch information of LegCo committee meetings.
#'
#' @param id The id of a meeting, or a vector of ids. If `NULL`, returns all
#'   meetings. Defaults to `NULL`.
#'
#' @param from Only fetch meetings on or after this date. Accepts character
#'   values in `'YYYY-MM-DD'` format, and objects of class `Date`, `POSIXt`,
#'   `POSIXct`, `POSIXlt` or anything else that can be coerced to a date with
#'   `as.Date()`. Defaults to `'1900-01-01'`.
#'
#' @param to Only meetings on or before this date. Accepts character values in
#'   `'YYYY-MM-DD'` format, and objects of class `Date`, `POSIXt`, `POSIXct`,
#'   `POSIXlt` or anything else that can be coerced to a date with `as.Date()`.
#'   Defaults to the current system date.
#'
#' @param type The type of meeting. If `all`, returns membership of committees
#'   of all terms.
#'
#' @param term_id The id of a term, or a vector of ids. If `'open'`, returns
#'   open meetings. If `'closed'`, returns closed meetings. If `'all'`, returns
#'   all meetings. Defaults to `'all'`.
#'
#' @param n The number of records to request. Defaults to `1000`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
meeting <- function(id = NULL, from = '1900-01-01', to = Sys.Date(), type = "all", 
                    term_id = NULL, n = 1000, extra_param = NULL, verbose = TRUE) {
  query <- "Tmeeting?$select=meet_id,subject_eng,subject_chi,start_date_time,meeting_type_eng,meeting_type_chi,venue_code,venue_name_eng,venue_name_chi,term_id,agenda_url_eng,agenda_url_chi"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("meet_id", id))
  }
  
  from <- as.Date(from)
  from <- paste0(format(from, "%Y-%m-%d"), "T00:00:00")
  to <- as.Date(to)
  to <- paste0(format(to, "%Y-%m-%d"), "T23:59:59")
  filter_args <- c(filter_args, paste0("start_date_time ge datetime\'", from, 
                                       "\' and start_date_time le datetime\'", to, "\'"))
  
  type <- paste0(toupper(substring(type, 1, 1)), substring(type, 2))
  if (type == "open") {
    filter_args <- c(filter_args, "meeting_type_eng eq 'Open'")
  } else if (type == "closed") {
    filter_args <- c(filter_args, "meeting_type_eng eq 'Closed'")
  }
  
  if (!is.null(term_id)) {
    filter_args <- c(filter_args, generate_filter("term_id", term_id))
  }
  
  query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("schedule", query, n, verbose)
  
  if (!is.null(df)) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    
    df
  }
}

#' @rdname meeting
#' @export
legco_meeting <- meeting
