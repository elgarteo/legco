#' Meetings of LegCo Committees
#'
#' Fetch basic information of LegCo committee meetings.
#'
#' This function corresponds to the \emph{Tmeeting} data endpoint of the Meeting
#' Schedule Database.
#'
#' @inherit meeting_schedule-db
#' @inheritParams hansard-db
#'
#' @examples
#' \donttest{
#' # Fetch all meeting conducted on March 20, 2019
#' x <- meeting(from = "2019-03-20", to = "2019-03-20")
#' }
#'
#' @export
#' 
meeting <- function(slot_id = NULL, meet_id = NULL, from = '1900-01-01',
                    to = Sys.Date(), type = "all", term_id = NULL,
                    n = 10000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Tmeeting?$select=slot_id,meet_id,subject_eng,subject_chi,start_date_time,
  meeting_type_eng,meeting_type_chi,venue_code,venue_name_eng,venue_name_chi,term_id,
  agenda_url_eng,agenda_url_chi"
  
  filter_args <- {}
  
  if (!is.null(slot_id)) {
    filter_args <- c(filter_args, generate_filter("slot_id", slot_id))
  }
  
  if (!is.null(meet_id)) {
    filter_args <- c(filter_args, generate_filter("meet_id", meet_id))
  }
  
  from <- as.Date(from)
  from <- paste0(format(from, "%Y-%m-%d"), "T00:00:00")
  to <- as.Date(to)
  to <- paste0(format(to, "%Y-%m-%d"), "T23:59:59")
  filter_args <- c(filter_args, paste0("start_date_time ge datetime\'", from, 
                                       "\' and start_date_time le datetime\'", to, "\'"))
  
  type <- tolower(type)
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
  
  df <- legco_api("schedule", query, n, count, verbose)
  
  if (!count) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
  }
  
  df
}

#' @rdname meeting
#' @export
legco_meeting <- meeting
