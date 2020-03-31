#' Attendance of LegCo Members
#'
#' Fetch attendance record of LegCo committee meetings
#'
#' This function corresponds to the \emph{Tattendance} data endpoint of the
#' Meeting Attendance Database.
#'
#' @param attn the attendance. \code{"p"} returns members who were present.
#'   \code{"a"} returns members who were absent. \code{"all"} returns all
#'   members. Defaults to \code{"all"}.
#'   
#' @inheritParams hansard-db
#' @inheritParams meeting_schedule-db
#'
#' @seealso LegCo API documentation for the Attendance database:
#'   \url{https://www.legco.gov.hk/odata/english/attendance-db.html}
#'
#' @examples
#' \donttest{
#' # Fetch members who were absent from the Subcommittee on Issues
#' # Relating to Bazaars meetings on October 13, 2017
#' x <- attendance(committee_id = 2704, from = "2017-10-13", to = "2017-10-13", attn = "a")
#' }
#'
#' @export
#' 
attendance <- function(committee_id = NULL, meet_id = NULL, member_id = NULL, 
                       attn = "all", from = '1900-01-01', to = Sys.Date(), 
                       n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "TAttendance?$select=committee_id,meet_id,meeting_name_english,meeting_name_chinese,meeting_start_date_time,member_id,member_name_english,member_name_chinese,present_absent"
  
  filter_args <- {}
  
  if (!is.null(committee_id)) {
    filter_args <- c(filter_args, generate_filter("committee_id", committee_id))
  }
  
  if (!is.null(meet_id)) {
    filter_args <- c(filter_args, generate_filter("meet_id", meet_id))
  }
  
  if (!is.null(member_id)) {
    filter_args <- c(filter_args, generate_filter("member_id", member_id))
  }
  
  attn <- tolower(attn)
  if (attn == "p") {
    filter_args <- c(filter_args, "present_absent eq 'Present'")
  } else if (attn == "a") {
    filter_args <- c(filter_args, "present_absent eq 'Absent'")
  }
  
  from <- as.Date(from)
  from <- paste0(format(from, "%Y-%m-%d"), "T00:00:00")
  to <- as.Date(to)
  to <- paste0(format(to, "%Y-%m-%d"), "T23:59:59")
  filter_args <- c(filter_args, paste0("meeting_start_date_time ge datetime\'", from, 
                                       "\' and meeting_start_date_time le datetime\'", to, "\'"))
  
  query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("attn", query, n, count, verbose)
  
  if (!count) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
  }
  
  df
}

#' @rdname attendance
#' @export
legco_attendance <- attendance
