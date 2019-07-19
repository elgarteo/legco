#' Attendance of LegCo Members
#'
#' Fetch attendance record of LegCo committee meetings
#'
#' @param committee_id The id of a committee, or a vector of ids. If `NULL`,
#'   returns records of meetings from all committee. Defaults to `NULL`.
#'
#' @param meet_id The id of a meeting, or a vector of ids. If `NULL`, returns
#'   records of all meetings. Defaults to `NULL`.
#'
#' @param member_id The id of a member, or a vector of ids. Defaults to `NULL`.
#'
#' @param attn The attendance. `'p'` returns members who were present. `'a'`
#'   returns members who were absent. `'all'` returns all members. Defaults to
#'   `'all'`.
#'
#' @param from Only fetch records of meetings on or after this date. Accepts
#'   character values in `'YYYY-MM-DD'` format, and objects of class `Date`,
#'   `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced to a
#'   date with `as.Date()`. Defaults to `'1900-01-01'`.
#'
#' @param to Only fetch records of meetings on or before this date. Accepts
#'   character values in `'YYYY-MM-DD'` format, and objects of class `Date`,
#'   `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced to a
#'   date with `as.Date()`. Defaults to the current system date.
#'
#' @param n The number of records to request. Defaults to `1000`.
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
