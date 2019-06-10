#' Attendance of LegCo Members
#'
#' Fetch attendance records of LegCo meetings
#'
#' @param committee_id The id of a committee, or a vector of ids. If `NULL`,
#'   returns records of meetings from all committee. Defaults to `NULL`.
#'
#' @param meet_id The id of a meeting, or a vector of ids. If `NULL`, returns
#'   records of all meetings. Defaults to `NULL`.
#'
#' @param member_id The id of a member, or a vector of ids. Defaults to `NULL`.
#'
#' @param lang The language. `'en'` returns records in English. `'zh'` returns
#'   results in Traditional Chinese. Defaults to `'en'`.
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
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
attendance <- function(committee_id = NULL, meet_id = NULL, member_id = NULL, 
                       lang = "en", attn = "all", from = '1900-01-01', to = Sys.Date(), 
                       n = 1000, extra_param = NULL, verbose = TRUE) {
  if (lang == "en") {
    query <- "TAttendance?$select=committee_id,meet_id,meeting_name_english,meeting_start_date_time,member_id,member_name_english,present_absent"
  } else if (lang == "zh") {
    query <- "TAttendance?$select=committee_id,meet_id,meeting_name_chinese,meeting_start_date_time,member_id,member_name_chinese,present_absent"
  }
  
  filter_args <- {}
  
  if (!is.null(committee_id)) {
    committee_id <- sapply(committee_id, function(X) paste0("\"", x, "\""))
    filter_args <- c(filter_args, generate_filter("committee_id", hansard_id))
  }
  
  if (!is.null(meet_id)) {
    filter_args <- c(filter_args, generate_filter("meet_id", meet_id))
  }
  
  if (!is.null(member_id)) {
    filter_args <- c(filter_args, generate_filter("member_id", member_id))
  }
  
  attn <- tolower(attn)
  if (attn == "p") {
    filter_args <- c(filter_args, "present_absent eq 'P' or present_absent eq 'Present'")
  } else if (attn == "a") {
    filter_args <- c(filter_args, "present_absent eq 'A' or present_absent eq 'Absent'")
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
  
  df <- legco_api("attn", query, n, verbose)
  
  # Rename column names
  colnames(df) <- c("CommitteeID", "MeetID", "MeetingName", "MeetingDateTime", "MemberID", "MemberName", "Attendance")
  
  # Unify format of attendance
  df$Attendance <- sapply(df$Attendance, function (x) {
    ifelse(x == "Absent" | x == "A", "A", "P")
  })
  
  df
  
}

#' @rdname attendance
#' @export
legco_attendance <- attendance