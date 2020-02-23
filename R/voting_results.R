#' Voting Results in LegCo Meetings
#'
#' Fetch result of votes made in LegCo council meetings.
#'
#' @param hansard_id The id of a hansard file, or a vector of ids. If `NULL`,
#'   returns voting results from all hansard files. Defaults to `NULL`.
#'
#' @param rundown_id The id of a rundown, or a vector of ids. Defaults to
#'   `NULL`.
#'
#' @param section_code The section code, or a vector of codes. Defaults to
#'   `NULL`.
#'
#' @param result The voting result. `'passed'` returns motions that have been
#'   passed. `'vetoed'` returns motions that have been vetoed. `'all'` returns
#'   all motions that has been voted in LegCo. Defaults to `all`.
#'
#' @inheritParams legco_api
#' @inheritParams hansard
#'
#' @export
#' 
voting_results <- function(hansard_id = NULL, rundown_id = NULL, section_code = NULL,
                     result = "all", lang = "en", from = '1900-01-01', to = Sys.Date(),
                     floor = FALSE, n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "VotingResults?$select=MeetingDate,Subject,VoteResult,SectionCode,RundownID,HansardID,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(hansard_id)) {
    filter_args <- c(filter_args, generate_filter("HansardID", hansard_id))
  }
  
  if (!is.null(rundown_id)) {
    filter_args <- c(filter_args, generate_filter("RundownID", rundown_id))
  }
  
  if (!is.null(section_code)) {
    filter_args <- c(filter_args, generate_filter("SectionCode", section_code))
  }
  
  result <- tolower(result)
  if (result == "passed") {
    filter_args <- c(filter_args, "VoteResults eq 'Passed'")
  } else if (result == "vetoed") {
    filter_args <- c(filter_args, "VoteResults eq 'Negatived'")
  }
  
  if (is.null(hansard_id) & is.null(rundown_id)) {
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

#' @rdname voting_results
#' @export
legco_voting_results <- voting_results
