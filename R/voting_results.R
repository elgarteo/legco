#' Voting Results in LegCo Meetings
#'
#' Fetch result of votes made in LegCo council meetings.
#'
#' This function corresponds to the \emph{VotingResults} data endpoint of the
#' Hansard Database.
#'
#' @inherit hansard-db
#'
#' @examples
#' \donttest{
#' # Fetch results of votes conducted during the Council meeting on January 19, 2018
#' x <- voting_results(hansard_id = 2714)
#' }
#'
#' @export
#' 
voting_results <- function(hansard_id = NULL, rundown_id = NULL, section_code = NULL,
                     result = "all", lang = "en", from = '1900-01-01', to = Sys.Date(),
                     floor = FALSE, n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "VotingResults?$select=MeetingDate,Subject,VoteResult,SectionCode,RundownID,HansardID,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(hansard_id)) {
    filter_args <- c(filter_args, .generate_filter("HansardID", hansard_id))
  }
  
  if (!is.null(rundown_id)) {
    filter_args <- c(filter_args, .generate_filter("RundownID", rundown_id))
  }
  
  if (!is.null(section_code)) {
    filter_args <- c(filter_args, .generate_filter("SectionCode", section_code))
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
