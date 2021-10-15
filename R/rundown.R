#' Rundown from LegCo Hansards
#'
#' Fetch the rundown from hansard files of LegCo council meetings.
#'
#' This function corresponds to the \emph{Rundown} data endpoint of the Hansard
#' Database.
#'
#' @inherit hansard-db
#'
#' @examples
#' \donttest{
#' # Fetch records of proceedings of the second reading of
#' # Smoking (Public Health) (Amendment) Bill 2019
#' x <- rundown(rundown_id = 899628:899649)
#' }
#'
#' @export
#' 
rundown <- function(rundown_id = NULL, hansard_id = NULL, speaker_id = NULL,
                     lang = "en", bookmark_name = NULL, from = '1900-01-01', to = Sys.Date(),
                     floor = FALSE, n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Rundown?$select=RundownID,MeetingDate,Content,BookmarkName,SpeakerID,HansardID,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(rundown_id)) {
    filter_args <- c(filter_args, .generate_filter("RundownID", rundown_id))
  }
  
  if (!is.null(hansard_id)) {
    filter_args <- c(filter_args, .generate_filter("HansardID", hansard_id))
  }

  if (!is.null(speaker_id)) {
    filter_args <- c(filter_args, .generate_filter("SpeakerID", speaker_id))
  }
  
  if (is.null(rundown_id) & is.null(hansard_id)) {
    lang <- tolower(lang)
    if (floor) {
      filter_args <- c(filter_args, "HansardType eq 'Floor'")
    } else if (lang == "en") {
      filter_args <- c(filter_args, "HansardType eq 'English'")
    } else if (lang == "zh") {
      filter_args <- c(filter_args, "HansardType eq 'Chinese'")
    } 
  }
  
  if (!is.null(bookmark_name)) {
    filter_args <- c(filter_args, .generate_filter("BookmarkName", bookmark_name))
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

#' @rdname rundown
#' @export
legco_rundown <- rundown
