#' Rundown from LegCo Hansards
#'
#' Fetch the rundown from hansard files of LegCo council meetings.
#'
#' @param id The id of a rundown, or a vector of ids. Defaults to `NULL`.
#'
#' @param hansard_id The id of a hansard file, or a vector of ids. If `NULL`,
#'   returns rundowns from all hansard files. Defaults to `NULL`.
#'
#' @param speaker_id The id of a speaker, or a vector of ids. Defaults to
#'   `NULL`.
#'
#' @param bookmark_name The bookmark name of the rundown, or a vector of names.
#'   Defaults to `NULL`.
#'
#' @inheritParams legco_api
#' @inheritParams hansard
#'
#' @export
#' 
rundown <- function(id = NULL, hansard_id = NULL, speaker_id = NULL,
                     lang = "en", bookmark_name = NULL, from = '1900-01-01', to = Sys.Date(),
                     floor = FALSE, n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Rundown?$select=RundownID,MeetingDate,Content,BookmarkName,SpeakerID,HansardID,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("RundownID", id))
  }
  
  if (!is.null(hansard_id)) {
    filter_args <- c(filter_args, generate_filter("HansardID", hansard_id))
  }

  if (!is.null(speaker_id)) {
    filter_args <- c(filter_args, generate_filter("SpeakerID", speaker_id))
  }
  
  if (is.null(id) & is.null(hansard_id)) {
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
    filter_args <- c(filter_args, generate_filter("BookmarkName", bookmark_name))
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
