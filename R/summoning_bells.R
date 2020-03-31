#' Summoning Bells in LegCo
#'
#' Fetch instances of summoning bell being rung in LegCo council meetings
#'
#' This function corresponds to the \emph{SummoningBells} data endpoint of the
#' Hansard Database.
#'
#' @inherit hansard-db
#'
#' @examples
#' \donttest{
#' # Fetch instances of summoning bell during the Council meeting on January 19, 2018
#' x <- summoning_bells(hansard_id = 2714)
#' }
#'
#' @export
#' 
summoning_bells <- function(hansard_id = NULL, rundown_id = NULL, section_code = NULL,
                           lang = "en", from = '1900-01-01', to = Sys.Date(), floor = FALSE,
                           n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "SummoningBells?$select=MeetingDate,SectionCode,RundownID,HansardID,HansardFileURL"
  
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
  
  if (is.null(hansard_id) & is.null(hansard_id)) {
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

#' @rdname summoning_bells
#' @export
legco_summoning_bells <- summoning_bells
