#' Statements by Public Officers
#'
#' Fetch statements made by Public Officers in LegCo council meetings.
#'
#' This function corresponds to the \emph{Statements} data endpoint of the
#' Hansard Database.
#'
#' @inherit hansard-db
#'
#' @examples
#' \donttest{
#' # Fetch statements made by government officials during the Council meeting on April 22, 2015
#' x <- statements(from = "2015-04-22")
#' }
#'
#' @export
#' 
statements <- function(hansard_id = NULL, rundown_id = NULL,
                      lang = "en", from = '1900-01-01', to = Sys.Date(),
                      floor = FALSE, n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Statements?$select=MeetingDate,Subject,Speaker,RundownID,HansardID,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(hansard_id)) {
    filter_args <- c(filter_args, .generate_filter("HansardID", hansard_id))
  }
  
  if (!is.null(rundown_id)) {
    filter_args <- c(filter_args, .generate_filter("RundownID", rundown_id))
  }
  
  lang <- tolower(lang)
  if (floor) {
    filter_args <- c(filter_args, "HansardType eq 'Floor'")
  } else if (lang == "en") {
    filter_args <- c(filter_args, "HansardType eq 'English'")
  } else if (lang == "zh") {
    filter_args <- c(filter_args, "HansardType eq 'Chinese'")
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

#' @rdname statements
#' @export
legco_statements <- statements
