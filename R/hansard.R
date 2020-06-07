#' Hansard of LegCo
#'
#' Fetch metadata and URLs of the hansard files of LegCo council meetings.
#'
#' This function corresponds to the \emph{Hansard} data endpoint of the Hansard
#' Database.
#'
#' @inherit hansard-db
#'
#' @examples
#' \donttest{
#' # Fetch metadata of hansard files of all LegCo meetings
#' # conducted between Feburary 20 to March 20, 2019
#' x <- hansard(from = "2019-02-20", to = "2019-03-20")
#' }
#'
#' @export
#' 
hansard <- function(hansard_id = NULL, lang = "en", from = '1900-01-01', to = Sys.Date(), floor = FALSE, 
                    n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Hansard?$select=HansardID,MeetingDate,AgendaDate,isCEQandA,isCEQT,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(hansard_id)) {
    filter_args <- c(filter_args, .generate_filter("HansardID", hansard_id)) # in utils-misc.R
  }
  
  if (is.null(hansard_id)) {
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

#' @rdname hansard
#' @export
legco_hansard <- hansard
